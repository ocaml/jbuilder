open! Stdune
open Fiber.O
open Dune_rpc_server
open Dune_rpc_private
module Dep_conf = Dune_rules.Dep_conf
module Build_system = Dune_engine.Build_system

module Build_outcome = struct
  type t = Dune_engine.Scheduler.Run.Build_outcome_for_rpc.t =
    | Success
    | Failure

  let sexp = Conv.enum [ ("Success", Success); ("Failure", Failure) ]
end

type pending_build_action =
  | Build of Dep_conf.t list * Build_outcome.t Fiber.Ivar.t

let absolutize_paths ~dir (loc : Loc.t) =
  let make_path name =
    Path.to_absolute_filename
      (if Filename.is_relative name then
        Path.append_local dir (Path.Local.parse_string_exn ~loc name)
      else
        Path.of_string name)
  in
  { Loc.start = { loc.start with pos_fname = make_path loc.start.pos_fname }
  ; stop = { loc.stop with pos_fname = make_path loc.stop.pos_fname }
  }

let diagnostic_of_error : Build_system.Error.t -> Dune_rpc_private.Diagnostic.t
    =
 fun m ->
  let message, related, dir = Build_system.Error.info m in
  let make_loc loc =
    match dir with
    | None -> loc
    | Some dir -> absolutize_paths ~dir loc
  in
  let loc = Option.map message.loc ~f:make_loc in
  let make_message pars = Pp.map_tags (Pp.concat pars) ~f:(fun _ -> ()) in
  let id = Build_system.Error.id m |> Diagnostic.Id.create in
  let promotion =
    match Build_system.Error.promotion m with
    | None -> []
    | Some { in_source; in_build } ->
      [ { Diagnostic.Promotion.in_source =
            Path.to_absolute_filename (Path.source in_source)
        ; in_build = Path.to_absolute_filename (Path.build in_build)
        }
      ]
  in
  let related =
    List.map related ~f:(fun (related : User_message.t) ->
        { Dune_rpc_private.Diagnostic.Related.message =
            make_message related.paragraphs
        ; loc = make_loc (Option.value_exn related.loc)
        })
  in
  { severity = None
  ; id
  ; targets = []
  ; message = make_message message.paragraphs
  ; loc
  ; promotion
  ; related
  ; directory =
      Option.map
        ~f:(fun p ->
          Path.to_absolute_filename
            (Path.drop_optional_build_context_maybe_sandboxed p))
        dir
  }

(* TODO un-copy-paste from dune/bin/arg.ml *)
let dep_parser =
  let open Dune_engine in
  Dune_lang.Syntax.set Stanza.syntax (Active Stanza.latest_version)
    Dep_conf.decode

module Decl = struct
  module Decl = Decl

  let build =
    Decl.request ~method_:"build" Conv.(list string) Build_outcome.sexp

  let shutdown = Decl.notification ~method_:"shutdown" Conv.unit

  module Status = struct
    type t = { clients : Id.t list }

    let sexp =
      let open Conv in
      let to_ clients = { clients } in
      let from { clients } = clients in
      iso (list Id.sexp) to_ from
  end

  let status = Decl.request ~method_:"status" Conv.unit Status.sexp
end

module Client = struct
  type t = { mutable next_id : int }

  let create () = { next_id = 0 }

  let to_dyn { next_id = _ } =
    let open Dyn.Encoder in
    opaque ()
end

module Session_comparable = Comparable.Make (struct
  type t = Client.t Session.t

  let compare = Session.compare

  let to_dyn s = Session.to_dyn Client.to_dyn s
end)

module Session_set = Session_comparable.Set

module Subscribers = struct
  type t =
    { build_progress : Session_set.t
    ; diagnostics : Session_set.t
    }

  let empty =
    { build_progress = Session_set.empty; diagnostics = Session_set.empty }

  let get t (which : Subscribe.t) =
    match which with
    | Build_progress -> t.build_progress
    | Diagnostics -> t.diagnostics

  let modify t (which : Subscribe.t) ~f =
    match which with
    | Build_progress -> { t with build_progress = f t.build_progress }
    | Diagnostics -> { t with diagnostics = f t.diagnostics }

  let add t which session =
    modify t which ~f:(fun set -> Session_set.add set session)

  let remove t which session =
    modify t which ~f:(fun set -> Session_set.remove set session)

  let notify t which ~f =
    let set = get t which in
    Fiber.parallel_iter_set (module Session_set) set ~f
end

(** Primitive unbounded FIFO channel. Reads are blocking. Writes are not
    blocking. At most one read is allowed at a time. *)
module Job_queue : sig
  type 'a t

  (** Remove the element from the internal queue without waiting for the next
      element. *)
  val pop_internal : 'a t -> 'a option

  val create : unit -> 'a t

  val read : 'a t -> 'a Fiber.t

  val write : 'a t -> 'a -> unit Fiber.t
end = struct
  (* invariant: if reader is Some then queue is empty *)
  type 'a t =
    { queue : 'a Queue.t
    ; mutable reader : 'a Fiber.Ivar.t option
    }

  let create () = { queue = Queue.create (); reader = None }

  let pop_internal t = Queue.pop t.queue

  let read t =
    Fiber.of_thunk (fun () ->
        match t.reader with
        | Some _ ->
          Code_error.raise "multiple concurrent reads of build job queue" []
        | None -> (
          match Queue.pop t.queue with
          | None ->
            let ivar = Fiber.Ivar.create () in
            t.reader <- Some ivar;
            Fiber.Ivar.read ivar
          | Some v -> Fiber.return v))

  let write t elem =
    Fiber.of_thunk (fun () ->
        match t.reader with
        | Some ivar ->
          t.reader <- None;
          Fiber.Ivar.fill ivar elem
        | None ->
          Queue.push t.queue elem;
          Fiber.return ())
end

type t =
  { config : Run.Config.t
  ; pending_build_jobs :
      (Dep_conf.t list * Build_outcome.t Fiber.Ivar.t) Job_queue.t
  ; build_handler : Build_system.Handler.t
  ; pool : Fiber.Pool.t
  ; mutable subscribers : Subscribers.t
  ; mutable clients : Session_set.t
  }

let build_handler t = t.build_handler

let handler (t : t Fdecl.t) : 'a Dune_rpc_server.Handler.t =
  let on_init session (_ : Initialize.Request.t) =
    let t = Fdecl.get t in
    let client = Client.create () in
    t.clients <- Session_set.add t.clients session;
    Fiber.return client
  in
  let on_terminate session =
    let t = Fdecl.get t in
    t.subscribers <-
      [ Subscribe.Diagnostics; Build_progress ]
      |> List.fold_left ~init:t.subscribers ~f:(fun acc which ->
             Subscribers.remove acc which session);
    t.clients <- Session_set.remove t.clients session;
    Fiber.return ()
  in
  let rpc =
    Handler.create ~on_terminate ~on_init
      ~version:Dune_rpc_private.Version.latest ()
  in
  let () =
    Handler.request rpc
      (Handler.callback (Handler.public ~since:(1, 0) ()) Fiber.return)
      Public.Request.ping
  in
  let () =
    let build targets =
      let ivar = Fiber.Ivar.create () in
      let targets =
        List.map targets ~f:(fun s ->
            Dune_lang.Decoder.parse dep_parser
              (Univ_map.set Univ_map.empty
                 Dune_engine.String_with_vars.decoding_env_key
                 (* CR-someday aalekseyev: hardcoding the version here is not
                    ideal, but it will do for now since this command is not
                    stable and we're only using it in tests. *)
                 (Dune_engine.Pform.Env.initial (3, 0)))
              (Dune_lang.Parser.parse_string ~fname:"dune rpc"
                 ~mode:Dune_lang.Parser.Mode.Single s))
      in
      let* () =
        Job_queue.write (Fdecl.get t).pending_build_jobs (targets, ivar)
      in
      Fiber.Ivar.read ivar
    in
    Handler.request rpc (Handler.callback Handler.private_ build) Decl.build
  in
  let () =
    let rec cancel_pending_jobs () =
      match Job_queue.pop_internal (Fdecl.get t).pending_build_jobs with
      | None -> Fiber.return ()
      | Some (_, job) ->
        let* () = Fiber.Ivar.fill job Build_outcome.Failure in
        cancel_pending_jobs ()
    in
    let shutdown () =
      let t = Fdecl.get t in
      let terminate_sessions () =
        Fiber.fork_and_join_unit cancel_pending_jobs (fun () ->
            Fiber.parallel_iter_set
              (module Session_set)
              t.clients ~f:Session.request_close)
      in
      let shutdown () =
        Fiber.fork_and_join_unit Dune_engine.Scheduler.shutdown Run.stop
      in
      Fiber.fork_and_join_unit terminate_sessions shutdown
    in
    Handler.notification rpc
      (Handler.callback Handler.private_ shutdown)
      Decl.shutdown
  in
  let () =
    let unsubscribe session (sub : Subscribe.t) =
      let t = Fdecl.get t in
      t.subscribers <- Subscribers.remove t.subscribers sub session;
      Fiber.return ()
    in
    let cb = Handler.callback' (Handler.public ~since:(1, 0) ()) unsubscribe in
    Handler.notification rpc cb Public.Notification.unsubscribe
  in
  let () =
    let subscribe session (sub : Subscribe.t) =
      let t = Fdecl.get t in
      t.subscribers <- Subscribers.add t.subscribers sub session;
      let* running = Fiber.Pool.running t.pool in
      match running with
      | false -> Fiber.return ()
      | true ->
        let errors = Build_system.errors () in
        Fiber.Pool.task t.pool ~f:(fun () ->
            let events =
              List.map errors ~f:(fun (e : Build_system.Error.t) ->
                  Diagnostic.Event.Add (diagnostic_of_error e))
            in
            Session.notification session Server_notifications.diagnostic events)
    in
    let cb = Handler.callback' (Handler.public ~since:(1, 0) ()) subscribe in
    Handler.notification rpc cb Public.Notification.subscribe
  in
  let () =
    let f () =
      let t = Fdecl.get t in
      let clients =
        Session_set.to_list_map t.clients ~f:(fun session ->
            Session.initialize session |> Initialize.Request.id)
      in
      Fiber.return { Decl.Status.clients }
    in
    let cb = Handler.callback Handler.private_ f in
    Handler.request rpc cb Decl.status
  in
  let () =
    let cb =
      let f () =
        Build_system.errors ()
        |> List.map ~f:diagnostic_of_error
        |> Fiber.return
      in
      Handler.callback (Handler.public ~since:(1, 0) ()) f
    in
    Handler.request rpc cb Public.Request.diagnostics
  in
  rpc

let task t f =
  let* running = Fiber.Pool.running t.pool in
  if running then
    Fiber.Pool.task t.pool ~f
  else
    Fiber.return ()

let error t errors =
  let t = Fdecl.get t in
  task t (fun () ->
      let events =
        lazy
          (List.map errors ~f:(fun (e : Build_system.Handler.error) ->
               match e with
               | Add x -> Diagnostic.Event.Add (diagnostic_of_error x)
               | Remove x -> Remove (diagnostic_of_error x)))
      in
      Subscribers.notify t.subscribers Diagnostics ~f:(fun session ->
          Session.notification session Server_notifications.diagnostic
            (Lazy.force events)))

let build_progress t ~complete ~remaining =
  let t = Fdecl.get t in
  let notification = Progress.In_progress { complete; remaining } in
  task t (fun () ->
      Subscribers.notify t.subscribers Build_progress ~f:(fun session ->
          Session.notification session Server_notifications.progress
            notification))

let build_event t (event : Build_system.Handler.event) =
  let t = Fdecl.get t in
  let notification =
    match event with
    | Start -> Progress.In_progress { complete = 0; remaining = 0 }
    | Finish -> Success
    | Interrupt -> Interrupted
    | Fail -> Failed
  in
  task t (fun () ->
      Subscribers.notify t.subscribers Build_progress ~f:(fun session ->
          Session.notification session Server_notifications.progress
            notification))

let create () =
  let t = Fdecl.create Dyn.Encoder.opaque in
  let pending_build_jobs = Job_queue.create () in
  let handler = Dune_rpc_server.make (handler t) in
  let pool = Fiber.Pool.create () in
  let config = Run.Config.Server { handler; backlog = 10; pool } in
  let build_handler =
    Build_system.Handler.create ~error:(error t)
      ~build_progress:(build_progress t) ~build_event:(build_event t)
  in
  let res =
    { config
    ; pending_build_jobs
    ; subscribers = Subscribers.empty
    ; clients = Session_set.empty
    ; build_handler
    ; pool
    }
  in
  Fdecl.set t res;
  res

let config t = t.config

let pending_build_action t =
  Job_queue.read t.pending_build_jobs
  |> Fiber.map ~f:(fun (targets, ivar) -> Build (targets, ivar))
