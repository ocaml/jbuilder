open! Stdune
open Fiber.O
module Function = Function

module Build = struct
  include Fiber

  let run = Fun.id

  let of_fiber = Fun.id
end

let unwrap_exn = ref Fun.id

module Code_error_with_memo_backtrace = struct
  (* A single memo frame and the OCaml frames it called which lead to the error *)
  type frame =
    { ocaml : string
    ; memo : Dyn.t
    }

  type t =
    { exn : Code_error.t
    ; reverse_backtrace : frame list
    ; (* [outer_call_stack] is a trick to capture some of the information that's
         lost by the async memo error handler. It can be safely ignored by the
         sync error handler. *)
      outer_call_stack : Dyn.t
    }

  type exn += E of t

  let frame_to_dyn { ocaml; memo } =
    Dyn.Record [ ("ocaml", Dyn.String ocaml); ("memo", memo) ]

  let to_dyn { exn; reverse_backtrace; outer_call_stack } =
    Dyn.Record
      [ ("exn", Code_error.to_dyn exn)
      ; ("backtrace", Dyn.Encoder.list frame_to_dyn (List.rev reverse_backtrace))
      ; ("outer_call_stack", outer_call_stack)
      ]

  let () =
    Printexc.register_printer (function
      | E t -> Some (Dyn.to_string (to_dyn t))
      | _ -> None)
end

module Allow_cutoff = struct
  type 'o t =
    | No
    | Yes of ('o -> 'o -> bool)
end

module type Output_simple = sig
  type t

  val to_dyn : t -> Dyn.t
end

module type Output_allow_cutoff = sig
  type t

  val to_dyn : t -> Dyn.t

  val equal : t -> t -> bool
end

module type Input = sig
  type t

  include Table.Key with type t := t
end

module Output = struct
  type 'o t =
    | Simple of (module Output_simple with type t = 'o)
    | Allow_cutoff of (module Output_allow_cutoff with type t = 'o)
end

module Visibility = struct
  type 'i t =
    | Hidden
    | Public of 'i Dune_lang.Decoder.t
end

module Exn_comparable = Comparable.Make (struct
  type t = Exn_with_backtrace.t

  let compare { Exn_with_backtrace.exn; backtrace = _ } (t : t) =
    Poly.compare (!unwrap_exn exn) (!unwrap_exn t.exn)

  let to_dyn = Exn_with_backtrace.to_dyn
end)

module Exn_set = Exn_comparable.Set

module Spec = struct
  type ('a, 'b, 'f) t =
    { info : Function.Info.t option
    ; input : (module Store_intf.Input with type t = 'a)
    ; output : (module Output_simple with type t = 'b)
    ; allow_cutoff : 'b Allow_cutoff.t
    ; decode : 'a Dune_lang.Decoder.t
    ; witness : 'a Type_eq.Id.t
    ; f : ('a, 'b, 'f) Function.t
    }

  type packed = T : (_, _, _) t -> packed [@@unboxed]

  (* This mutable table is safe under the assumption that [register] is called
     only at the top level, which is currently true. This means that all
     memoization tables created not at the top level are hidden. *)
  let by_name : packed String.Table.t = String.Table.create 256

  let find name = String.Table.find by_name name

  let register t =
    match t.info with
    | None -> Code_error.raise "[Spec.register] got a function with no info" []
    | Some info -> (
      match find info.name with
      | Some _ ->
        Code_error.raise
          "[Spec.register] called twice on a function with the same name"
          [ ("name", Dyn.String info.name) ]
      | None -> String.Table.set by_name info.name (T t))

  let create (type o) ~info ~input ~visibility ~(output : o Output.t) ~f =
    let (output : (module Output_simple with type t = o)), allow_cutoff =
      match output with
      | Simple (module Output) -> ((module Output), Allow_cutoff.No)
      | Allow_cutoff (module Output) -> ((module Output), Yes Output.equal)
    in
    let decode =
      match visibility with
      | Visibility.Public decode -> decode
      | Hidden ->
        let open Dune_lang.Decoder in
        let+ loc = loc in
        User_error.raise ~loc [ Pp.text "<not-implemented>" ]
    in
    { info
    ; input
    ; output
    ; allow_cutoff
    ; decode
    ; witness = Type_eq.Id.create ()
    ; f
    }
end

module Id = Id.Make ()

(* We can get rid of this once we use the memoization system more pervasively
   and all the dependencies are properly specified *)
module Caches = struct
  let cleaners = ref []

  let register ~clear = cleaners := clear :: !cleaners

  let clear () = List.iter !cleaners ~f:(fun f -> f ())
end

(* A value calculated during a sample attempt, or an exception with a backtrace
   if the attempt failed. *)
module Value = struct
  type error =
    | Sync of Exn_with_backtrace.t
    | Async of Exn_set.t

  type 'a t = ('a, error) Result.t

  let get_sync_exn = function
    | Ok a -> a
    | Error (Sync exn) -> Exn_with_backtrace.reraise exn
    | Error (Async _) ->
      (* It's not possible for a sync computation to raise more than one error *)
      assert false

  let get_async_exn = function
    | Ok a -> Fiber.return a
    | Error (Sync _) -> assert false
    | Error (Async exns) -> Fiber.reraise_all (Exn_set.to_list exns)
end

module Dep_node_without_state = struct
  type ('a, 'b, 'f) t =
    { spec : ('a, 'b, 'f) Spec.t
    ; input : 'a
    ; id : Id.t
    }

  type packed = T : (_, _, _) t -> packed [@@unboxed]
end

module Dag : Dag.S with type value := Dep_node_without_state.packed =
Dag.Make (struct
  type t = Dep_node_without_state.packed
end)

(** [Value_id] is an identifier allocated every time a node value is computed
    and found to be different from before.

    The clients then use [Value_id] to see if the value have changed since the
    previous value they observed. This means we don't need to run the cutoff
    comparison at every client.

    There is a downside, though: if the value changes from x to y, and then back
    to x, then the Value_id changes without the value actually changing, which
    is a shame. So we should test and see if Value_id is worth keeping or it's
    better to just evaluate the cutoff multiple times. *)
module Value_id : sig
  type t

  val create : unit -> t

  val equal : t -> t -> bool

  val to_dyn : t -> Dyn.t
end = struct
  type t = int

  let to_dyn = Int.to_dyn

  let next = ref 0

  let create () =
    let res = !next in
    next := res + 1;
    res

  let equal = Int.equal
end

let _ = Value_id.to_dyn

module M = struct
  module rec Completion : sig
    type ('a, 'b, 'f) t =
      | Sync : ('a, 'b, 'a -> 'b) t
      | Async : 'b Cached_value.t Fiber.Ivar.t -> ('a, 'b, 'a -> 'b Fiber.t) t
  end =
    Completion

  and Cached_value : sig
    type 'a t =
      { value : 'a Value.t
      ; (* The value id, used to check that the value is the same. *)
        id : Value_id.t
      ; (* When was last computed or confirmed unchanged *)
        mutable last_validated_at : Run.t
      ; (* The values stored in [deps] must have been calculated at
           [last_validated_at] too.

           In fact the set of deps can change over the lifetime of
           [Cached_value] even if the [value] and [id] do not change. This can
           happen if the value gets re-computed, but a cutoff prevents us from
           updating the value id.

           Note that [deps] should be listed in the order in which they were
           depended on to avoid recomputations of the dependencies that are no
           longer relevant (see an example below). [Async] functions induce a
           partial (rather than a total) order on dependencies, and so [deps]
           should be a linearisation of this partial order. It is also worth
           noting that the problem only occurs with dynamic dependencies,
           because static dependencies can never become irrelevant.

           As an example, consider the function [let f x = let y = g x in h y].
           The correct order of dependencies of [f 0] is [g 0] and then [h y1],
           where [y1] is the result of computing [g 0] in the first build run.
           Now consider the situation where (i) [h y1] is incorrectly listed
           first in [deps], and (ii) both [g] and [h] have changed in the second
           build run (e.g. because they read modified files). To determine that
           [f] needs to be recomputed, we start by recomputing [h y1], which is
           likely to be a waste because now we are really interested in [h y2],
           where [y2] is the result of computing [g 0] in the second run. Had we
           listed [g 0] first, we would recompute it and the work wouldn't be
           wasted since [f 0] does depend on it.

           aalekseyev: now we have a more stringent requirement: [deps] must be
           a linearization of dependency causality order, otherwise the
           validation algorithm may create spurious dependency cycles. *)
        mutable deps : Last_dep.t list
      }
  end =
    Cached_value

  and Deps_so_far : sig
    type t =
      { set : Id.Set.t
      ; deps_reversed : Dep_node.packed list
      }
  end =
    Deps_so_far

  and Running_state : sig
    type t =
      { mutable deps_so_far : Deps_so_far.t
      ; sample_attempt : Dag.node
      }
  end =
    Running_state

  and State : sig
    type ('a, 'b, 'f) t =
      (* [Considering] marks computations currently being considered (either
         validated, or run). Note, however, that a stale value of [Considering]
         may remain if evaluation is cancelled. Only [Considering] constructors
         with the value of [run] equal to [Run.current ()] should be taken into
         account. *)
      | Not_considering
      | Considering of
          { run : Run.t
          ; running : Running_state.t
          ; completion : ('a, 'b, 'f) Completion.t
          }
  end =
    State

  and Dep_node : sig
    type ('a, 'b, 'f) t =
      { without_state : ('a, 'b, 'f) Dep_node_without_state.t
      ; mutable state : ('a, 'b, 'f) State.t
      ; mutable last_cached_value : 'b Cached_value.t option
      }

    type packed = T : (_, _, _) t -> packed [@@unboxed]
  end =
    Dep_node

  (* We store the [Value_id] of the last ['b Cache_value.t] value we depended on
     to support early cutoff. When [Value_id <> Dep_node.last_cached_value.id],
     the early cutoff fails and the holder of the corresponding [Last_dep] needs
     to be recomputed. *)
  and Last_dep : sig
    type t = T : ('a, 'b, 'f) Dep_node.t * Value_id.t -> t
  end =
    Last_dep
end

module State = M.State
module Running_state = M.Running_state
module Dep_node = M.Dep_node
module Last_dep = M.Last_dep
module Deps_so_far = M.Deps_so_far

let no_deps_so_far : Deps_so_far.t = { set = Id.Set.empty; deps_reversed = [] }

let currently_considering (v : _ State.t) : _ State.t =
  match v with
  | Not_considering -> Not_considering
  | Considering { run; _ } as running ->
    if Run.is_current run then
      running
    else
      Not_considering

let get_cached_value_in_current_cycle (dep_node : _ Dep_node.t) =
  match dep_node.last_cached_value with
  | None -> None
  | Some cv ->
    if Run.is_current cv.last_validated_at then
      Some cv
    else
      None

module Cached_value = struct
  include M.Cached_value

  let capture_dep_values ~deps_rev =
    List.rev_map deps_rev ~f:(function Dep_node.T dep_node ->
        (match get_cached_value_in_current_cycle dep_node with
        | None ->
          Code_error.raise
            "Attempted to create a cached value based on some stale inputs" []
        | Some cv -> Last_dep.T (dep_node, cv.id)))

  let create x ~deps_rev =
    { deps = capture_dep_values ~deps_rev
    ; value = x
    ; last_validated_at = Run.current ()
    ; id = Value_id.create ()
    }

  let confirm_old_value t ~deps_rev =
    t.last_validated_at <- Run.current ();
    t.deps <- capture_dep_values ~deps_rev;
    t

  let value_changed (type a) (node : (_, a, _) Dep_node.t) prev_output
      curr_output =
    match (prev_output, curr_output) with
    | Error _, _ -> true
    | _, Error _ -> true
    | Ok prev_output, Ok curr_output -> (
      match node.without_state.spec.allow_cutoff with
      | Yes equal -> not (equal prev_output curr_output)
      | No -> true)
end

let ser_input (type a) (node : (a, _, _) Dep_node_without_state.t) =
  let (module Input : Store_intf.Input with type t = a) = node.spec.input in
  Input.to_dyn node.input

module Stack_frame_without_state = struct
  open Dep_node_without_state

  type t = Dep_node_without_state.packed

  let name (T t) = Option.map t.spec.info ~f:(fun x -> x.name)

  let input (T t) = ser_input t

  let to_dyn t =
    Dyn.Tuple
      [ String
          (match name t with
          | Some name -> name
          | None -> "<unnamed>")
      ; input t
      ]
end

module Stack_frame_with_state = struct
  type ('i, 'o, 'f) unpacked =
    { without_state : ('i, 'o, 'f) Dep_node_without_state.t
    ; running_state : Running_state.t
    }

  type t = T : ('i, 'o, 'f) unpacked -> t

  let to_dyn (T t) = Stack_frame_without_state.to_dyn (T t.without_state)
end

module To_open = struct
  module Stack_frame = Stack_frame_with_state
end

open To_open

module Cycle_error = struct
  type t =
    { cycle : Stack_frame_without_state.t list
    ; stack : Stack_frame_without_state.t list
    }

  exception E of t

  let get t = t.cycle

  let stack t = t.stack
end

let global_dep_dag = Dag.create ()

module Call_stack = struct
  (* fiber context variable keys *)
  let call_stack_key = Fiber.Var.create ()

  let get_call_stack () =
    Fiber.Var.get call_stack_key |> Option.value ~default:[]

  let get_call_stack_without_state () =
    get_call_stack ()
    |> List.map ~f:(fun (Stack_frame_with_state.T t) ->
           Dep_node_without_state.T t.without_state)

  let get_call_stack_as_dyn () =
    Dyn.Encoder.list Stack_frame.to_dyn (get_call_stack ())

  let get_call_stack_tip () = List.hd_opt (get_call_stack ())

  let push_async_frame (frame : Stack_frame_with_state.t) f =
    let stack = get_call_stack () in
    Fiber.Var.set call_stack_key (frame :: stack) (fun () ->
        Implicit_output.forbid_async f)

  let push_sync_frame (frame : Stack_frame_with_state.t) f =
    let stack = get_call_stack () in
    Fiber.Var.set_sync call_stack_key (frame :: stack) (fun () ->
        Implicit_output.forbid_sync f)
end

let pp_stack () =
  let open Pp.O in
  let stack = Call_stack.get_call_stack () in
  Pp.vbox
    (Pp.box (Pp.text "Memoized function stack:")
    ++ Pp.cut
    ++ Pp.chain stack ~f:(fun frame -> Dyn.pp (Stack_frame.to_dyn frame)))

let dump_stack () = Format.eprintf "%a" Pp.to_fmt (pp_stack ())

(** Describes the state of a given sample attempt. The sample attempt starts out
    in [Running] state, accumulates dependencies over time and then transitions
    to [Finished].

    We maintain a DAG of running attempts for cycle detection, but finished ones
    don't need to be in the DAG (although currently they still are, until a run
    is complete and we throw away the entire DAG). *)
module Sample_attempt_dag_node = struct
  type t =
    | Running of Dag.node
    | Finished
end

(* Add a dependency on the [node] from the caller, if there is one. *)
let add_dep_from_caller (type i o f) ~called_from_peek
    (node : (i, o, f) Dep_node.t)
    (sample_attempt_dag_node : Sample_attempt_dag_node.t) =
  match Call_stack.get_call_stack_tip () with
  | None -> ()
  | Some (Stack_frame_with_state.T caller) -> (
    let running_state_of_caller = caller.running_state in
    let () =
      match (caller.without_state.spec.f, node.without_state.spec.f) with
      | Async _, Async _ -> ()
      | Async _, Sync _ -> ()
      | Sync _, Sync _ -> ()
      | Sync _, Async _ ->
        if not called_from_peek then
          Code_error.raise
            "[Memo.add_dep_from_caller ~called_from_peek:false] Synchronous \
             functions are not allowed to depend on asynchronous ones."
            [ ("stack", Call_stack.get_call_stack_as_dyn ())
            ; ("adding", Stack_frame_without_state.to_dyn (T node.without_state))
            ]
    in
    match
      Id.Set.mem running_state_of_caller.deps_so_far.set node.without_state.id
    with
    | true -> ()
    | false ->
      let () =
        match sample_attempt_dag_node with
        | Finished -> ()
        | Running node -> (
          try
            Dag.add_assuming_missing global_dep_dag
              running_state_of_caller.sample_attempt node
          with
          | Dag.Cycle cycle ->
            raise
              (Cycle_error.E
                 { stack = Call_stack.get_call_stack_without_state ()
                 ; cycle = List.map cycle ~f:(fun node -> node.Dag.data)
                 }))
      in
      running_state_of_caller.deps_so_far <-
        { set =
            Id.Set.add running_state_of_caller.deps_so_far.set
              node.without_state.id
        ; deps_reversed =
            Dep_node.T node :: running_state_of_caller.deps_so_far.deps_reversed
        })

type ('input, 'output, 'f) t =
  { spec : ('input, 'output, 'f) Spec.t
  ; cache : ('input, ('input, 'output, 'f) Dep_node.t) Store.t
  }

module Stack_frame = struct
  type ('input, 'output, 'f) memo = ('input, 'output, 'f) t

  include Stack_frame_without_state

  let as_instance_of (type i) (Dep_node_without_state.T t)
      ~of_:(memo : (i, _, _) memo) : i option =
    match Type_eq.Id.same memo.spec.witness t.spec.witness with
    | Some Type_eq.T -> Some t.input
    | None -> None
end

let handle_code_error ~frame (exn : Exn_with_backtrace.t) : Exn_with_backtrace.t
    =
  let code_error (e : Code_error_with_memo_backtrace.t) =
    let bt = exn.backtrace in
    let { Code_error_with_memo_backtrace.exn
        ; reverse_backtrace
        ; outer_call_stack = _
        } =
      e
    in
    Code_error_with_memo_backtrace.E
      { exn
      ; reverse_backtrace =
          { ocaml = Printexc.raw_backtrace_to_string bt
          ; memo = Stack_frame.to_dyn frame
          }
          :: reverse_backtrace
      ; outer_call_stack = Call_stack.get_call_stack_as_dyn ()
      }
  in
  Exn_with_backtrace.map exn ~f:(fun exn ->
      match exn with
      | Code_error.E exn ->
        code_error
          { Code_error_with_memo_backtrace.exn
          ; reverse_backtrace = []
          ; outer_call_stack = Dyn.String "<n/a>"
          }
      | Code_error_with_memo_backtrace.E e -> code_error e
      | another_exn -> another_exn)

let create_with_cache (type i o f) name ~cache ?doc ~input ~visibility ~output
    (typ : (i, o, f) Function.Type.t) (f : f) =
  let f = Function.of_type typ f in
  let spec =
    Spec.create ~info:(Some { name; doc }) ~input ~output ~visibility ~f
  in
  (match visibility with
  | Public _ -> Spec.register spec
  | Hidden -> ());
  Caches.register ~clear:(fun () -> Store.clear cache);
  { cache; spec }

let create_with_store (type i) name
    ~store:(module S : Store_intf.S with type key = i) ?doc ~input ~visibility
    ~output typ f =
  let cache = Store.make (module S) in
  create_with_cache name ~cache ?doc ~input ~output ~visibility typ f

let create (type i) name ?doc ~input:(module Input : Input with type t = i)
    ~visibility ~output typ f =
  (* This mutable table is safe: the implementation tracks all dependencies. *)
  let cache = Store.of_table (Table.create (module Input) 16) in
  let input = (module Input : Store_intf.Input with type t = i) in
  create_with_cache name ~cache ?doc ~input ~visibility ~output typ f

let create_hidden (type output) name ?doc ~input typ impl =
  let module O = struct
    type t = output

    let to_dyn (_ : t) = Dyn.Opaque
  end in
  create
    ~output:(Simple (module O))
    ~visibility:Hidden name ?doc ~input typ impl

module Exec = struct
  let make_dep_node ~spec ~state ~last_cached_value ~input : _ Dep_node.t =
    let dep_node_without_state : _ Dep_node_without_state.t =
      { id = Id.gen (); input; spec }
    in
    { without_state = dep_node_without_state; last_cached_value; state }
end

let dep_node (type i o f) (t : (i, o, f) t) inp =
  match Store.find t.cache inp with
  | Some dep_node -> dep_node
  | None ->
    let dep_node =
      Exec.make_dep_node ~spec:t.spec ~input:inp ~state:Not_considering
        ~last_cached_value:None
    in
    Store.set t.cache inp dep_node;
    dep_node

module Prev_cycle_cache_lookup_result = struct
  type 'a t =
    | Valid of 'a Cached_value.t
    | Invalid of { old_value : 'a Cached_value.t option }
end

module Changed_or_not = struct
  type t =
    | Changed
    | Unchanged
end

(* CR-someday aalekseyev: There's a lot of duplication between Exec_sync and
   Exec_async. We should reduce the duplication, but there are ideas in the air
   of getting rid of "Sync" memoization entirely, so I'm not investing effort
   into that. *)
module rec Exec_sync : sig
  val exec_dep_node : ('a, 'b, 'a -> 'b) Dep_node.t -> 'b

  val exec : ('a, 'b, 'a -> 'b) t -> 'a -> 'b

  val exec_dep_node_internal :
    ('a, 'b, 'a -> 'b) Dep_node.t -> 'b Cached_value.t
end = struct
  module Start_considering_result = struct
    type 'a t =
      | Done of 'a Cached_value.t
      | Needs_work of
          { sample_attempt : Dag.node
                (** [work] must be called to force the value to be computed. *)
          ; work : unit -> 'a Cached_value.t
          }

    let sample_attempt_dag_node t : Sample_attempt_dag_node.t =
      match t with
      | Done _ -> Finished
      | Needs_work { sample_attempt; _ } -> Running sample_attempt
  end

  let deps_changed =
    let rec go deps =
      match deps with
      | [] -> Changed_or_not.Unchanged
      | Last_dep.T (dep, v_id) :: deps -> (
        let res = Exec_unknown.exec_dep_node_internal_from_sync dep in
        match Value_id.equal res.id v_id with
        | true -> go deps
        | false -> Changed_or_not.Changed)
    in
    go

  let do_validate (dep_node : _ Dep_node.t) inp running_state =
    let res =
      let frame =
        (T { without_state = dep_node.without_state; running_state }
          : Stack_frame_with_state.t)
      in
      Call_stack.push_sync_frame frame (fun () ->
          let from_cache =
            match dep_node.last_cached_value with
            | None ->
              Prev_cycle_cache_lookup_result.Invalid { old_value = None }
            | Some cv -> (
              let res = deps_changed cv.deps in
              match res with
              | Unchanged -> Prev_cycle_cache_lookup_result.Valid cv
              | Changed ->
                dep_node.last_cached_value <- None;
                Invalid { old_value = Some cv })
          in
          match from_cache with
          | Valid v -> v
          | Invalid { old_value } -> (
            match dep_node.without_state.spec.f with
            | Function.Sync f ->
              let res = Exn_with_backtrace.try_with (fun () -> f inp) in
              let res =
                Result.map_error res ~f:(fun exn ->
                    let exn =
                      handle_code_error
                        ~frame:(Dep_node_without_state.T dep_node.without_state)
                        exn
                    in
                    Value.Sync exn)
              in
              let deps_rev = running_state.deps_so_far.deps_reversed in
              let res =
                match old_value with
                | None -> Cached_value.create res ~deps_rev
                | Some old_cv -> (
                  match
                    Cached_value.value_changed dep_node old_cv.value res
                  with
                  | false -> Cached_value.confirm_old_value ~deps_rev old_cv
                  | true -> Cached_value.create res ~deps_rev)
              in
              dep_node.last_cached_value <- Some res;
              res))
    in
    dep_node.state <- Not_considering;
    res

  let newly_considering (dep_node : _ Dep_node.t) inp =
    let sample_attempt : Dag.node =
      { info = Dag.create_node_info global_dep_dag
      ; data = Dep_node_without_state.T dep_node.without_state
      }
    in
    let running_state : Running_state.t =
      { sample_attempt; deps_so_far = no_deps_so_far }
    in
    dep_node.state <-
      Considering
        { run = Run.current (); running = running_state; completion = Sync };
    Start_considering_result.Needs_work
      { sample_attempt
      ; work = (fun () -> do_validate dep_node inp running_state)
      }

  let start_considering_dep_node (dep_node : ('a, 'b, 'a -> 'b) Dep_node.t) =
    match currently_considering dep_node.state with
    | Not_considering -> (
      match get_cached_value_in_current_cycle dep_node with
      | None -> newly_considering dep_node dep_node.without_state.input
      | Some cv -> Done cv)
    | Considering
        { running = { sample_attempt; deps_so_far = _ }; completion = Sync; _ }
      ->
      Needs_work
        { sample_attempt
        ; work =
            (fun () ->
              (* dependency cycle, should be caught by [add_dep_from_caller] *)
              assert false)
        }

  let consider_dep_node (dep_node : _ Dep_node.t) =
    let pre_res = start_considering_dep_node dep_node in
    add_dep_from_caller ~called_from_peek:false dep_node
      (Start_considering_result.sample_attempt_dag_node pre_res);
    match pre_res with
    | Done v -> v
    | Needs_work { sample_attempt = _; work } -> work ()

  let exec_dep_node dep_node =
    let res = consider_dep_node dep_node in
    Value.get_sync_exn res.value

  let exec_dep_node_internal = consider_dep_node

  let exec t inp = exec_dep_node (dep_node t inp)
end

and Exec_async : sig
  (** Two kinds of recursive calls: *)

  (** [exec_dep_node_internal]: called when we're validating nodes and checking
      whether or not the user callback is worth running *)
  val exec_dep_node_internal :
    ('a, 'b, 'a -> 'b Fiber.t) Dep_node.t -> 'b Cached_value.t Fiber.t

  (** [exec] and variants thereof *)
  val exec : ('a, 'b, 'a -> 'b Fiber.t) t -> 'a -> 'b Fiber.t

  val exec_dep_node : ('a, 'b, 'a -> 'b Fiber.t) Dep_node.t -> 'b Fiber.t
end = struct
  module Start_considering_result = struct
    type 'a t =
      | Done of 'a Cached_value.t
      | Needs_work of
          { sample_attempt : Dag.node
                (** [work] must be called to force the value to be computed. *)
          ; work : 'a Cached_value.t Fiber.t
          }

    let sample_attempt_dag_node t : Sample_attempt_dag_node.t =
      match t with
      | Done _ -> Finished
      | Needs_work { sample_attempt; _ } -> Running sample_attempt
  end

  let deps_changed =
    let rec go deps =
      match deps with
      | [] -> Fiber.return Changed_or_not.Unchanged
      | Last_dep.T (dep, v_id) :: deps -> (
        let* res = Exec_unknown.exec_dep_node_internal dep in
        match Value_id.equal res.id v_id with
        | true -> go deps
        | false -> Fiber.return Changed_or_not.Changed)
    in
    go

  let do_validate (dep_node : _ Dep_node.t) inp ivar running_state =
    let* res =
      Call_stack.push_async_frame
        (T { without_state = dep_node.without_state; running_state })
        (fun () ->
          let* from_cache =
            match dep_node.last_cached_value with
            | None ->
              Fiber.return
                (Prev_cycle_cache_lookup_result.Invalid { old_value = None })
            | Some cv -> (
              let+ res = deps_changed cv.deps in
              match res with
              | Unchanged ->
                cv.last_validated_at <- Run.current ();
                Prev_cycle_cache_lookup_result.Valid cv
              | Changed ->
                dep_node.last_cached_value <- None;
                Invalid { old_value = Some cv })
          in
          match from_cache with
          | Valid v -> Fiber.return v
          | Invalid { old_value } -> (
            match dep_node.without_state.spec.f with
            | Function.Async f ->
              (* A consequence of using [Fiber.collect_errors] is that memoized
                 functions don't report errors promptly - errors are reported
                 once all child fibers terminate. To fix this, we should use
                 [Fiber.with_error_handler], but we don't have access to dune's
                 error reporting mechanism in memo *)
              let+ res = Fiber.collect_errors (fun () -> f inp) in
              let res =
                Result.map_error res ~f:(fun exns ->
                    (* this step deduplicates the errors *)
                    Value.Async (Exn_set.of_list exns))
              in
              (* update the output cache with the correct value *)
              let deps_rev = running_state.deps_so_far.deps_reversed in
              let res =
                match old_value with
                | None -> Cached_value.create res ~deps_rev
                | Some old_cv -> (
                  match
                    Cached_value.value_changed dep_node old_cv.value res
                  with
                  | true -> Cached_value.create res ~deps_rev
                  | false -> Cached_value.confirm_old_value ~deps_rev old_cv)
              in
              dep_node.last_cached_value <- Some res;
              res))
    in
    dep_node.state <- Not_considering;
    let+ () = Fiber.Ivar.fill ivar res in
    res

  let newly_considering (dep_node : _ Dep_node.t) inp =
    let sample_attempt : Dag.node =
      { info = Dag.create_node_info global_dep_dag
      ; data = Dep_node_without_state.T dep_node.without_state
      }
    in
    let ivar = Fiber.Ivar.create () in
    let running_state : Running_state.t =
      { sample_attempt; deps_so_far = no_deps_so_far }
    in
    dep_node.state <-
      Considering
        { run = Run.current ()
        ; running = running_state
        ; completion = Async ivar
        };
    Start_considering_result.Needs_work
      { sample_attempt; work = do_validate dep_node inp ivar running_state }

  let start_considering_dep_node (dep_node : _ Dep_node.t) =
    match currently_considering dep_node.state with
    | Not_considering -> (
      match get_cached_value_in_current_cycle dep_node with
      | None -> newly_considering dep_node dep_node.without_state.input
      | Some cv -> Done cv)
    | Considering
        { running = { sample_attempt; deps_so_far = _ }
        ; completion = Async ivar
        ; _
        } ->
      Needs_work { sample_attempt; work = Fiber.Ivar.read ivar }

  let consider_dep_node (dep_node : _ Dep_node.t) =
    let pre_res = start_considering_dep_node dep_node in
    add_dep_from_caller ~called_from_peek:false dep_node
      (Start_considering_result.sample_attempt_dag_node pre_res);
    match pre_res with
    | Done v -> Fiber.return v
    | Needs_work { sample_attempt = _; work } -> work

  let exec_dep_node dep_node =
    let* res = consider_dep_node dep_node in
    Value.get_async_exn res.value

  let exec_dep_node_internal = consider_dep_node

  let exec t inp = exec_dep_node (dep_node t inp)
end

and Exec_unknown : sig
  val exec_dep_node_internal_from_sync :
    ('a, 'b, 'f) Dep_node.t -> 'b Cached_value.t

  val exec_dep_node_internal :
    ('a, 'b, 'f) Dep_node.t -> 'b Cached_value.t Fiber.t
end = struct
  let exec_dep_node_internal (type i o f) (t : (i, o, f) Dep_node.t) :
      o Cached_value.t Fiber.t =
    match t.without_state.spec.f with
    | Async _ -> Exec_async.exec_dep_node_internal t
    | Sync _ -> Fiber.return (Exec_sync.exec_dep_node_internal t)

  let exec_dep_node_internal_from_sync (type i o f) (dep : (i, o, f) Dep_node.t)
      =
    match dep.without_state.spec.f with
    | Async _ -> Code_error.raise "sync computation depends on async" []
    | Sync _ -> Exec_sync.exec_dep_node_internal dep
end

let exec (type i o f) (t : (i, o, f) t) =
  match t.spec.f with
  | Function.Async _ -> (Exec_async.exec t : f)
  | Function.Sync _ -> (Exec_sync.exec t : f)

let peek_exn (type i o f) (t : (i, o, f) t) inp =
  match Store.find t.cache inp with
  | None -> Code_error.raise "[peek_exn] got a never-forced cell" []
  | Some dep_node -> (
    match currently_considering dep_node.state with
    | Considering _ ->
      Code_error.raise "[peek_exn] got a currently-considering cell" []
    | Not_considering -> (
      match get_cached_value_in_current_cycle dep_node with
      | None -> Code_error.raise "[peek_exn] got a non-evaluated cell" []
      | Some cv ->
        (* Not adding any dependency in the [None] cases sounds somewhat wrong,
           but adding a full dependency is also wrong (the thing doesn't depend
           on the value), and it's unclear that None can be reasonably handled
           at all.

           We just consider it a bug when [peek_exn] raises. *)
        add_dep_from_caller ~called_from_peek:true dep_node Finished;
        Value.get_sync_exn cv.value))

let get_deps (type i o f) (t : (i, o, f) t) inp =
  match Store.find t.cache inp with
  | None -> None
  | Some dep_node -> (
    match get_cached_value_in_current_cycle dep_node with
    | None -> None
    | Some cv ->
      Some
        (List.map cv.deps ~f:(fun (Last_dep.T (dep, _value)) ->
             ( Option.map dep.without_state.spec.info ~f:(fun x -> x.name)
             , ser_input dep.without_state ))))

let get_func name =
  match Spec.find name with
  | None -> User_error.raise [ Pp.textf "function %s doesn't exist!" name ]
  | Some spec -> spec

let call name input =
  let (Spec.T spec) = get_func name in
  let (module Output : Output_simple with type t = _) = spec.output in
  let input = Dune_lang.Decoder.parse spec.decode Univ_map.empty input in
  let+ output =
    (match spec.f with
    | Function.Async f -> f
    | Function.Sync f -> fun x -> Fiber.return (f x))
      input
  in
  Output.to_dyn output

let function_info_of_spec (Spec.T spec) =
  match spec.info with
  | Some info -> info
  | None -> Code_error.raise "[function_info_of_spec] got an unnamed spec" []

let registered_functions () =
  String.Table.to_seq_values Spec.by_name
  |> Seq.fold_left
       ~f:(fun xs x -> List.cons (function_info_of_spec x) xs)
       ~init:[]
  |> List.sort ~compare:(fun x y ->
         String.compare x.Function.Info.name y.Function.Info.name)

let function_info name = get_func name |> function_info_of_spec

let get_call_stack = Call_stack.get_call_stack_without_state

module Sync = struct
  type nonrec ('i, 'o) t = ('i, 'o, 'i -> 'o) t
end

module Async = struct
  type nonrec ('i, 'o) t = ('i, 'o, 'i -> 'o Fiber.t) t
end

let invalidate_dep_node (node : _ Dep_node.t) = node.last_cached_value <- None

module Current_run = struct
  let f () = Run.current ()

  let memo =
    create "current-run"
      ~input:(module Unit)
      ~output:(Simple (module Run))
      ~visibility:Hidden Sync f

  let exec () = exec memo ()

  let invalidate () = invalidate_dep_node (dep_node memo ())
end

let current_run () = Current_run.exec ()

module Lazy_id = Stdune.Id.Make ()

module With_implicit_output = struct
  type ('i, 'o, 'f) t = 'f

  let create (type i o f io) name ?doc ~input ~visibility
      ~output:(module O : Output_simple with type t = o) ~implicit_output
      (typ : (i, o, f) Function.Type.t) (impl : f) =
    let output =
      Output.Simple
        (module struct
          type t = o * io option

          let to_dyn ((o, _io) : t) =
            Dyn.List [ O.to_dyn o; Dyn.String "<implicit output is opaque>" ]
        end)
    in
    match typ with
    | Function.Type.Sync ->
      let memo =
        create name ?doc ~input ~visibility ~output Sync (fun i ->
            Implicit_output.collect_sync implicit_output (fun () -> impl i))
      in
      (fun input ->
         let res, output = exec memo input in
         Implicit_output.produce_opt implicit_output output;
         res
        : f)
    | Function.Type.Async ->
      let memo =
        create name ?doc ~input ~visibility ~output Async (fun i ->
            Implicit_output.collect_async implicit_output (fun () -> impl i))
      in
      (fun input ->
         Fiber.map (exec memo input) ~f:(fun (res, output) ->
             Implicit_output.produce_opt implicit_output output;
             res)
        : f)

  let exec t = t
end

module Cell = struct
  type ('a, 'b, 'f) t = ('a, 'b, 'f) Dep_node.t

  let input (t : (_, _, _) t) = t.without_state.input

  let get_sync (type a b) (dep_node : (a, b, a -> b) Dep_node.t) =
    Exec_sync.exec_dep_node dep_node

  let get_async (type a b) (dep_node : (a, b, a -> b Fiber.t) Dep_node.t) =
    Exec_async.exec_dep_node dep_node

  let invalidate = invalidate_dep_node
end

let cell t inp = dep_node t inp

module Implicit_output = Implicit_output
module Store = Store_intf

let lazy_cell (type a) ?(cutoff = ( == )) f =
  let module Output = struct
    type t = a

    let to_dyn _ = Dyn.Opaque

    let equal = cutoff
  end in
  let visibility = Visibility.Hidden in
  let f = Function.of_type Function.Type.Sync f in
  let spec =
    Spec.create ~info:None
      ~input:(module Unit)
      ~output:(Allow_cutoff (module Output))
      ~visibility ~f
  in
  Exec.make_dep_node ~spec ~state:Not_considering ~last_cached_value:None
    ~input:()

let lazy_ ?(cutoff = ( == )) f =
  let cell = lazy_cell ~cutoff f in
  fun () -> Cell.get_sync cell

let lazy_async_cell (type a) ?(cutoff = ( == )) f =
  let module Output = struct
    type t = a

    let to_dyn _ = Dyn.Opaque

    let equal = cutoff
  end in
  let visibility = Visibility.Hidden in
  let f = Function.of_type Function.Type.Async f in
  let spec =
    Spec.create ~info:None
      ~input:(module Unit)
      ~output:(Allow_cutoff (module Output))
      ~visibility ~f
  in
  let cell =
    Exec.make_dep_node ~spec ~state:Not_considering ~last_cached_value:None
      ~input:()
  in
  cell

let lazy_async ?(cutoff = ( == )) f =
  let cell = lazy_async_cell ~cutoff f in
  fun () -> Cell.get_async cell

module Lazy = struct
  type 'a t = unit -> 'a

  let of_val x () = x

  let create = lazy_

  let force f = f ()

  let map x ~f = create (fun () -> f (force x))

  let map2 x y ~f = create (fun () -> f (x ()) (y ()))

  let bind x ~f = create (fun () -> force (f (force x)))

  module Async = struct
    type 'a t = unit -> 'a Fiber.t

    let of_val a () = Fiber.return a

    let create = lazy_async

    let force f = f ()

    let map t ~f = create (fun () -> Fiber.map ~f (t ()))
  end
end

module Run = struct
  module Fdecl = struct
    (* [Lazy.t] is the simplest way to create a node in the memoization dag. *)
    type nonrec 'a t = 'a Fdecl.t Lazy.t

    let create to_dyn =
      let cell =
        lazy_cell
          ~cutoff:(fun _ _ -> false)
          (fun () ->
            let (_ : Run.t) = current_run () in
            Fdecl.create to_dyn)
      in
      fun () -> Cell.get_sync cell

    let set t x = Fdecl.set (Lazy.force t) x

    let get t = Fdecl.get (Lazy.force t)
  end

  include Run
end

module Poly = struct
  module type Function_interface = sig
    type 'a input

    type 'a output

    val name : string

    val id : 'a input -> 'a Type_eq.Id.t

    val to_dyn : _ input -> Dyn.t
  end

  module Mono (F : Function_interface) = struct
    open F

    type key = K : 'a input -> key

    module Key = struct
      type t = key

      let to_dyn (K t) = to_dyn t

      let hash (K t) = Type_eq.Id.hash (id t)

      let equal (K x) (K y) = Type_eq.Id.equal (id x) (id y)
    end

    type value = V : ('a Type_eq.Id.t * 'a output) -> value

    let get (type a) ~value ~(input_with_matching_id : a input) : a output =
      match value with
      | V (id_v, res) -> (
        match Type_eq.Id.same id_v (id input_with_matching_id) with
        | None ->
          Code_error.raise
            "Type_eq.Id.t mismatch in Memo.Poly: the likely reason is that the \
             provided Function.id returns different ids for the same input."
            [ ("Function.name", Dyn.String name) ]
        | Some Type_eq.T -> res)
  end

  module Sync (Function : sig
    include Function_interface

    val eval : 'a input -> 'a output
  end) =
  struct
    open Function
    include Mono (Function)

    let impl = function
      | K input -> V (id input, eval input)

    let memo = create_hidden name ~input:(module Key) Sync impl

    let eval x = get ~value:(exec memo (K x)) ~input_with_matching_id:x
  end

  module Async (Function : sig
    include Function_interface

    val eval : 'a input -> 'a output Fiber.t
  end) =
  struct
    open Function
    include Mono (Function)

    let impl = function
      | K input -> Fiber.map (eval input) ~f:(fun v -> V (id input, v))

    let memo = create_hidden name ~input:(module Key) Async impl

    let eval x =
      Fiber.map (exec memo (K x)) ~f:(fun value ->
          get ~value ~input_with_matching_id:x)
  end
end

let should_clear_caches =
  match Sys.getenv_opt "DUNE_WATCHING_MODE_INCREMENTAL" with
  | Some "true" -> false
  | Some "false"
  | None ->
    true
  | Some _ ->
    User_error.raise
      [ Pp.text "Invalid value of DUNE_WATCHING_MODE_INCREMENTAL" ]

let restart_current_run () =
  Current_run.invalidate ();
  Run.restart ()

let reset () =
  restart_current_run ();
  if should_clear_caches then Caches.clear ()
