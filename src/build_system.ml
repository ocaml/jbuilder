open! Stdune
open Import
open Fiber.O

module Vspec = Build.Vspec

(* Where we store stamp files for aliases *)
let alias_dir = Path.(relative build_dir) ".aliases"

let () = Hooks.End_of_build.always Memo.reset

module Promoted_to_delete : sig
  val add : Path.t -> unit
  val load : unit -> Path.Set.t
end = struct
  module P = Utils.Persistent(struct
      type t = Path.Set.t
      let name = "PROMOTED-TO-DELETE"
      let version = 1
    end)

  let db = ref Path.Set.empty

  let fn = Path.relative Path.build_dir ".to-delete-in-source-tree"

  let needs_dumping = ref false

  let add p =
    if not (Path.Set.mem !db p) then begin
      needs_dumping := true;
      db := Path.Set.add !db p
    end

  let load () =
    Option.value ~default:Path.Set.empty (P.load fn)

  let dump () =
    if !needs_dumping && Path.build_dir_exists () then begin
      needs_dumping := false;
      load ()
      |> Path.Set.union !db
      |> P.dump fn
    end

  let () = Hooks.End_of_build.always dump
end

let files_in_source_tree_to_delete () =
  Promoted_to_delete.load ()

let rule_loc ~file_tree ~loc ~dir =
  match loc with
  | Some loc -> loc
  | None ->
    let dir = Path.drop_optional_build_context dir in
    let file =
      match
        Option.bind (File_tree.find_dir file_tree dir)
          ~f:File_tree.Dir.dune_file
      with
      | Some file -> File_tree.Dune_file.path file
      | None      -> Path.relative dir "_unknown_"
    in
    Loc.in_file file

module Internal_rule = struct
  module Id = struct
    include Id.Make ()
    module Top_closure_f = Top_closure.Make(Set)(struct
        type 'a t = 'a Fiber.t
        let return = Fiber.return
        let ( >>= ) = Fiber.O.( >>= )
      end)
    module Top_closure = Top_closure.Make(Set)(Monad.Id)
  end

  module T = struct
    type t =
      { id               : Id.t
      ; static_deps      : Static_deps.t Fiber.Once.t
      ; targets          : Path.Set.t
      ; context          : Context.t option
      ; build            : (unit, Action.t) Build.t
      ; mode             : Dune_file.Rule.Mode.t
      ; loc              : Loc.t option
      ; dir              : Path.t
      ; env              : Env.t option
      ; sandbox          : bool
      ; locks            : Path.t list
      ; (* Reverse dependencies discovered so far, labelled by the
           requested target *)
        mutable rev_deps : (Path.t * t) list
      ; (* Transitive reverse dependencies discovered so far. *)
        mutable transitive_rev_deps : Id.Set.t
      }

    let compare a b = Id.compare a.id b.id
  end
  include T

  let _pp fmt { targets; dir ; _ } =
    Fmt.record fmt
      [ "targets", Fmt.const (Fmt.ocaml_list Path.pp) (Path.Set.to_list targets)
      ; "dir", Fmt.const Path.pp dir
      ]

  module Set = Set.Make(T)

  let equal a b = Id.equal a.id b.id
  let hash t = Id.hash t.id

  let loc ~file_tree ~dir t  = rule_loc ~file_tree ~dir ~loc:t.loc

  let to_sexp t : Sexp.t =
    Sexp.Encoder.record
      [ "id", Id.to_sexp t.id
      ; "loc", Sexp.Encoder.option Loc.to_sexp t.loc
      ]

  let lib_deps t =
    (* Forcing this lazy ensures that the various globs and
       [if_file_exists] are resolved inside the [Build.t] value. *)
    let+ _ = Fiber.Once.get t.static_deps in
    Build_interpret.lib_deps t.build

  (* Represent the build goal given by the user. This rule is never
     actually executed and is only used starting point of all
     dependency paths. *)
  let root =
    { id          = Id.gen ()
    ; static_deps = Fiber.Once.create (fun () -> Fiber.return Static_deps.empty)
    ; targets     = Path.Set.empty
    ; context     = None
    ; build       = Build.return (Action.Progn [])
    ; mode        = Standard
    ; loc         = None
    ; dir         = Path.root
    ; env         = None
    ; sandbox     = false
    ; locks       = []
    ; rev_deps    = []
    ; transitive_rev_deps = Id.Set.empty
    }

  (* Create a shim for the main build goal *)
  let shim_of_build_goal ~build ~static_deps =
    { root with
      id = Id.gen ()
    ; static_deps
    ; build
    }
end

module File_kind = struct
  type 'a t =
    | Ignore_contents : unit t
    | Sexp_file       : 'a Vfile_kind.t -> 'a t

  let eq : type a b. a t -> b t -> (a, b) Type_eq.t option = fun a b ->
    match a, b with
    | Ignore_contents, Ignore_contents -> Some Type_eq.T
    | Sexp_file a    , Sexp_file b     -> Vfile_kind.eq a b
    | _                                -> None

  let eq_exn a b = Option.value_exn (eq a b)
end

module File_spec = struct
  type 'a t =
    { rule         : Internal_rule.t (* Rule which produces it *)
    ; mutable kind : 'a File_kind.t
    ; mutable data : 'a option
    }

  type packed = T : _ t -> packed

  let create rule kind =
    T { rule; kind; data = None }
end

module Alias0 = struct
  include Alias
  let dep t = Build.path (stamp_file t)

  let dep_multi_contexts ~dir ~name ~file_tree ~contexts =
    ignore
      (find_dir_specified_on_command_line ~dir ~file_tree : File_tree.Dir.t);
    Build.paths (List.map contexts ~f:(fun ctx ->
      let dir = Path.append (Path.(relative build_dir) ctx) dir in
      stamp_file (make ~dir name)))

  open Build.O

  let dep_rec_internal ~name ~dir ~ctx_dir =
    Build.lazy_no_targets (lazy (
      File_tree.Dir.fold dir ~traverse_ignored_dirs:false
        ~init:(Build.return true)
        ~f:(fun dir acc ->
          let path = Path.append ctx_dir (File_tree.Dir.path dir) in
          let fn = stamp_file (make ~dir:path name) in
          acc
          >>>
          Build.if_file_exists fn
            ~then_:(Build.path fn >>^ Fn.const false)
            ~else_:(Build.arr Fn.id))))

  let dep_rec t ~loc ~file_tree =
    let ctx_dir, src_dir = Path.extract_build_context_dir_exn (Alias.dir t) in
    match File_tree.find_dir file_tree src_dir with
    | None ->
      Build.fail { fail = fun () ->
        Errors.fail loc "Don't know about directory %s!"
          (Path.to_string_maybe_quoted src_dir) }
    | Some dir ->
      let name = Alias.name t in
      dep_rec_internal ~name ~dir ~ctx_dir
      >>^ fun is_empty ->
      if is_empty && not (is_standard name) then
        Errors.fail loc
          "This alias is empty.\n\
           Alias %S is not defined in %s or any of its descendants."
          name (Path.to_string_maybe_quoted src_dir)

  let dep_rec_multi_contexts ~dir:src_dir ~name ~file_tree ~contexts =
    let open Build.O in
    let dir = find_dir_specified_on_command_line ~dir:src_dir ~file_tree in
    Build.all (List.map contexts ~f:(fun ctx ->
      let ctx_dir = Path.(relative build_dir) ctx in
      dep_rec_internal ~name ~dir ~ctx_dir))
    >>^ fun is_empty_list ->
    let is_empty = List.for_all is_empty_list ~f:Fn.id in
    if is_empty && not (is_standard name) then
      die "From the command line:\n\
           @{<error>Error@}: Alias %S is empty.\n\
           It is not defined in %s or any of its descendants."
        name (Path.to_string_maybe_quoted src_dir)

  let package_install ~(context : Context.t) ~pkg =
    make (sprintf ".%s-files" (Package.Name.to_string pkg))
      ~dir:context.build_dir
end

(* The purpose of this module is to detect incorrect use of
   the thunks that add rules to a rule collector.
   Once we migrate to the world where the rules collection is done
   with implicit outputs only, we should get rid of this module (probably along
   with the rules collector). *)
module Thunk_with_backtrace = struct
  type t = {
    thunk : unit -> unit;
    ran : unit option ref;
  }

  let run t =
    match !(t.ran) with
    | Some _last_run ->
      (* CR-someday aalekseyev: this should probably be disallowed. *)
      ()
    | None ->
      t.ran := Some ();
      match Exn_with_backtrace.try_with t.thunk with
      | Error exn ->
        Exn.code_error "thunk raised"
          [ "exn", Exn_with_backtrace.to_sexp exn
          ]
      | Ok ok ->
        ok

  let create thunk =
    { thunk; ran = ref None }
end

module Dir_status = struct

  type collection_stage =
    | Pending
    | Loading
    | Frozen

  type alias_action =
    { stamp  : Digest.t
    ; action : (unit, Action.t) Build.t
    ; locks  : Path.t list
    ; context : Context.t
    ; env : Env.t option
    ; loc : Loc.t option
    }

  module Alias : sig
    type t

    val create : unit -> t

    type immutable =
      { deps     : Dep.Set.t
      ; dyn_deps : (unit, Dep.Set.t) Build.t
      ; actions  : alias_action list
      }

    val freeze : t -> immutable
    val assert_frozen : t -> immutable

    val add_deps : t -> Dep.Set.t -> unit
    val add_dyn_deps : t -> (unit, Dep.Set.t) Build.t -> unit
    val add_action : t -> alias_action -> unit
  end = struct
    type t =
      { mutable deps     : Dep.Set.t
      ; mutable dyn_deps : (unit, Dep.Set.t) Build.t
      ; mutable actions  : alias_action list
      ; mutable frozen : bool
      }

    let create () =
      { deps     = Dep.Set.empty
      ; dyn_deps = Build.return Dep.Set.empty
      ; actions  = []
      ; frozen = false
      }

    type immutable =
      { deps     : Dep.Set.t
      ; dyn_deps : (unit, Dep.Set.t) Build.t
      ; actions  : alias_action list
      }

    let _of_immutable { deps; dyn_deps; actions } =
      { deps; dyn_deps; actions; frozen = true }

    let to_immutable { deps; dyn_deps; actions; frozen } =
      assert frozen;
      { deps; dyn_deps; actions }

    let freeze t =
      if t.frozen
      then Exn.code_error "Alias.freeze called twice" []
      else (t.frozen <- true; to_immutable t)

    let assert_frozen t =
      assert t.frozen;
      to_immutable t

    let add_deps (t : t) deps =
      assert (not t.frozen);
      t.deps <- Dep.Set.union t.deps deps

    let add_dyn_deps (t : t) deps =
      assert (not t.frozen);
      t.dyn_deps <-
        (let open Build.O in
         Build.fanout t.dyn_deps deps >>^ fun (a, b) ->
         Dep.Set.union a b)

    let add_action (t : t) action =
      assert (not t.frozen);
      t.actions <- action :: t.actions

  end

  module Rules_collector : sig
    type t
    type frozen

    (** state transition diagram:
        pending -> loading -> frozen *)

    val create_pending : info:Sexp.t -> unit -> t
    val start_loading :
      t ->
      (unit, [`Already_loading]) Result.t
    val freeze : t -> frozen

    val rules : frozen -> Build_interpret.Rule.t list
    val aliases : frozen -> Alias.immutable String.Map.t

    val forbid_freeze_until_thunk_is_forced :
      t -> Thunk_with_backtrace.t -> unit

    val add_rule : t -> Build_interpret.Rule.t -> unit
    val modify_alias : t -> string -> f:(Alias.t -> unit) -> unit
  end = struct

    type t =
      { mutable rules   : Build_interpret.Rule.t list
      ; mutable aliases : Alias.t String.Map.t
      ; mutable stage   : collection_stage
      ; mutable thunks : Thunk_with_backtrace.t list
      ; info : Sexp.t
      }
    type frozen = t

    let create_pending ~info () =
      { rules   = []
      ; aliases = String.Map.empty
      ; stage   = Pending
      ; thunks = []
      ; info
      }

    let rules t = t.rules
    let aliases t = String.Map.map t.aliases ~f:Alias.assert_frozen

    let assert_not_frozen t why = match t.stage with
      | Frozen ->
        Exn.code_error (sprintf "%s called on a frozen Rules_collector" why) []
      | Pending ->
        ()
      | Loading ->
        ()

    let forbid_freeze_until_thunk_is_forced t (thunk : Thunk_with_backtrace.t) =
      assert_not_frozen t "forbid_freeze";
      assert (Option.is_none !(thunk.ran));
      t.thunks <- thunk :: t.thunks

    let add_rule t rule =
      assert_not_frozen t "add_rule";
      t.rules <- rule :: t.rules

    let modify_alias t name ~f =
      assert_not_frozen t "modify_alias";
      let def =
        match String.Map.find t.aliases name with
        | None ->
          let alias = Alias.create () in
          t.aliases <- String.Map.add t.aliases name alias;
          alias
        | Some x -> x
      in
      f def

    let start_loading t = match t.stage with
      | Frozen ->
        Exn.code_error "start_loading called on a frozen Rules_collector" []
      | Loading -> Error `Already_loading
      | Pending ->
        t.stage <- Loading;
        (Ok ())

    let freeze t =
      match t.stage with
      | Frozen ->
        Exn.code_error "Rules_collector.freeze called twice" []
      | Loading ->
        List.iter t.thunks ~f:(fun (b : Thunk_with_backtrace.t) ->
          if Option.is_none !(b.ran) then
            Exn.code_error
              "tried to freeze with some pending modifications"
              ["pending-modifications",
               (* CR-someday aalekseyev: include information on where the thunk was
                  constructed. This information is a bit too expensive to capture
                  unconditionally though. *)
               Atom ""
              ; "info", t.info
              ]
        );
        t.stage <- Frozen;
        (String.Map.iter t.aliases ~f:(fun x -> ignore (Alias.freeze x));
         t)
      | Pending ->
        Exn.code_error "Rules_collector.freeze called while still Pending" []

  end

  type t =
    | Collecting_rules of Rules_collector.t
    | Loaded  of Path.Set.t (* set of targets in the directory *)
    | Forward of Path.t (* Load this directory first       *)
    | Failed_to_load
end

module Trace : sig
  module Entry : sig
    type t =
      { rule_digest    : Digest.t
      ; targets_digest : Digest.t
      }
  end

  val get : Path.t -> Entry.t option
  val set : Path.t -> Entry.t -> unit
end = struct
  module Entry = struct
    type t =
      { rule_digest    : Digest.t
      ; targets_digest : Digest.t
      }
  end

  (* Keyed by the first target *)
  type t = Entry.t Path.Table.t

  let file = Path.relative Path.build_dir ".db"

  module P = Utils.Persistent(struct
      type nonrec t = t
      let name = "INCREMENTAL-DB"
      let version = 2
    end)

  let needs_dumping = ref false

  let t = lazy (
    match P.load file with
    | Some t -> t
    | None -> Path.Table.create 1024)

  let dump () =
    if !needs_dumping && Path.build_dir_exists () then begin
      needs_dumping := false;
      P.dump file (Lazy.force t)
    end

  let () = Hooks.End_of_build.always dump

  let get path =
    let t = Lazy.force t in
    Path.Table.find t path

  let set path e =
    let t = Lazy.force t in
    needs_dumping := true;
    Path.Table.replace t ~key:path ~data:e
end

type extra_sub_directories_to_keep =
  | All
  | These of String.Set.t

type hook =
  | Rule_started
  | Rule_completed

module Action_and_deps = struct
  type t = Action.t * Dep.Set.t

  let to_sexp (action, deps) =
    Sexp.Encoder.record
      [ "action", Dune_lang.to_sexp
                    (Action.For_shell.encode (Action.for_shell action))
      ; "deps", Dune_lang.to_sexp (Dep.Set.encode deps)
      ]
end

module Rule_fn = struct
  let loc_decl = Fdecl.create ()

  let loc () = Fdecl.get loc_decl ()
end

type t =
  { (* File specification by targets *)
    files       : File_spec.packed Path.Table.t
  ; contexts    : Context.t String.Map.t
  ; file_tree   : File_tree.t
  ; mutable local_mkdirs : Path.Set.t
  ; dirs : Dir_status.t Path.Table.t
  ; gen_rules :
      (dir:Path.t -> string list -> extra_sub_directories_to_keep)
        String.Map.t Fdecl.t
  ; mutable load_dir_stack : Path.t list
  ; (* Set of directories under _build that have at least one rule and
       all their ancestors. *)
    mutable build_dirs_to_keep : Path.Set.t
  ; mutable prefix : (unit, unit) Build.t option
  ; hook : hook -> unit
  ; (* Package files are part of *)
    packages : Package.Name.t Path.Table.t
  }

let t = ref None
let set x =
  match !t with
  | None -> t := Some x
  | Some _ -> Exn.code_error "build system already initialized" []
let get_build_system () =
  match !t with
  | Some t -> t
  | None -> Exn.code_error "build system not yet initialized" []
let reset () = t := None
let t = get_build_system

let string_of_paths set =
  Path.Set.to_list set
  |> List.map ~f:(fun p -> sprintf "- %s"
                             (Path.to_string_maybe_quoted
                                (Path.drop_optional_build_context p)))
  |> String.concat ~sep:"\n"

let set_rule_generators generators =
  let t = t () in
  assert (String.Map.keys generators = String.Map.keys t.contexts);
  Fdecl.set t.gen_rules generators

let get_dir_status t ~dir =
  Path.Table.find_or_add t.dirs dir ~f:(fun _ ->
    if Path.is_in_source_tree dir then
      Dir_status.Loaded (File_tree.files_of t.file_tree dir)
    else if Path.equal dir Path.build_dir then
      (* Not allowed to look here *)
      Dir_status.Loaded Path.Set.empty
    else if not (Path.is_managed dir) then
      Dir_status.Loaded
        (match Path.readdir_unsorted dir with
         | exception _ -> Path.Set.empty
         | files ->
           Path.Set.of_list (List.map files ~f:(Path.relative dir)))
    else begin
      let (ctx, sub_dir) = Path.extract_build_context_exn dir in
      if ctx = ".aliases" then
        Forward (Path.(append build_dir) sub_dir)
      else if ctx <> "install" && not (String.Map.mem t.contexts ctx) then
        Dir_status.Loaded Path.Set.empty
      else
        Collecting_rules
          (Dir_status.Rules_collector.create_pending ~info:(Path.to_sexp dir) ())
    end)

module Target = Build_interpret.Target
module Pre_rule = Build_interpret.Rule

let get_file : type a. t -> Path.t -> a File_kind.t -> a File_spec.t =
  fun t fn kind ->
    match Path.Table.find t.files fn with
    | None ->
      let loc = Rule_fn.loc () in
      Errors.fail_opt loc "no rule found for %s" (Path.to_string fn)
    | Some (File_spec.T file) ->
      let Type_eq.T = File_kind.eq_exn kind file.kind in
      file

let vfile_to_string (type a) (module K : Vfile_kind.S with type t = a) _fn x =
  K.to_string x

type bs = t

module Build_exec = struct
  open Build.Repr

  let exec (bs : bs) ~(eval_pred : Dep.eval_pred) (t : ('a, 'b) Build.t) (x : 'a)
    : 'b * Dep.Set.t =
    let rec exec
      : type a b. Dep.Set.t ref -> (a, b) t -> a -> b = fun dyn_deps t x ->
      match t with
      | Arr f -> f x
      | Targets _ -> x
      | Store_vfile (Vspec.T (fn, kind)) ->
        let file = get_file bs fn (Sexp_file kind) in
        file.data <- Some x;
        Write_file (fn, vfile_to_string kind fn x)
      | Compose (a, b) ->
        exec dyn_deps a x |> exec dyn_deps b
      | First t ->
        let x, y = x in
        (exec dyn_deps t x, y)
      | Second t ->
        let x, y = x in
        (x, exec dyn_deps t y)
      | Split (a, b) ->
        let x, y = x in
        let x = exec dyn_deps a x in
        let y = exec dyn_deps b y in
        (x, y)
      | Fanout (a, b) ->
        let a = exec dyn_deps a x in
        let b = exec dyn_deps b x in
        (a, b)
      | Deps _ -> x
      | Paths_for_rule _ -> x
      | Paths_glob g -> eval_pred g
      | Contents p -> Io.read_file p
      | Lines_of p -> Io.lines_of_file p
      | Vpath (Vspec.T (fn, kind)) ->
        let file : b File_spec.t = get_file bs fn (Sexp_file kind) in
        Option.value_exn file.data
      | Dyn_paths t ->
        let fns = exec dyn_deps t x in
        dyn_deps := Dep.Set.add_paths !dyn_deps fns;
        x
      | Dyn_deps t ->
        let fns = exec dyn_deps t x in
        dyn_deps := Dep.Set.union !dyn_deps fns;
        x
      | Record_lib_deps _ -> x
      | Fail { fail } -> fail ()
      | If_file_exists (_, state) ->
        exec dyn_deps (get_if_file_exists_exn state) x
      | Catch (t, on_error) -> begin
          try
            exec dyn_deps t x
          with exn ->
            on_error exn
        end
      | Lazy_no_targets t ->
        exec dyn_deps (Lazy.force t) x
      | Memo m ->
        begin match m.state with
        | Evaluated (x, deps) ->
          dyn_deps := Dep.Set.union !dyn_deps deps;
          x
        | Evaluating ->
          die "Dependency cycle evaluating memoized build arrow %s" m.name
        | Unevaluated ->
          m.state <- Evaluating;
          let dyn_deps' = ref Dep.Set.empty in
          match exec dyn_deps' m.t x with
          | x ->
            m.state <- Evaluated (x, !dyn_deps');
            dyn_deps := Dep.Set.union !dyn_deps !dyn_deps';
            x
          | exception exn ->
            m.state <- Unevaluated;
            reraise exn
        end
    in
    let dyn_deps = ref Dep.Set.empty in
    let result = exec dyn_deps (Build.repr t) x in
    (result, !dyn_deps)
end

(* [copy_source] is [true] for rules copying files from the source directory *)
let add_spec t fn spec ~copy_source =
  match Path.Table.find t.files fn with
  | None ->
    Path.Table.add t.files fn spec
  | Some (File_spec.T { rule; _ }) ->
    match copy_source, rule.mode with
    | true, (Standard | Not_a_rule_stanza) ->
      Errors.warn (Internal_rule.loc rule ~dir:(Path.parent_exn fn)
                     ~file_tree:t.file_tree)
        "File %s is both generated by a rule and present in the source tree.\n\
         As a result, the rule is currently ignored, however this will become an error \
         in the future.\n\
         %t"
        (String.maybe_quoted (Path.basename fn))
        (fun ppf ->
           match rule.mode with
           | Not_a_rule_stanza ->
             Format.fprintf ppf "Delete file %s to get rid of this warning."
               (Path.to_string_maybe_quoted (Path.drop_optional_build_context fn))
           | Standard ->
             Format.fprintf ppf
               "To keep the current behavior and get rid of this warning, add a field \
                (fallback) to the rule."
           | _ -> assert false);
      Path.Table.add t.files fn spec
    | _ ->
      let (File_spec.T { rule = rule2; _ }) = spec in
      let string_of_loc = function
        | None -> "<internal location>"
        | Some { Loc.start; _ } ->
          start.pos_fname ^ ":" ^ string_of_int start.pos_lnum
      in
      die "Multiple rules generated for %s:\n\
           - %s\n\
           - %s"
        (Path.to_string_maybe_quoted fn)
        (if copy_source then
           "<internal copy rule>"
         else
           string_of_loc rule.loc)
        (string_of_loc rule2.loc)

let create_file_specs t targets rule ~copy_source =
  List.iter targets ~f:(function
    | Target.Normal fn ->
      add_spec t fn (File_spec.create rule Ignore_contents) ~copy_source
    | Target.Vfile (Vspec.T (fn, kind)) ->
      add_spec t fn (File_spec.create rule (Sexp_file kind)) ~copy_source)

(* This contains the targets of the actions that are being executed. On exit, we
   need to delete them as they might contain garbage *)
let pending_targets = ref Path.Set.empty

let () =
  Hooks.End_of_build.always (fun () ->
    let fns = !pending_targets in
    pending_targets := Path.Set.empty;
    Path.Set.iter fns ~f:Path.unlink_no_err)

let compute_targets_digest_after_rule_execution ~loc targets =
  let good, bad =
    List.partition_map targets ~f:(fun fn ->
      match Utils.Cached_digest.refresh fn with
      | digest -> Left digest
      | exception (Unix.Unix_error _ | Sys_error _) -> Right fn)
  in
  match bad with
  | [] -> Digest.string (Marshal.to_string good [])
  | missing ->
    Errors.fail_opt loc
      "rule failed to generate the following targets:\n%s"
      (string_of_paths (Path.Set.of_list missing))

let make_local_dir t fn =
  if not (Path.Set.mem t.local_mkdirs fn) then begin
    Path.mkdir_p fn;
    t.local_mkdirs <- Path.Set.add t.local_mkdirs fn
  end

let make_local_dirs t ~dirs =
  Path.Set.iter dirs ~f:(make_local_dir t)

let make_local_managed_dir t path =
  if Path.is_managed path then
    make_local_dir t path

let make_local_dirs_map t ~dirs ~map_path =
  Path.Set.iter dirs ~f:(fun dir ->
    map_path dir
    |> make_local_managed_dir t)

let sandbox_dir = Path.relative Path.build_dir ".sandbox"

let locks : (Path.t, Fiber.Mutex.t) Hashtbl.t = Hashtbl.create 32

let rec with_locks mutexes ~f =
  match mutexes with
  | [] -> f ()
  | m :: mutexes ->
    Fiber.Mutex.with_lock
      (Hashtbl.find_or_add locks m ~f:(fun _ -> Fiber.Mutex.create ()))
      (fun () -> with_locks mutexes ~f)

let remove_old_artifacts t ~dir ~subdirs_to_keep =
  if not (Path.is_in_build_dir dir) ||
     Path.Table.mem t.files (Path.relative dir Config.dune_keep_fname) then
    ()
  else
    match Path.readdir_unsorted dir with
    | exception _ -> ()
    | files ->
      List.iter files ~f:(fun fn ->
        let path = Path.relative dir fn in
        let path_is_a_target = Path.Table.mem t.files path in
        if path_is_a_target then ()
        else
          match Unix.lstat (Path.to_string path) with
          | { st_kind = S_DIR; _ } -> begin
              match subdirs_to_keep with
              | All -> ()
              | These set ->
                if String.Set.mem set fn ||
                   Path.Set.mem t.build_dirs_to_keep path then ()
                else
                  Path.rm_rf path
            end
          | exception _ -> Path.unlink path
          | _ -> Path.unlink path)

let no_rule_found =
  let fail fn ~loc =
    Errors.fail_opt loc "No rule found for %s" (Utils.describe_target fn)
  in
  fun t ~loc fn ->
    match Utils.analyse_target fn with
    | Other _ -> fail fn ~loc
    | Regular (ctx, _) ->
      if String.Map.mem t.contexts ctx then
        fail fn ~loc
      else
        die "Trying to build %s but build context %s doesn't exist.%s"
          (Path.to_string_maybe_quoted fn)
          ctx
          (hint ctx (String.Map.keys t.contexts))
    | Alias (ctx, fn') ->
      if String.Map.mem t.contexts ctx then
        fail fn ~loc
      else
        let fn = Path.append (Path.relative Path.build_dir ctx) fn' in
        die "Trying to build alias %s but build context %s doesn't exist.%s"
          (Path.to_string_maybe_quoted fn)
          ctx
          (hint ctx (String.Map.keys t.contexts))

type rule_collection_implicit_output = Thunk_with_backtrace.t Appendable_list.t
let rule_collection_implicit_output =
  Memo.Implicit_output.add (module struct
    type t = rule_collection_implicit_output

    let union x y = Appendable_list.(@) x y
    let name = "rule collection"
  end)

let handle_add_rule_effects f =
  let res, effects =
    Memo.Implicit_output.collect_sync rule_collection_implicit_output f
  in
  Option.iter effects ~f:(fun l ->
    List.iter (Appendable_list.to_list l) ~f:(Thunk_with_backtrace.run));
  res

module Pred = struct
  let eval_def =
    Memo.create "eval-pred"
      ~doc:"Evaluate a predicate in a directory"
      ~input:(module File_selector)
      ~output:(Allow_cutoff (module Path.Set))
      ~visibility:Hidden
      Sync
      None

  let build_def =
    Memo.create "build-pred"
      ~doc:"build a predicate"
      ~input:(module File_selector)
      ~output:(Allow_cutoff (module Unit))
      ~visibility:Hidden
      Async
      None
end

let eval_pred g = Memo.exec Pred.eval_def g
let build_pred g = Memo.exec Pred.build_def g

let rec compile_rule t ?(copy_source=false) pre_rule =
  let { Pre_rule.
        context
      ; env
      ; build
      ; targets = target_specs
      ; sandbox
      ; mode
      ; locks
      ; loc
      ; dir
      } =
    pre_rule
  in
  let targets = Target.paths target_specs in
  let static_deps = static_deps t build in
  let rule =
    let id = Internal_rule.Id.gen () in
    { Internal_rule.
      id
    ; static_deps
    ; targets
    ; build
    ; context
    ; env
    ; sandbox
    ; locks
    ; mode
    ; loc
    ; dir
    ; transitive_rev_deps = Internal_rule.Id.Set.singleton id
    ; rev_deps = []
    }
  in
  create_file_specs t target_specs rule ~copy_source

and static_deps t build =
  Fiber.Once.create (fun () ->
    Fiber.return
      (Build_interpret.static_deps build
         ~all_targets:(targets_of t)))

and start_rule t _rule =
  t.hook Rule_started

and setup_copy_rules t ~ctx_dir ~non_target_source_files =
  Path.Set.iter non_target_source_files ~f:(fun path ->
    let ctx_path = Path.append ctx_dir path in
    let build = Build.copy ~src:path ~dst:ctx_path in
    (* We temporarily allow overrides while setting up copy rules from
       the source directory so that artifact that are already present
       in the source directory are not re-computed.

       This allows to keep generated files in tarballs. Maybe we
       should allow it on a case-by-case basis though. *)
    compile_rule t (Pre_rule.make build ~context:None ~env:None)
      ~copy_source:true)

and load_dir   t ~dir = ignore (load_dir_and_get_targets t ~dir : Path.Set.t)
and targets_of t ~dir =         load_dir_and_get_targets t ~dir

and load_dir_and_get_targets t ~dir =
  match get_dir_status t ~dir with
  | Failed_to_load -> raise Already_reported

  | Loaded targets -> targets

  | Forward dir' ->
    load_dir t ~dir:dir';
    begin match get_dir_status t ~dir with
    | Loaded targets -> targets
    | _ -> assert false
    end

  | Collecting_rules collector ->
    let () =
      match Dir_status.Rules_collector.start_loading collector with
      | Error `Already_loading ->
        die "recursive dependency between directories:\n    %s"
          (String.concat ~sep:"\n--> "
             (List.map t.load_dir_stack ~f:Utils.describe_target))
      | Ok () ->
        ()
    in

    t.load_dir_stack <- dir :: t.load_dir_stack;

    try
      load_dir_step2_exn t ~dir ~collector
    with exn ->
      (match Path.Table.find t.dirs dir with
       | Some (Loaded _) -> ()
       | _ ->
         (match t.load_dir_stack with
          | [] -> assert false
          | x :: l ->
            t.load_dir_stack <- l;
            assert (Path.equal x dir)));
      Path.Table.replace t.dirs ~key:dir ~data:Failed_to_load;
      reraise exn

and load_dir_step2_exn t ~dir ~collector =
  let context_name, sub_dir = Path.extract_build_context_exn dir in

  (* Load all the rules *)
  let extra_subdirs_to_keep =
    if context_name = "install" then
      These String.Set.empty
    else
      let gen_rules = String.Map.find_exn (Fdecl.get t.gen_rules) context_name in
      handle_add_rule_effects
        (fun () ->
           gen_rules ~dir (Path.explode_exn sub_dir))
  in
  let collector = Dir_status.Rules_collector.freeze collector in
  let rules = Dir_status.Rules_collector.rules collector in

  (* Compute alias rules *)
  let alias_dir = Path.append (Path.relative alias_dir context_name) sub_dir in
  let alias_rules, alias_stamp_files =
    let open Build.O in
    let aliases =
      Dir_status.Rules_collector.aliases collector
    in
    let aliases =
      if String.Map.mem aliases "default" then
        aliases
      else
        match Path.extract_build_context_dir dir with
        | None -> aliases
        | Some (ctx_dir, src_dir) ->
          match File_tree.find_dir t.file_tree src_dir with
          | None -> aliases
          | Some dir ->
            String.Map.add aliases "default"
              ({ deps = Dep.Set.empty
               ; dyn_deps =
                   (Alias0.dep_rec_internal ~name:"install" ~dir ~ctx_dir
                    >>^ fun (_ : bool) ->
                    Dep.Set.empty)
               ; actions = []
               } : Dir_status.Alias.immutable)
    in
    String.Map.foldi aliases ~init:([], Path.Set.empty)
      ~f:(fun name { Dir_status.Alias.deps; dyn_deps; actions } (rules, alias_stamp_files) ->
        let base_path = Path.relative alias_dir name in
        let rules, deps =
          List.fold_left actions ~init:(rules, deps)
            ~f:(fun (rules, deps)
                 { Dir_status. stamp; action; locks ; context ; loc ; env } ->
                 let path =
                   Path.extend_basename base_path
                     ~suffix:("-" ^ Digest.to_string stamp)
                 in
                 let rule =
                   Pre_rule.make ~locks ~context:(Some context) ~env ?loc
                     (Build.progn [ action; Build.create_file path ])
                 in
                 (rule :: rules, Dep.Set.add deps (Dep.file path)))
        in
        let path = Path.extend_basename base_path ~suffix:Alias0.suffix in
        let rule =
          Pre_rule.make
            ~context:None
            ~env:None
            (Build.deps deps >>>
             dyn_deps >>>
             Build.dyn_deps (Build.arr Fn.id)
             >>^ (fun dyn_deps ->
               let deps = Dep.Set.union deps dyn_deps in
               let trace = Dep.Set.trace ~env:Env.initial ~eval_pred deps in
               Marshal.to_string trace []
               |> Digest.string
               |> Digest.to_string)
             >>>
             Build.write_file_dyn path)
        in
        ( rule :: rules
        , Path.Set.add alias_stamp_files path
        )
      )
  in
  Path.Table.replace t.dirs ~key:alias_dir ~data:(Loaded alias_stamp_files);

  (* Compute the set of targets and the set of source files that must not be copied *)
  let user_rule_targets, source_files_to_ignore =
    List.fold_left rules ~init:(Path.Set.empty, Path.Set.empty)
      ~f:(fun (acc_targets, acc_ignored) { Pre_rule.targets; mode; _ } ->
        let targets = Build_interpret.Target.paths targets in
        (Path.Set.union targets acc_targets,
         match mode with
         | Promote _ | Ignore_source_files ->
           Path.Set.union targets acc_ignored
         | _ ->
           acc_ignored))
  in
  let source_files_to_ignore =
    Path.Set.map source_files_to_ignore ~f:Path.drop_build_context_exn in
  (* Take into account the source files *)
  let targets, to_copy, subdirs_to_keep =
    match context_name with
    | "install" ->
      (user_rule_targets,
       None,
       String.Set.empty)
    | ctx_name ->
      (* This condition is [true] because of [get_dir_status] *)
      assert (String.Map.mem t.contexts ctx_name);
      let files, subdirs =
        match File_tree.find_dir t.file_tree sub_dir with
        | None -> (Path.Set.empty, String.Set.empty)
        | Some dir ->
          (File_tree.Dir.file_paths    dir,
           File_tree.Dir.sub_dir_names dir)
      in
      let files = Path.Set.diff files source_files_to_ignore in
      if Path.Set.is_empty files then
        (user_rule_targets, None, subdirs)
      else
        let ctx_path = Path.(relative build_dir) context_name in
        (Path.Set.union user_rule_targets
           (Path.Set.map files ~f:(Path.append ctx_path)),
         Some (ctx_path, files),
         subdirs)
  in
  let subdirs_to_keep =
    match extra_subdirs_to_keep with
    | All -> All
    | These set -> These (String.Set.union subdirs_to_keep set)
  in

  (* Filter out fallback rules *)
  let rules =
    match to_copy with
    | None ->
      (* If there are no source files to copy, fallback rules are
         automatically kept *)
      rules
    | Some (_, to_copy) ->
      List.filter rules ~f:(fun (rule : Build_interpret.Rule.t) ->
        match rule.mode with
        | Standard | Promote _
        | Not_a_rule_stanza | Ignore_source_files -> true
        | Fallback ->
          let source_files_for_targtes =
            List.fold_left rule.targets ~init:Path.Set.empty
              ~f:(fun acc target ->
                Path.Set.add acc
                  (Build_interpret.Target.path target
                   (* All targets are in [dir] and we know it
                      correspond to a directory of a build context
                      since there are source files to copy, so this
                      call can't fail. *)
                   |> Path.drop_build_context_exn))
          in
          if Path.Set.is_subset source_files_for_targtes ~of_:to_copy then
            (* All targets are present *)
            false
          else begin
            if Path.Set.is_empty (Path.Set.inter source_files_for_targtes to_copy) then
              (* No target is present *)
              true
            else begin
              let absent_targets =
                Path.Set.diff source_files_for_targtes to_copy
              in
              let present_targets =
                Path.Set.diff source_files_for_targtes absent_targets
              in
              Errors.fail
                (rule_loc
                   ~file_tree:t.file_tree
                   ~loc:rule.loc
                   ~dir:(Path.drop_optional_build_context dir))
                "\
Some of the targets of this fallback rule are present in the source tree,
and some are not. This is not allowed. Either none of the targets must
be present in the source tree, either they must all be.

The following targets are present:
%s

The following targets are not:
%s
"
                (string_of_paths present_targets)
                (string_of_paths absent_targets)
            end
          end)
  in

  (* Set the directory status to loaded *)
  Path.Table.replace t.dirs ~key:dir ~data:(Loaded targets);
  (match t.load_dir_stack with
   | [] -> assert false
   | x :: l ->
     t.load_dir_stack <- l;
     assert (Path.equal x dir));

  (* Compile the rules and cleanup stale artifacts *)
  List.iter rules ~f:(compile_rule t ~copy_source:false);
  Option.iter to_copy ~f:(fun (ctx_dir, source_files) ->
    setup_copy_rules t ~ctx_dir ~non_target_source_files:source_files);
  remove_old_artifacts t ~dir ~subdirs_to_keep;

  List.iter alias_rules ~f:(compile_rule t ~copy_source:false);
  remove_old_artifacts t ~dir:alias_dir ~subdirs_to_keep;

  targets

let get_file_spec_other t fn =
  let dir = Path.parent_exn fn in
  if Path.is_in_build_dir dir then
    load_dir t ~dir;
  Fiber.return (Path.Table.find t.files fn)

and get_file_spec t path =
  match Path.Table.find t.files path with
  | Some _ as some -> Fiber.return some
  | None ->
    let dir = Path.parent_exn path in
    if Path.is_strict_descendant_of_build_dir dir then begin
      load_dir t ~dir;
      match Path.Table.find t.files path with
      | Some _ as some -> Fiber.return some
      | None ->
        let loc = Rule_fn.loc () in
        no_rule_found t ~loc path
    end else if Path.exists path then
      Fiber.return None
    else
      let loc = Rule_fn.loc () in
      Errors.fail_opt loc
        "File unavailable: %s" (Path.to_string_maybe_quoted path)

let all_targets () =
  let t = t () in
  String.Map.iter t.contexts ~f:(fun ctx ->
    File_tree.fold t.file_tree ~traverse_ignored_dirs:true ~init:()
      ~f:(fun dir () ->
        load_dir t
          ~dir:(Path.append ctx.Context.build_dir (File_tree.Dir.path dir))));
  Path.Table.foldi t.files ~init:[] ~f:(fun key _ acc -> key :: acc)

let build_file_def =
  Memo.create
    "build-file"
    ~output:(Allow_cutoff (module Unit))
    ~doc:"Build a file."
    ~input:(module Path)
    ~visibility:(Public Path_dune_lang.decode)
    Async
    None

let build_file = Memo.exec build_file_def

let execute_rule_def =
  Memo.create
    "execute-rule"
    ~output:(Allow_cutoff (module Unit))
    ~doc:"-"
    ~input:(module Internal_rule)
    ~visibility:Hidden
    Async
    None

let execute_rule = Memo.exec execute_rule_def

let build_deps =
  Dep.Set.parallel_iter ~f:(function
    | Alias a -> build_file (Alias.stamp_file a)
    | File f -> build_file f
    | Glob g -> build_pred g
    | Universe
    | Env _ -> Fiber.return ())

(* Evaluate a rule and return the action and set of dynamic dependencies *)
let evaluate_action_and_dynamic_deps_def =
  let f (rule : Internal_rule.t) =
    let t = t () in
    let* static_deps = Fiber.Once.get rule.static_deps in
    let rule_deps = Static_deps.rule_deps static_deps in
    let+ () = build_deps rule_deps in
    Build_exec.exec t ~eval_pred rule.build ()
  in
  Memo.create
    "evaluate-action-and-dynamic-deps"
    ~output:(Simple (module Action_and_deps))
    ~doc:"Evaluate the build arrow part of a rule and return the \
          action and dynamic dependency of the rule."
    ~input:(module Internal_rule)
    ~visibility:Hidden
    Async
    (Some f)

let evaluate_action_and_dynamic_deps =
  Memo.exec evaluate_action_and_dynamic_deps_def

let () =
  Fdecl.set Rule_fn.loc_decl (fun () ->
    let stack = Memo.get_call_stack () in
    List.find_map stack ~f:(fun frame ->
      match Memo.Stack_frame.as_instance_of frame ~of_:execute_rule_def with
      | Some input -> Some input
      | None ->
        Memo.Stack_frame.as_instance_of frame ~of_:evaluate_action_and_dynamic_deps_def)
    |> Option.bind ~f:(fun rule -> rule.Internal_rule.loc))

let evaluate_rule (rule : Internal_rule.t) =
  let* static_deps = Fiber.Once.get rule.static_deps in
  let+ (action, dynamic_action_deps) = evaluate_action_and_dynamic_deps rule in
  let static_action_deps = Static_deps.action_deps static_deps in
  let action_deps = Dep.Set.union static_action_deps dynamic_action_deps in
  (action, action_deps)

(* Same as the function just bellow, but with less opportunity for
   parallelism. We keep this dead code here for documentation purposes
   as it is easier to read the one bellow. The reader only has to
   check that both function do the same thing. *)
let _evaluate_rule_and_wait_for_dependencies rule =
  let* (action, action_deps) = evaluate_rule rule in
  let+ () = build_deps action_deps in
  (action, action_deps)

(* The following function does exactly the same as the function above
   with the difference that it starts the build of static dependencies
   before we know the final action and set of dynamic dependencies. We
   do this to increase opportunities for parallelism.
*)
let evaluate_rule_and_wait_for_dependencies (rule : Internal_rule.t) =
  let* static_deps = Fiber.Once.get rule.static_deps in
  let static_action_deps = Static_deps.action_deps static_deps in
  (* Build the static dependencies in parallel with evaluation the
     action and dynamic dependencies *)
  let* (action, dynamic_action_deps) =
    Fiber.fork_and_join_unit
      (fun () -> build_deps static_action_deps)
      (fun () -> evaluate_action_and_dynamic_deps rule)
  in
  build_deps dynamic_action_deps
  >>>
  let action_deps = Dep.Set.union static_action_deps dynamic_action_deps in
  Fiber.return (action, action_deps)

let () =
  (* Evaluate and execute a rule *)
  let execute_rule rule =
    let t = t () in
    let { Internal_rule.
          dir
        ; targets
        ; env
        ; context
        ; mode
        ; sandbox
        ; locks
        ; id = _
        ; static_deps = _
        ; build = _
        ; loc
        ; transitive_rev_deps = _
        ; rev_deps = _
        } = rule
    in
    start_rule t rule;
    let* (action, deps) = evaluate_rule_and_wait_for_dependencies rule in
    make_local_dir t dir;
    let targets_as_list  = Path.Set.to_list targets  in
    let head_target = List.hd targets_as_list in
    let prev_trace = Trace.get head_target in
    let rule_digest =
      let env =
        match env, context with
        | None, None -> Env.initial
        | Some e, _ -> e
        | None, Some c -> c.env
      in
      let trace =
        ( Dep.Set.trace deps ~env ~eval_pred
        , List.map targets_as_list ~f:Path.to_string
        , Option.map context ~f:(fun c -> c.name)
        , Action.for_shell action
        )
      in
      Digest.string (Marshal.to_string trace [])
    in
    let targets_digest =
      match List.map targets_as_list ~f:Utils.Cached_digest.file with
      | l -> Some (Digest.string (Marshal.to_string l []))
      | exception (Unix.Unix_error _ | Sys_error _) -> None
    in
    let sandbox_dir =
      if sandbox then
        let digest = Digest.to_string rule_digest in
        Some (Path.relative sandbox_dir digest)
      else
        None
    in
    let force =
      !Clflags.force &&
      List.exists targets_as_list ~f:Path.is_alias_stamp_file
    in
    let something_changed =
      match prev_trace, targets_digest, Dep.Set.has_universe deps with
      | Some prev_trace, Some targets_digest, false ->
        prev_trace.rule_digest <> rule_digest ||
        prev_trace.targets_digest <> targets_digest
      | _ -> true
    in
    begin
      if force || something_changed then begin
        List.iter targets_as_list ~f:Path.unlink_no_err;
        pending_targets := Path.Set.union targets !pending_targets;
        let action =
          match sandbox_dir with
          | None ->
            action
          | Some sandbox_dir ->
            Path.rm_rf sandbox_dir;
            let sandboxed path = Path.sandbox_managed_paths ~sandbox_dir path in
            make_local_dirs_map t ~dirs:(Dep.Set.dirs deps) ~map_path:sandboxed;
            make_local_dir t (sandboxed dir);
            Action.sandbox action
              ~sandboxed
              ~deps
              ~targets:targets_as_list
              ~eval_pred
        in
        make_local_dirs t ~dirs:(Action.chdirs action);
        let+ () =
          with_locks locks ~f:(fun () ->
            Action_exec.exec ~context ~env ~targets action)
        in
        Option.iter sandbox_dir ~f:Path.rm_rf;
        (* All went well, these targets are no longer pending *)
        pending_targets := Path.Set.diff !pending_targets targets;
        let targets_digest =
          compute_targets_digest_after_rule_execution ~loc targets_as_list
        in
        Trace.set head_target { rule_digest; targets_digest }
      end else
        Fiber.return ()
    end >>| fun () ->
    begin
      match mode with
      | Standard | Fallback | Not_a_rule_stanza | Ignore_source_files -> ()
      | Promote (lifetime, into) ->
        Path.Set.iter targets ~f:(fun path ->
          let in_source_tree = Path.drop_build_context_exn path in
          let in_source_tree =
            match into with
            | None -> in_source_tree
            | Some { loc; dir } ->
              Path.relative
                (Path.relative (Path.parent_exn in_source_tree) dir
                   ~error_loc:loc)
                (Path.basename in_source_tree)
          in
          if not (Path.exists in_source_tree) ||
             (Utils.Cached_digest.file path <>
              Utils.Cached_digest.file in_source_tree) then begin
            if lifetime = Until_clean then
              Promoted_to_delete.add in_source_tree;
            Scheduler.ignore_for_watch in_source_tree;
            Io.copy_file ~src:path ~dst:in_source_tree ()
          end)
    end;
    t.hook Rule_completed
  in
  Memo.set_impl execute_rule_def execute_rule

let () =
  (* a rule can have multiple files, but rule.run_rule may only be called once *)
  let build_file path =
    let t = t () in
    let on_error exn = Dep_path.reraise exn (Path path) in
    Fiber.with_error_handler ~on_error (fun () ->
      get_file_spec t path >>= function
      | None ->
        (* file already exists *)
        Fiber.return ()
      | Some (File_spec.T file) -> execute_rule file.rule)
  in
  Memo.set_impl build_file_def build_file

let () =
  let f g =
    eval_pred g
    |> Path.Set.to_list
    |> Fiber.parallel_iter ~f:build_file
  in
  Memo.set_impl Pred.build_def f

let shim_of_build_goal t request =
  let request =
    let open Build.O in
    request >>^ fun () ->
    Action.Progn []
  in
  Internal_rule.shim_of_build_goal
    ~build:request
    ~static_deps:(static_deps t request)

let build_request t ~request =
  let result = Fdecl.create () in
  let request =
    let open Build.O in
    request >>^ fun res ->
    Fdecl.set result res
  in
  let rule = shim_of_build_goal t request in
  let+ (_act, _deps) = evaluate_rule_and_wait_for_dependencies rule in
  Fdecl.get result

let process_memcycle exn =
  let cycle =
    Memo.Cycle_error.get exn
    |> List.filter_map ~f:(Memo.Stack_frame.as_instance_of ~of_:build_file_def)
  in
  match List.last cycle with
  | None ->
    let frames : string list =
      Memo.Cycle_error.get exn
      |> List.map ~f:(Format.asprintf "%a" Memo.Stack_frame.pp)
    in
    Exn.code_error "dependency cycle that does not involve any files"
      ["frames", Sexp.Encoder.(list string) frames]
  | Some last ->
    let first = List.hd cycle in
    let cycle = if last = first then cycle else last :: cycle in
    Exn.Fatal_error
      (Format.asprintf "Dependency cycle between the following files:\n    %s"
         (List.map cycle ~f:Path.to_string_maybe_quoted
          |> String.concat ~sep:"\n--> "))

let do_build ~request =
  let t = t () in
  Hooks.End_of_build.once Promotion.finalize;
  (fun () -> build_request t ~request)
  |> Fiber.with_error_handler ~on_error:(
    Exn_with_backtrace.map_and_reraise
      ~f:(Dep_path.map ~f:(function
        | Memo.Cycle_error.E exn -> process_memcycle exn
        | _ as exn -> exn
      )))

let init ~contexts ~file_tree ~hook =
  let contexts =
    List.map contexts ~f:(fun c -> (c.Context.name, c))
    |> String.Map.of_list_exn
  in
  set
    { contexts
    ; files      = Path.Table.create 1024
    ; packages   = Path.Table.create 1024
    ; local_mkdirs = Path.Set.empty
    ; dirs       = Path.Table.create 1024
    ; load_dir_stack = []
    ; file_tree
    ; gen_rules = Fdecl.create ()
    ; build_dirs_to_keep = Path.Set.empty
    ; prefix = None
    ; hook
    }

module Rule = struct
  module Id = Internal_rule.Id

  type t =
    { id      : Id.t
    ; dir     : Path.t
    ; deps    : Dep.Set.t
    ; targets : Path.Set.t
    ; context : Context.t option
    ; action  : Action.t
    }

  let compare a b = Id.compare a.id b.id

  module Set = Set.Make(struct type nonrec t = t let compare = compare end)
end

let set_package file package =
  let t = t () in
  Path.Table.add t.packages file package

let package_deps pkg files =
  let t = t () in
  let rules_seen = ref Internal_rule.Set.empty in
  let add_package acc p =
    let open Package.Name.Infix in
    if p = pkg then
      acc
    else
      Package.Name.Set.add acc p
  in
  let rec loop fn acc =
    match Path.Table.find_all t.packages fn with
    | [] -> loop_deps fn acc
    | pkgs ->
      if List.mem pkg ~set:pkgs then
        loop_deps fn acc
      else
        List.fold_left pkgs ~init:acc ~f:add_package
  and loop_deps fn acc =
    match Path.Table.find t.files fn with
    | None -> acc
    | Some (File_spec.T { rule = ir; _ }) ->
      if Internal_rule.Set.mem !rules_seen ir then
        acc
      else begin
        rules_seen := Internal_rule.Set.add !rules_seen ir;
        (* We know that at this point of execution, all the relevant
           ivars have been filled so the following calls to
           [X.peek_exn] cannot raise. *)
        let static_deps = Fiber.Once.peek_exn ir.static_deps in
        let static_action_deps = Static_deps.action_deps static_deps in
        let _act, dynamic_action_deps =
          Memo.peek_exn evaluate_action_and_dynamic_deps_def ir
        in
        let action_deps =
          Path.Set.union
            (Dep.Set.paths static_action_deps ~eval_pred)
            (Dep.Set.paths dynamic_action_deps ~eval_pred)
        in
        Path.Set.fold action_deps ~init:acc ~f:loop
      end
  in
  let open Build.O in
  Build.paths_for_rule files >>^ fun () ->
  (* We know that at this point of execution, all the relevant ivars
     have been filled *)
  Path.Set.fold files ~init:Package.Name.Set.empty ~f:loop_deps

(* +-----------------------------------------------------------------+
   | Adding rules to the system                                      |
   +-----------------------------------------------------------------+ *)

let rec add_build_dir_to_keep t ~dir =
  if not (Path.Set.mem t.build_dirs_to_keep dir) then begin
    t.build_dirs_to_keep <- Path.Set.add t.build_dirs_to_keep dir;
    Option.iter (Path.parent dir) ~f:(fun dir ->
      if not (Path.is_root dir) then
        add_build_dir_to_keep t ~dir)
  end

let get_collector t ~dir =
  match get_dir_status t ~dir with
  | Collecting_rules collector ->
    add_build_dir_to_keep t ~dir;
    collector
  | Failed_to_load -> raise Already_reported
  | Loaded _ | Forward _ ->
    Exn.code_error
      (if Path.is_in_source_tree dir then
         "Build_system.get_collector called on source directory"
       else if Path.equal dir Path.build_dir then
         "Build_system.get_collector called on build_dir"
       else if not (Path.is_managed dir) then
         "Build_system.get_collector called on external directory"
       else
         "Build_system.get_collector called on closed directory")
      [ "dir", Path.to_sexp dir
      ; "load_dir_stack", Sexp.Encoder.list Path.to_sexp t.load_dir_stack
      ]

let produce_rule_collection collector f =
  let thunk = Thunk_with_backtrace.create f in
  Dir_status.Rules_collector.forbid_freeze_until_thunk_is_forced collector thunk;
  Memo.Implicit_output.produce rule_collection_implicit_output (
    Appendable_list.singleton thunk)

let add_rule (rule : Build_interpret.Rule.t) =
  let t = t () in
  let rule =
    match t.prefix with
    | None -> rule
    | Some prefix -> { rule with build = Build.O.(>>>) prefix rule.build } in
  let collector = get_collector t ~dir:rule.dir in
  produce_rule_collection collector (fun () -> Dir_status.Rules_collector.add_rule collector rule)

let prefix_rules' t prefix ~f =
  let old_prefix = t.prefix in
  t.prefix <- prefix;
  protectx () ~f ~finally:(fun () -> t.prefix <- old_prefix)

let prefix_rules prefix ~f =
  let t = t () in
  begin match Build_interpret.targets prefix with
  | [] -> ()
  | targets ->
    Exn.code_error "Build_system.prefix_rules' prefix contains targets"
      ["targets", Path.Set.to_sexp (Build_interpret.Target.paths targets)]
  end;
  let prefix =
    match t.prefix with
    | None -> prefix
    | Some p -> Build.O.(>>>) p prefix
  in
  prefix_rules' t (Some prefix) ~f

let () =
  let f g =
    let dir = File_selector.dir g in
    let t = t () in
    Path.Set.filter (targets_of t ~dir) ~f:(File_selector.test g)
  in
  Memo.set_impl Pred.eval_def f

module Alias = struct
  include Alias0

  let modify_alias build_system t ~f =
    let collector =
      let dir = Alias.dir t in
      get_collector build_system ~dir in
    produce_rule_collection collector (fun () ->
      let name = Alias.name t in
      Dir_status.Rules_collector.modify_alias ~f collector name)

  let add_deps t ?dyn_deps deps =
    let build_system = get_build_system () in
    modify_alias build_system t ~f:(fun def ->
      Dir_status.Alias.add_deps def deps;
      Option.iter dyn_deps ~f:(Dir_status.Alias.add_dyn_deps def))

  let add_action t ~context ~env ~loc ?(locks=[]) ~stamp action =
    let build_system = get_build_system () in
    modify_alias build_system t ~f:(fun def ->
      Dir_status.Alias.add_action def
        { stamp = Digest.string (Marshal.to_string stamp [])
        ; action
        ; locks
        ; context
        ; loc
        ; env
        })
end

let targets_of ~dir = targets_of (t ()) ~dir
let load_dir ~dir = load_dir (t ()) ~dir

let is_target file =
  Path.Set.mem (targets_of ~dir:(Path.parent_exn file)) file

module Print_rules : sig
  val evaluate_rules
    :  recursive:bool
    -> request:(unit, unit) Build.t
    -> Rule.t list Fiber.t
end = struct
  let rules_for_files rules deps =
    Dep.Set.paths deps ~eval_pred
    |> Path.Set.fold ~init:Rule.Set.empty ~f:(fun path acc ->
      match Path.Map.find rules path with
      | None -> acc
      | Some rule -> Rule.Set.add acc rule)
    |> Rule.Set.to_list

  let entry_point t ~f =
    (match t.load_dir_stack with
     | [] ->
       ()
     | stack ->
       Exn.code_error
         "Build_system.entry_point: called inside the rule generator callback"
         ["stack", Sexp.Encoder.list Path.to_sexp stack]
    );
    f ()

  let evaluate_rules ~recursive ~request =
    let t = t () in
    entry_point t ~f:(fun () ->
      let rules = ref Internal_rule.Id.Map.empty in
      let rec run_rule (rule : Internal_rule.t) =
        if Internal_rule.Id.Map.mem !rules rule.id then
          Fiber.return ()
        else begin
          let* (action, deps) = evaluate_rule rule in
          let rule =
            { Rule.
              id = rule.id
            ; dir = rule.dir
            ; deps
            ; targets = rule.targets
            ; context = rule.context
            ; action
            } in
          rules := Internal_rule.Id.Map.add !rules rule.id rule;
          if recursive then
            Dep.Set.parallel_iter_files deps ~f:proc_rule ~eval_pred
          else
            Fiber.return ()
        end
      and proc_rule dep =
        get_file_spec_other t dep >>= function
        | None -> Fiber.return () (* external files *)
        | Some (File_spec.T file) -> run_rule file.rule
      in
      let rule_shim = shim_of_build_goal t request in
      let* (_act, goal) = evaluate_rule rule_shim in
      let+ () = Dep.Set.parallel_iter_files goal ~f:proc_rule ~eval_pred in
      let rules =
        Internal_rule.Id.Map.fold !rules ~init:Path.Map.empty
          ~f:(fun (r : Rule.t) acc ->
            Path.Set.fold r.targets ~init:acc ~f:(fun fn acc ->
              Path.Map.add acc fn r)) in
      match
        Rule.Id.Top_closure.top_closure
          (rules_for_files rules goal)
          ~key:(fun (r : Rule.t) -> r.id)
          ~deps:(fun (r : Rule.t) -> rules_for_files rules r.deps)
      with
      | Ok l -> l
      | Error cycle ->
        die "dependency cycle detected:\n   %s"
          (List.map cycle ~f:(fun rule ->
             Path.to_string (Path.Set.choose_exn rule.Rule.targets))
           |> String.concat ~sep:"\n-> "))
end

include Print_rules

module All_lib_deps : sig
  val all_lib_deps
    :  request:(unit, unit) Build.t
    -> Lib_deps_info.t Path.Map.t String.Map.t Fiber.t
end = struct
  let static_deps_of_request request =
    Static_deps.paths @@
    Build_interpret.static_deps request ~all_targets:targets_of

  let rules_for_files t paths =
    Path.Set.fold paths ~init:[] ~f:(fun path acc ->
      if Path.is_in_build_dir path then
        load_dir ~dir:(Path.parent_exn path);
      match Path.Table.find t.files path with
      | None -> acc
      | Some (File_spec.T { rule; _ }) -> rule :: acc)
    |> Internal_rule.Set.of_list
    |> Internal_rule.Set.to_list

  let rules_for_targets t targets =
    Internal_rule.Id.Top_closure_f.top_closure
      (rules_for_files t targets)
      ~key:(fun (r : Internal_rule.t) -> r.id)
      ~deps:(fun (r : Internal_rule.t) ->
        Fiber.Once.get r.static_deps
        >>| Static_deps.paths ~eval_pred
        >>| rules_for_files t)
    >>| function
    | Ok l -> l
    | Error cycle ->
      die "dependency cycle detected:\n   %s"
        (List.map cycle ~f:(fun rule ->
           Path.to_string (Path.Set.choose_exn rule.Internal_rule.targets))
         |> String.concat ~sep:"\n-> ")

  let all_lib_deps ~request =
    let t = t () in
    let targets = static_deps_of_request request ~eval_pred in
    let* rules= rules_for_targets t targets in
    let+ lib_deps =
      Fiber.parallel_map rules ~f:(fun rule ->
        let+ deps = Internal_rule.lib_deps rule in
        (rule, deps))
    in
    List.fold_left lib_deps ~init:[]
      ~f:(fun acc (rule, deps) ->
        if Lib_name.Map.is_empty deps then
          acc
        else
          match Path.extract_build_context rule.Internal_rule.dir with
          | None -> acc
          | Some (context, p) -> ((context, (p, deps)) :: acc))
    |> String.Map.of_list_multi
    |> String.Map.filteri ~f:(fun ctx _ -> String.Map.mem t.contexts ctx)
    |> String.Map.map ~f:(Path.Map.of_list_reduce ~f:Lib_deps_info.merge)
end

include All_lib_deps
