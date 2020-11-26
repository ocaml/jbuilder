open! Stdune
open Import
open Fiber.O

let () = Hooks.End_of_build.always Memo.reset

module Fs : sig
  val mkdir_p : Path.Build.t -> unit

  (** Creates directory if inside build path, otherwise asserts that directory
      exists. *)
  val mkdir_p_or_check_exists : loc:Loc.t -> Path.t -> unit

  val assert_exists : loc:Loc.t -> Path.t -> unit
end = struct
  let mkdir_p_def =
    Memo.create "mkdir_p" ~doc:"mkdir_p"
      ~input:(module Path.Build)
      ~output:(Simple (module Unit))
      ~visibility:Hidden Sync
      (fun p -> Path.mkdir_p (Path.build p))

  let mkdir_p = Memo.exec mkdir_p_def

  let assert_exists_def =
    Memo.create "assert_path_exists" ~doc:"Path.exists"
      ~input:(module Path)
      ~output:(Simple (module Bool))
      ~visibility:Hidden Sync Path.exists

  let assert_exists ~loc path =
    if not (Memo.exec assert_exists_def path) then
      User_error.raise ~loc
        [ Pp.textf "%S does not exist" (Path.to_string_maybe_quoted path) ]

  let mkdir_p_or_check_exists ~loc path =
    match Path.as_in_build_dir path with
    | None -> assert_exists ~loc path
    | Some path -> mkdir_p path
end

module Promoted_to_delete : sig
  val add : Path.t -> unit

  val load : unit -> Path.Set.t
end = struct
  module P = Persistent.Make (struct
    type t = Path.Set.t

    let name = "PROMOTED-TO-DELETE"

    let version = 1
  end)

  let db = ref Path.Set.empty

  let fn = Path.relative Path.build_dir ".to-delete-in-source-tree"

  let needs_dumping = ref false

  let add p =
    if not (Path.Set.mem !db p) then (
      needs_dumping := true;
      db := Path.Set.add !db p
    )

  let load () = Option.value ~default:Path.Set.empty (P.load fn)

  let dump () =
    if !needs_dumping && Path.build_dir_exists () then (
      needs_dumping := false;
      load () |> Path.Set.union !db |> P.dump fn
    )

  let () = Hooks.End_of_build.always dump
end

let files_in_source_tree_to_delete () = Promoted_to_delete.load ()

module Alias0 = struct
  include Alias

  let dep t = Build.path (Path.build (stamp_file t))

  let dep_multi_contexts ~dir ~name ~contexts =
    ignore (File_tree.find_dir_specified_on_command_line ~dir);
    let context_to_stamp_file ctx =
      let ctx_dir = Context_name.build_dir ctx in
      let dir = Path.Build.(append_source ctx_dir dir) in
      Path.build (stamp_file (make ~dir name))
    in
    Build.paths (List.map contexts ~f:context_to_stamp_file)

  open Build.O

  let dep_rec_internal ~name ~dir ~ctx_dir =
    let f dir acc =
      let path = Path.Build.append_source ctx_dir (File_tree.Dir.path dir) in
      let fn = stamp_file (make ~dir:path name) in
      let fn = Path.build fn in
      Build.map2 ~f:( && ) acc
        (Build.if_file_exists fn
           ~then_:(Build.path fn >>> Build.return false)
           ~else_:(Build.return true))
    in
    File_tree.Dir.fold dir ~traverse:Sub_dirs.Status.Set.normal_only
      ~init:(Build.return true) ~f

  let dep_rec t ~loc =
    let ctx_dir, src_dir =
      Path.Build.extract_build_context_dir_exn (Alias.dir t)
    in
    match File_tree.find_dir src_dir with
    | None ->
      Build.fail
        { fail =
            (fun () ->
              User_error.raise ~loc
                [ Pp.textf "Don't know about directory %s!"
                    (Path.Source.to_string_maybe_quoted src_dir)
                ])
        }
    | Some dir ->
      let name = Alias.name t in
      let+ is_empty = dep_rec_internal ~name ~dir ~ctx_dir in
      if is_empty && not (is_standard name) then
        User_error.raise ~loc
          [ Pp.text "This alias is empty."
          ; Pp.textf "Alias %S is not defined in %s or any of its descendants."
              (Alias.Name.to_string name)
              (Path.Source.to_string_maybe_quoted src_dir)
          ]

  let dep_rec_multi_contexts ~dir:src_dir ~name ~contexts =
    let open Build.O in
    let dir = File_tree.find_dir_specified_on_command_line ~dir:src_dir in
    let+ is_empty_list =
      Build.all
        (List.map contexts ~f:(fun ctx ->
             let ctx_dir = Context_name.build_dir ctx in
             dep_rec_internal ~name ~dir ~ctx_dir))
    in
    let is_empty = List.for_all is_empty_list ~f:Fun.id in
    if is_empty && not (is_standard name) then
      User_error.raise
        [ Pp.textf "Alias %S specified on the command line is empty."
            (Alias.Name.to_string name)
        ; Pp.textf "It is not defined in %s or any of its descendants."
            (Path.Source.to_string_maybe_quoted src_dir)
        ]

  let package_install ~(context : Build_context.t) ~(pkg : Package.t) =
    let dir =
      let dir = Package.dir pkg in
      Path.Build.append_source context.build_dir dir
    in
    let name = Package.name pkg in
    sprintf ".%s-files" (Package.Name.to_string name)
    |> Alias.Name.of_string |> make ~dir
end

module Loaded = struct
  type build =
    { allowed_subdirs : Path.Unspecified.w Dir_set.t
    ; rules_produced : Rules.t
    ; rules_here : Rule.t Path.Build.Map.t
    ; rules_of_alias_dir : Rule.t Path.Build.Map.t
    }

  type t =
    | Non_build of Path.Set.t
    | Build of build

  let no_rules ~allowed_subdirs =
    Build
      { allowed_subdirs
      ; rules_produced = Rules.empty
      ; rules_here = Path.Build.Map.empty
      ; rules_of_alias_dir = Path.Build.Map.empty
      }
end

module Dir_triage = struct
  type t =
    | Known of Loaded.t
    | Alias_dir_of of Path.Build.t
    | Need_step2
end

(* Stores information needed to determine if rule need to be reexecuted. *)
module Trace_db : sig
  module Entry : sig
    type t =
      { rule_digest : Digest.t
      ; dynamic_deps_stages : (Action_exec.Dynamic_dep.Set.t * Digest.t) list
      ; targets_digest : Digest.t
      }
  end

  val get : Path.t -> Entry.t option

  val set : Path.t -> Entry.t -> unit
end = struct
  module Entry = struct
    type t =
      { rule_digest : Digest.t
      ; dynamic_deps_stages : (Action_exec.Dynamic_dep.Set.t * Digest.t) list
      ; targets_digest : Digest.t
      }
  end

  (* Keyed by the first target of the rule. *)
  type t = Entry.t Path.Table.t

  let file = Path.relative Path.build_dir ".db"

  module P = Persistent.Make (struct
    type nonrec t = t

    let name = "INCREMENTAL-DB"

    let version = 4
  end)

  let needs_dumping = ref false

  let t =
    (* This [lazy] is safe: it does not call any memoized functions. *)
    lazy
      ( match P.load file with
      | Some t -> t
      (* This mutable table is safe: it's only used by [execute_rule_impl] to
         decide whether to rebuild a rule or not; [execute_rule_impl] ensures
         that the targets are produced deterministically. *)
      | None -> Path.Table.create 1024 )

  let dump () =
    if !needs_dumping && Path.build_dir_exists () then (
      needs_dumping := false;
      P.dump file (Lazy.force t)
    )

  let () = Hooks.End_of_build.always dump

  let get path =
    let t = Lazy.force t in
    Path.Table.find t path

  let set path e =
    let t = Lazy.force t in
    needs_dumping := true;
    Path.Table.set t path e
end

module Subdir_set = struct
  type t =
    | All
    | These of String.Set.t

  let to_dir_set = function
    | All -> Dir_set.universal
    | These s ->
      String.Set.to_list s
      |> List.map ~f:Path.Local.of_string
      |> Dir_set.of_list

  let of_dir_set d =
    match Dir_set.toplevel_subdirs d with
    | Infinite -> All
    | Finite s -> These s

  let of_list l = These (String.Set.of_list l)

  let empty = These String.Set.empty

  let mem t dir =
    match t with
    | All -> true
    | These t -> String.Set.mem t dir

  let union a b =
    match (a, b) with
    | All, _
    | _, All ->
      All
    | These a, These b -> These (String.Set.union a b)

  let union_all = List.fold_left ~init:empty ~f:union
end

type extra_sub_directories_to_keep = Subdir_set.t

module Action_and_deps = struct
  type t = Action.t * Dep.Set.t

  let to_dyn (action, deps) =
    let open Dyn.Encoder in
    let action =
      Action.for_shell action |> Action.For_shell.encode |> Dune_lang.to_dyn
    in
    record [ ("action", action); ("deps", Dep.Set.to_dyn deps) ]
end

module Rule_fn = struct
  let loc_decl = Fdecl.create Dyn.Encoder.opaque

  let loc () = Fdecl.get loc_decl ()
end

module Context_or_install = struct
  type t =
    | Install of Context_name.t
    | Context of Context_name.t

  let to_dyn = function
    | Install ctx -> Dyn.List [ Dyn.String "install"; Context_name.to_dyn ctx ]
    | Context s -> Context_name.to_dyn s
end

type caching =
  { cache : (module Cache.Caching)
  ; check_probability : float
  }

type t =
  { contexts : Build_context.t Context_name.Map.t
  ; init_rules : Rules.t Fdecl.t
  ; gen_rules :
      (   Context_or_install.t
       -> (dir:Path.Build.t -> string list -> extra_sub_directories_to_keep)
          option)
      Fdecl.t
  ; (* Package files are part of *)
    packages : (Path.Build.t -> Package.Id.Set.t) Fdecl.t
  ; mutable caching : caching option
  ; sandboxing_preference : Sandbox_mode.t list
  ; mutable rule_done : int
  ; mutable rule_total : int
  ; vcs : Vcs.t list Fdecl.t
  }

let t = ref None

let set x =
  match !t with
  | None -> t := Some x
  | Some _ -> Code_error.raise "build system already initialized" []

let get_build_system () =
  match !t with
  | Some t -> t
  | None -> Code_error.raise "build system not yet initialized" []

let reset () = t := None

let t = get_build_system

let contexts () = (t ()).contexts

let pp_paths set =
  Pp.enumerate (Path.Set.to_list set) ~f:(fun p ->
      Path.drop_optional_build_context p
      |> Path.to_string_maybe_quoted |> Pp.verbatim)

let set_rule_generators ~init ~gen_rules =
  let t = t () in
  let (), init_rules = Rules.collect init in
  Fdecl.set t.init_rules init_rules;
  Fdecl.set t.gen_rules gen_rules

let set_vcs vcs =
  let open Fiber.O in
  let t = t () in
  let () = Fdecl.set t.vcs vcs in
  match t.caching with
  | None -> Fiber.return ()
  | Some ({ cache = (module Caching); _ } as caching) ->
    let+ caching =
      let+ with_repositories =
        let f ({ Vcs.root; _ } as vcs) =
          let+ commit = Vcs.commit_id vcs in
          { Cache.directory = Path.to_absolute_filename root
          ; remote = "" (* FIXME: fill or drop from the protocol *)
          ; commit
          }
        in
        let+ repositories = Fiber.parallel_map ~f (Fdecl.get t.vcs) in
        Caching.Cache.with_repositories Caching.cache repositories
      in
      match with_repositories with
      | Result.Ok cache ->
        let cache =
          ( module struct
            let cache = cache

            module Cache = Caching.Cache
          end : Cache.Caching )
        in
        Some { caching with cache }
      | Result.Error e ->
        User_warning.emit
          [ Pp.textf "Unable to set cache repositiories, disabling cache: %s" e
          ];
        None
    in
    t.caching <- caching

let get_vcs () =
  let t = t () in
  Fdecl.get t.vcs

let get_cache () =
  let t = t () in
  t.caching

let get_dir_triage t ~dir =
  match Dpath.analyse_dir dir with
  | Source dir ->
    Dir_triage.Known
      (Non_build (Path.set_of_source_paths (File_tree.files_of dir)))
  | External _ ->
    Dir_triage.Known
      (Non_build
         ( match Path.readdir_unsorted dir with
         | Error Unix.ENOENT -> Path.Set.empty
         | Error m ->
           User_warning.emit
             [ Pp.textf "Unable to read %s" (Path.to_string_maybe_quoted dir)
             ; Pp.textf "Reason: %s" (Unix.error_message m)
             ];
           Path.Set.empty
         | Ok filenames -> Path.Set.of_listing ~dir ~filenames ))
  | Build (Regular Root) ->
    let allowed_subdirs =
      Subdir_set.to_dir_set
        (Subdir_set.of_list
           ( ( [ Dpath.Build.alias_dir; Dpath.Build.install_dir ]
             |> List.map ~f:Path.Build.basename )
           @ ( Context_name.Map.keys t.contexts
             |> List.map ~f:Context_name.to_string ) ))
    in
    Dir_triage.Known (Loaded.no_rules ~allowed_subdirs)
  | Build (Install Root) ->
    let allowed_subdirs =
      Context_name.Map.keys t.contexts
      |> List.map ~f:Context_name.to_string
      |> Subdir_set.of_list |> Subdir_set.to_dir_set
    in
    Dir_triage.Known (Loaded.no_rules ~allowed_subdirs)
  | Build (Alias p) ->
    let build_dir = Dpath.Target_dir.build_dir p in
    Alias_dir_of build_dir
  | Build (Invalid _) ->
    Dir_triage.Known (Loaded.no_rules ~allowed_subdirs:Dir_set.empty)
  | Build (Install (With_context _))
  | Build (Regular (With_context _)) ->
    Need_step2

let describe_rule (rule : Rule.t) =
  match rule.info with
  | From_dune_file { start; _ } ->
    start.pos_fname ^ ":" ^ string_of_int start.pos_lnum
  | Internal -> "<internal location>"
  | Source_file_copy -> "file present in source tree"

let report_rule_src_dir_conflict dir fn (rule : Rule.t) =
  let loc =
    match rule.info with
    | From_dune_file loc -> loc
    | Internal
    | Source_file_copy ->
      let dir =
        match Path.Build.drop_build_context dir with
        | None -> Path.build dir
        | Some s -> Path.source s
      in
      Loc.in_dir dir
  in
  User_error.raise ~loc
    [ Pp.textf "Rule has a target %s" (Path.Build.to_string_maybe_quoted fn)
    ; Pp.textf "This conflicts with a source directory in the same directory"
    ]

let report_rule_conflict fn (rule' : Rule.t) (rule : Rule.t) =
  let fn = Path.build fn in
  User_error.raise
    [ Pp.textf "Multiple rules generated for %s:"
        (Path.to_string_maybe_quoted fn)
    ; Pp.textf "- %s" (describe_rule rule')
    ; Pp.textf "- %s" (describe_rule rule)
    ]
    ~hints:
      ( match (rule.info, rule'.info) with
      | Source_file_copy, _
      | _, Source_file_copy ->
        [ Pp.textf "rm -f %s"
            (Path.to_string_maybe_quoted (Path.drop_optional_build_context fn))
        ]
      | _ -> [] )

(* This contains the targets of the actions that are being executed. On exit, we
   need to delete them as they might contain garbage *)
let pending_targets = ref Path.Build.Set.empty

let () =
  Hooks.End_of_build.always (fun () ->
      let fns = !pending_targets in
      pending_targets := Path.Build.Set.empty;
      Path.Build.Set.iter fns ~f:(fun p -> Path.unlink_no_err (Path.build p)))

let compute_targets_digest targets =
  match
    List.map targets ~f:(fun target -> Cached_digest.file (Path.build target))
  with
  | l -> Some (Digest.generic l)
  | exception (Unix.Unix_error _ | Sys_error _) -> None

let compute_targets_digest_or_raise_error ~loc targets =
  let remove_write_permissions =
    (* Remove write permissions on targets. A first theoretical reason is that
       the build process should be a computational graph and targets should not
       change state once built. A very practical reason is that enabling the
       cache will remove write permission because of hardlink sharing anyway, so
       always removing them enables to catch mistakes earlier. *)
    (* FIXME: searching the dune version for each single target seems way
       suboptimal. This information could probably be stored in rules directly. *)
    if targets = [] then
      false
    else
      let _, src_dir =
        Path.Build.extract_build_context_dir_exn (List.hd targets)
      in
      let dir = File_tree.nearest_dir src_dir in
      let version = File_tree.Dir.project dir |> Dune_project.dune_version in
      version >= (2, 4)
  in
  let refresh =
    if remove_write_permissions then
      Cached_digest.refresh_and_chmod
    else
      Cached_digest.refresh
  in
  let good, bad =
    List.partition_map targets ~f:(fun target ->
        let fn = Path.build target in
        match refresh fn with
        | digest -> Left (target, digest)
        | exception (Unix.Unix_error _ | Sys_error _) -> Right fn)
  in
  match bad with
  | [] -> (good, Digest.generic (List.map ~f:snd good))
  | missing ->
    User_error.raise ~loc
      [ Pp.textf "Rule failed to generate the following targets:"
      ; pp_paths (Path.Set.of_list missing)
      ]

let sandbox_dir = Path.Build.relative Path.Build.root ".sandbox"

(* This mutable table is safe: it merely maps paths to lazily created mutexes. *)
let locks : (Path.t, Fiber.Mutex.t) Table.t = Table.create (module Path) 32

let rec with_locks mutexes ~f =
  match mutexes with
  | [] -> f ()
  | m :: mutexes ->
    Fiber.Mutex.with_lock
      (Table.find_or_add locks m ~f:(fun _ -> Fiber.Mutex.create ()))
      (fun () -> with_locks mutexes ~f)

let remove_old_artifacts ~dir ~rules_here ~(subdirs_to_keep : Subdir_set.t) =
  match Path.readdir_unsorted_with_kinds (Path.build dir) with
  | exception _ -> ()
  | Error _ -> ()
  | Ok files ->
    List.iter files ~f:(fun (fn, kind) ->
        let path = Path.Build.relative dir fn in
        let path_is_a_target = Path.Build.Map.mem rules_here path in
        if path_is_a_target then
          ()
        else
          match kind with
          | Unix.S_DIR -> (
            match subdirs_to_keep with
            | All -> ()
            | These set ->
              if String.Set.mem set fn then
                ()
              else
                Path.rm_rf (Path.build path) )
          | _ -> Path.unlink (Path.build path))

let no_rule_found t ~rules ~loc fn =
  let fail fn ~loc =
    let hint =
      if Path.Build.Map.is_empty rules then
        [ Pp.text "In fact, there are no rules defined in this directory" ]
      else
        [ Pp.text "May I interest you in one of the following targets instead?"
        ; rules |> Path.Build.Map.keys
          |> Pp.enumerate ~f:(fun t ->
                 Pp.text (Path.Build.to_string_maybe_quoted t))
        ]
    in
    User_error.raise ?loc
      ([ Pp.textf "No rule found for %s" (Dpath.describe_target fn) ] @ hint)
  in
  let hints ctx =
    let candidates =
      Context_name.Map.keys t.contexts |> List.map ~f:Context_name.to_string
    in
    User_message.did_you_mean (Context_name.to_string ctx) ~candidates
  in
  match Dpath.analyse_target fn with
  | Other _ -> fail fn ~loc
  | Regular (ctx, _) ->
    if Context_name.Map.mem t.contexts ctx then
      fail fn ~loc
    else
      User_error.raise
        [ Pp.textf "Trying to build %s but build context %s doesn't exist."
            (Path.Build.to_string_maybe_quoted fn)
            (Context_name.to_string ctx)
        ]
        ~hints:(hints ctx)
  | Install (ctx, _) ->
    if Context_name.Map.mem t.contexts ctx then
      fail fn ~loc
    else
      User_error.raise
        [ Pp.textf
            "Trying to build %s for install but build context %s doesn't exist."
            (Path.Build.to_string_maybe_quoted fn)
            (Context_name.to_string ctx)
        ]
        ~hints:(hints ctx)
  | Alias (ctx, fn') ->
    if Context_name.Map.mem t.contexts ctx then
      fail fn ~loc
    else
      let fn =
        Path.append_source (Path.build (Context_name.build_dir ctx)) fn'
      in
      User_error.raise
        [ Pp.textf
            "Trying to build alias %s but build context %s doesn't exist."
            (Path.to_string_maybe_quoted fn)
            (Context_name.to_string ctx)
        ]
        ~hints:(hints ctx)

(* +-------------------- Adding rules to the system --------------------+ *)

module rec Load_rules : sig
  val load_dir : dir:Path.t -> Loaded.t

  val file_exists : Path.t -> bool

  val targets_of : dir:Path.t -> Path.Set.t
end = struct
  open Load_rules

  let create_copy_rules ~ctx_dir ~non_target_source_files =
    Path.Source.Set.to_list non_target_source_files
    |> List.map ~f:(fun path ->
           let ctx_path = Path.Build.append_source ctx_dir path in
           let build = Build.copy ~src:(Path.source path) ~dst:ctx_path in
           Rule.make
           (* There's an [assert false] in [prepare_managed_paths] that blows up
              if we try to sandbox this. *)
             ~sandbox:Sandbox_config.no_sandboxing build ~context:None ~env:None
             ~info:Source_file_copy)

  let compile_rules ~dir ~source_dirs rules =
    List.concat_map rules ~f:(fun rule ->
        assert (Path.Build.( = ) dir rule.Rule.dir);
        Path.Build.Set.to_list rule.action.targets
        |> List.map ~f:(fun target ->
               if String.Set.mem source_dirs (Path.Build.basename target) then
                 report_rule_src_dir_conflict dir target rule
               else
                 (target, rule)))
    |> Path.Build.Map.of_list_reducei ~f:report_rule_conflict

  (* Here we are doing a O(log |S|) lookup in a set S of files in the build
     directory [dir]. We could memoize these lookups, but it doesn't seem to be
     worth it, since we're unlikely to perform exactly the same lookup many
     times. As far as I can tell, each lookup will be done twice: when computing
     static dependencies of a [Build.t] with [Build.static_deps] and when
     executing the very same [Build.t] with [Build.exec] -- the results of both
     [Build.static_deps] and [Build.exec] are cached. *)
  let file_exists fn =
    let dir = Path.parent_exn fn in
    Path.Set.mem (targets_of ~dir) fn

  let targets_of ~dir =
    match load_dir ~dir with
    | Non_build targets -> targets
    | Build { rules_here; _ } ->
      Path.Build.Map.keys rules_here |> Path.Set.of_list_map ~f:Path.build

  let compute_alias_rules ~context_name ~(collected : Rules.Dir_rules.ready)
      ~dir ~sub_dir =
    let alias_dir =
      let context_name = Context_name.to_string context_name in
      Path.Build.append_source
        (Path.Build.relative Dpath.Build.alias_dir context_name)
        sub_dir
    in
    let alias_rules =
      let open Build.O in
      let aliases = collected.aliases in
      let aliases =
        if Alias.Name.Map.mem aliases Alias.Name.default then
          aliases
        else
          match Path.Build.extract_build_context_dir dir with
          | None -> aliases
          | Some (ctx_dir, src_dir) -> (
            match File_tree.find_dir src_dir with
            | None -> aliases
            | Some dir ->
              let default_alias =
                let dune_version =
                  File_tree.Dir.project dir |> Dune_project.dune_version
                in
                if dune_version >= (2, 0) then
                  Alias.Name.all
                else
                  Alias.Name.install
              in
              Alias.Name.Map.set aliases Alias.Name.default
                { deps = Path.Set.empty
                ; dyn_deps =
                    (let+ _ =
                       Alias0.dep_rec_internal ~name:default_alias ~dir ~ctx_dir
                     in
                     Path.Set.empty)
                ; actions = Appendable_list.empty
                } )
      in
      Alias.Name.Map.foldi aliases ~init:[]
        ~f:(fun
             name
             { Rules.Dir_rules.Alias_spec.deps; dyn_deps; actions }
             rules
           ->
          let base_path =
            Path.Build.relative alias_dir (Alias.Name.to_string name)
          in
          let rules, action_stamp_files =
            List.fold_left (Appendable_list.to_list actions)
              ~init:(rules, Path.Set.empty)
              ~f:(fun
                   (rules, action_stamp_files)
                   { Rules.Dir_rules.stamp; action; locks; context; loc; env }
                 ->
                let path =
                  Path.Build.extend_basename base_path
                    ~suffix:("-" ^ Digest.to_string stamp)
                in
                let rule =
                  Rule.make ~locks ~context:(Some context) ~env
                    ~info:(Rule.Info.of_loc_opt loc)
                    (Build.progn [ action; Build.create_file path ])
                in
                ( rule :: rules
                , Path.Set.add action_stamp_files (Path.build path) ))
          in
          let deps = Path.Set.union deps action_stamp_files in
          let path =
            Path.Build.extend_basename base_path ~suffix:Alias0.suffix
          in
          Rule.make ~context:None ~env:None
            (let action =
               let+ () = Build.path_set deps
               and+ dyn_deps = Build.dyn_path_set_reuse dyn_deps in
               let deps = Path.Set.union deps dyn_deps in
               Action.with_stdout_to path
                 (Action.digest_files (Path.Set.to_list deps))
             in
             Build.with_targets ~targets:[ path ] action)
          :: rules)
    in
    fun ~subdirs_to_keep ->
      let rules_here =
        compile_rules ~dir:alias_dir ~source_dirs:String.Set.empty alias_rules
      in
      remove_old_artifacts ~rules_here ~dir:alias_dir ~subdirs_to_keep;
      rules_here

  let filter_out_fallback_rules ~to_copy rules =
    List.filter rules ~f:(fun (rule : Rule.t) ->
        match rule.mode with
        | Standard
        | Promote _
        | Ignore_source_files ->
          true
        | Fallback ->
          let source_files_for_targtes =
            (* All targets are in [dir] and we know it correspond to a directory
               of a build context since there are source files to copy, so this
               call can't fail. *)
            Path.Build.Set.to_list rule.action.targets
            |> Path.Source.Set.of_list_map ~f:Path.Build.drop_build_context_exn
          in
          if Path.Source.Set.is_subset source_files_for_targtes ~of_:to_copy
          then
            (* All targets are present *)
            false
          else if
            Path.Source.Set.is_empty
              (Path.Source.Set.inter source_files_for_targtes to_copy)
          then
            (* No target is present *)
            true
          else
            let absent_targets =
              Path.Source.Set.diff source_files_for_targtes to_copy
            in
            let present_targets =
              Path.Source.Set.diff source_files_for_targtes absent_targets
            in
            User_error.raise ~loc:(Rule.loc rule)
              [ Pp.text
                  "Some of the targets of this fallback rule are present in \
                   the source tree, and some are not. This is not allowed. \
                   Either none of the targets must be present in the source \
                   tree, either they must all be."
              ; Pp.nop
              ; Pp.text "The following targets are present:"
              ; pp_paths (Path.set_of_source_paths present_targets)
              ; Pp.nop
              ; Pp.text "The following targets are not:"
              ; pp_paths (Path.set_of_source_paths absent_targets)
              ])

  (** A directory is only allowed to be generated if its parent knows about it.
      This restriction is necessary to prevent stale artifact deletion from
      removing that directory.

      This module encodes that restriction. *)
  module Generated_directory_restrictions : sig
    type restriction =
      | Unrestricted
      | Restricted of Path.Unspecified.w Dir_set.t Memo.Lazy.t

    (** Used by the child to ask about the restrictions placed by the parent. *)
    val allowed_by_parent : dir:Path.Build.t -> restriction
  end = struct
    type restriction =
      | Unrestricted
      | Restricted of Path.Unspecified.w Dir_set.t Memo.Lazy.t

    let corresponding_source_dir ~dir =
      match Dpath.analyse_target dir with
      | Install _
      | Alias _
      | Other _ ->
        None
      | Regular (_ctx, sub_dir) -> File_tree.find_dir sub_dir

    let source_subdirs_of_build_dir ~dir =
      match corresponding_source_dir ~dir with
      | None -> String.Set.empty
      | Some dir -> File_tree.Dir.sub_dir_names dir

    let allowed_dirs ~dir ~subdir : restriction =
      if String.Set.mem (source_subdirs_of_build_dir ~dir) subdir then
        Unrestricted
      else
        Restricted
          (Memo.Lazy.create (fun () ->
               match load_dir ~dir:(Path.build dir) with
               | Non_build _ -> Dir_set.just_the_root
               | Build { allowed_subdirs; _ } ->
                 Dir_set.descend allowed_subdirs subdir))

    let allowed_by_parent ~dir =
      allowed_dirs
        ~dir:(Path.Build.parent_exn dir)
        ~subdir:(Path.Build.basename dir)
  end

  let load_dir_step2_exn t ~dir =
    let context_name, sub_dir =
      match Dpath.analyse_path dir with
      | Build (Install (ctx, path)) -> (Context_or_install.Install ctx, path)
      | Build (Regular (ctx, path)) -> (Context_or_install.Context ctx, path)
      | Build (Alias _)
      | Build (Other _)
      | Source _
      | External _ ->
        Code_error.raise "[load_dir_step2_exn] was called on a strange path"
          [ ("path", Path.to_dyn dir) ]
    in
    (* the above check makes this safe *)
    let dir = Path.as_in_build_dir_exn dir in
    (* Load all the rules *)
    let extra_subdirs_to_keep, rules_produced =
      let gen_rules =
        match (Fdecl.get t.gen_rules) context_name with
        | None ->
          Code_error.raise "[gen_rules] did not specify rules for the context"
            [ ("context_name", Context_or_install.to_dyn context_name) ]
        | Some rules -> rules
      in
      Rules.collect (fun () -> gen_rules ~dir (Path.Source.explode sub_dir))
    in
    let rules =
      let dir = Path.build dir in
      Rules.Dir_rules.union
        (Rules.find rules_produced dir)
        (Rules.find (Fdecl.get t.init_rules) dir)
    in
    let collected = Rules.Dir_rules.consume rules in
    let rules = collected.rules in
    let alias_rules =
      match context_name with
      | Context context_name ->
        Some (compute_alias_rules ~context_name ~collected ~dir ~sub_dir)
      | Install _ -> None
    in
    let file_tree_dir =
      match context_name with
      | Install _ -> None
      | Context _ -> File_tree.find_dir sub_dir
    in
    (* Compute the set of targets and the set of source files that must not be
       copied *)
    let source_files_to_ignore =
      List.fold_left rules ~init:Path.Build.Set.empty
        ~f:(fun acc_ignored { Rule.action; mode; _ } ->
          let targets = action.targets in
          match mode with
          | Promote { only = None; _ }
          | Ignore_source_files ->
            Path.Build.Set.union targets acc_ignored
          | Promote { only = Some pred; _ } ->
            let to_ignore =
              Path.Build.Set.filter targets ~f:(fun target ->
                  Predicate_lang.Glob.exec pred
                    (Path.reach (Path.build target) ~from:(Path.build dir))
                    ~standard:Predicate_lang.any)
            in
            Path.Build.Set.union to_ignore acc_ignored
          | _ -> acc_ignored)
    in
    let source_files_to_ignore =
      Path.Build.Set.to_list source_files_to_ignore
      |> Path.Source.Set.of_list_map ~f:Path.Build.drop_build_context_exn
    in
    (* Take into account the source files *)
    let to_copy, source_dirs =
      match context_name with
      | Install _ -> (None, String.Set.empty)
      | Context context_name ->
        (* This condition is [true] because of [get_dir_status] *)
        assert (Context_name.Map.mem t.contexts context_name);
        let files, subdirs =
          match file_tree_dir with
          | None -> (Path.Source.Set.empty, String.Set.empty)
          | Some dir ->
            (File_tree.Dir.file_paths dir, File_tree.Dir.sub_dir_names dir)
        in
        let files = Path.Source.Set.diff files source_files_to_ignore in
        if Path.Source.Set.is_empty files then
          (None, subdirs)
        else
          let ctx_path = Context_name.build_dir context_name in
          (Some (ctx_path, files), subdirs)
    in
    let subdirs_to_keep =
      match extra_subdirs_to_keep with
      | All -> Subdir_set.All
      | These set -> These (String.Set.union source_dirs set)
    in
    (* Filter out fallback rules *)
    let rules =
      match to_copy with
      | None ->
        (* If there are no source files to copy, fallback rules are
           automatically kept *)
        rules
      | Some (_, to_copy) -> filter_out_fallback_rules ~to_copy rules
    in
    (* Compile the rules and cleanup stale artifacts *)
    let rules =
      ( match to_copy with
      | None -> []
      | Some (ctx_dir, source_files) ->
        create_copy_rules ~ctx_dir ~non_target_source_files:source_files )
      @ rules
    in
    let rules_here = compile_rules ~dir ~source_dirs rules in
    let allowed_by_parent =
      Generated_directory_restrictions.allowed_by_parent ~dir
    in
    ( match allowed_by_parent with
    | Unrestricted -> ()
    | Restricted restriction -> (
      match Path.Build.Map.find (Rules.to_map rules_produced) dir with
      | None -> ()
      | Some rules ->
        if Dir_set.here (Memo.Lazy.force restriction) then
          ()
        else
          Code_error.raise
            "Generated rules in a directory not allowed by the parent"
            [ ("dir", Path.Build.to_dyn dir)
            ; ("rules", Rules.Dir_rules.to_dyn rules)
            ] ) );
    let rules_generated_in =
      Rules.to_map rules_produced
      |> Path.Build.Map.keys
      |> List.filter_map ~f:(Path.Local_gen.descendant ~of_:dir)
      |> Dir_set.of_list
    in
    let allowed_granddescendants_of_parent =
      match allowed_by_parent with
      | Unrestricted ->
        (* In this case the parent isn't going to be able to create any
           generated granddescendant directories. (rules that attempt to do so
           may run into the [allowed_by_parent] check or will be simply ignored) *)
        Dir_set.empty
      | Restricted restriction -> Memo.Lazy.force restriction
    in
    let descendants_to_keep =
      Dir_set.union_all
        [ rules_generated_in
        ; Subdir_set.to_dir_set subdirs_to_keep
        ; allowed_granddescendants_of_parent
        ]
    in
    let subdirs_to_keep = Subdir_set.of_dir_set descendants_to_keep in
    remove_old_artifacts ~dir ~rules_here ~subdirs_to_keep;
    let alias_targets =
      Option.map ~f:(fun f -> f ~subdirs_to_keep) alias_rules
    in
    let rules_of_alias_dir =
      Option.value ~default:Path.Build.Map.empty alias_targets
    in
    Loaded.Build
      { allowed_subdirs = descendants_to_keep
      ; rules_produced
      ; rules_here
      ; rules_of_alias_dir
      }

  let load_dir_impl t ~dir : Loaded.t =
    match get_dir_triage t ~dir with
    | Known l -> l
    | Need_step2 -> load_dir_step2_exn t ~dir
    | Alias_dir_of dir' -> (
      match load_dir ~dir:(Path.build dir') with
      | Non_build _ -> Code_error.raise "Can only forward to a build dir" []
      | Build
          ( { rules_here = _
            ; rules_of_alias_dir
            ; rules_produced = _
            ; allowed_subdirs = _
            } as load ) ->
        Loaded.Build
          { load with
            rules_here = rules_of_alias_dir
          ; rules_of_alias_dir = Path.Build.Map.empty
          } )

  let load_dir =
    let load_dir_impl dir = load_dir_impl (t ()) ~dir in
    let memo =
      Memo.create_hidden "load-dir" ~doc:"load dir"
        ~input:(module Path)
        Sync load_dir_impl
    in
    fun ~dir -> Memo.exec memo dir
end

open Load_rules

let load_dir_and_get_buildable_targets ~dir =
  let loaded = load_dir ~dir in
  match loaded with
  | Non_build _ -> Path.Build.Map.empty
  | Build { rules_here; _ } -> rules_here

let get_rule fn =
  Option.bind (Path.as_in_build_dir fn) ~f:(fun fn ->
      let dir = Path.Build.parent_exn fn in
      match load_dir ~dir:(Path.build dir) with
      | Non_build _ -> assert false
      | Build { rules_here; _ } -> Path.Build.Map.find rules_here fn)

type rule_or_source =
  | Source
  | Rule of Rule.t

let get_rule_or_source t path =
  let dir = Path.parent_exn path in
  if Path.is_strict_descendant_of_build_dir dir then
    let rules = load_dir_and_get_buildable_targets ~dir in
    let path = Path.as_in_build_dir_exn path in
    match Path.Build.Map.find rules path with
    | Some rule -> Fiber.return (Rule rule)
    | None ->
      let loc = Rule_fn.loc () in
      no_rule_found t ~rules ~loc path
  else if Path.exists path then
    Fiber.return Source
  else
    let loc = Rule_fn.loc () in
    User_error.raise ?loc
      [ Pp.textf "File unavailable: %s" (Path.to_string_maybe_quoted path) ]

let all_targets t =
  let root = File_tree.root () in
  Context_name.Map.to_list t.contexts
  |> List.fold_left ~init:Path.Build.Set.empty ~f:(fun acc (_, ctx) ->
         File_tree.Dir.fold root ~traverse:Sub_dirs.Status.Set.all ~init:acc
           ~f:(fun dir acc ->
             match
               load_dir
                 ~dir:
                   (Path.build
                      (Path.Build.append_source ctx.Build_context.build_dir
                         (File_tree.Dir.path dir)))
             with
             | Non_build _ -> acc
             | Build { rules_here; rules_of_alias_dir; _ } ->
               List.fold_left ~init:acc ~f:Path.Build.Set.add
                 ( Path.Build.Map.keys rules_of_alias_dir
                 @ Path.Build.Map.keys rules_here )))

module type Rec = sig
  val build_file : Path.t -> unit Fiber.t

  val execute_rule : Rule.t -> unit Fiber.t

  module Pred : sig
    val eval : File_selector.t -> Path.Set.t

    val build : File_selector.t -> unit Fiber.t
  end
end

(* Separation between [Used_recursively] and [Exported] is necessary because at
   least one module in the recursive module group must be pure (i.e. only expose
   functions). *)
module rec Used_recursively : Rec = Exported

and Exported : sig
  include Rec

  module Build_request : sig
    type 'a t =
      | Non_memoized : 'a Build.t -> 'a t
      | Memoized : Rule.t -> Action.t t

    (** Evaluate a build request and return its static and dynamic dependencies.
        Note that the evaluation forces building of the static dependencies. *)
    val evaluate : 'a t -> ('a * Dep.Set.t) Fiber.t

    (** A hack exported because [package_deps] is not in a fiber. *)
    val peek_deps_exn : Rule.t -> Dep.Set.t

    (** Evaluate a build request and return its static and dynamic dependencies.
        Unlike [evaluate], this function also forces building of the dynamic
        dependencies. *)
    val evaluate_and_wait_for_dynamic_dependencies :
      'a t -> ('a * Dep.Set.t) Fiber.t
  end

  (** Exported to inspect memoization cycles. *)
  val build_file_memo : (Path.t, unit, Path.t -> unit Fiber.t) Memo.t
end = struct
  open Used_recursively

  let build_deps =
    Dep.Set.parallel_iter ~f:(function
      | Alias a -> build_file (Path.build (Alias.stamp_file a))
      | File f -> build_file f
      | File_selector g -> Pred.build g
      | Universe
      | Env _
      | Sandbox_config _ ->
        Fiber.return ())

  let eval_pred = Pred.eval

  let () = Build.set_file_system_accessors ~file_exists ~eval_pred

  module Build_request = struct
    type 'a t =
      | Non_memoized : 'a Build.t -> 'a t
      | Memoized : Rule.t -> Action.t t

    let build (type a) (t : a t) =
      match t with
      | Non_memoized build -> build
      | Memoized rule -> rule.action.build

    let static_deps (type a) (t : a t) = Build.static_deps (build t)

    let evaluate_and_discover_dynamic_deps_unmemoized t =
      let+ () = build_deps (static_deps t).rule_deps in
      Build.exec (build t)

    let memo =
      Memo.create "evaluate-rule-and-discover-dynamic-deps"
        ~output:(Simple (module Action_and_deps))
        ~doc:
          "Evaluate the build description of a rule and return the action and \
           dynamic dependencies of the rule."
        ~input:(module Rule)
        ~visibility:Hidden Async
        (fun rule ->
          evaluate_and_discover_dynamic_deps_unmemoized (Memoized rule))

    let evaluate_and_discover_dynamic_deps (type a) (t : a t) =
      match t with
      | Non_memoized _ -> evaluate_and_discover_dynamic_deps_unmemoized t
      | Memoized rule -> Memo.exec memo rule

    let evaluate t =
      let+ result, dynamic_deps = evaluate_and_discover_dynamic_deps t in
      (result, Dep.Set.union (static_deps t).action_deps dynamic_deps)

    let peek_deps_exn rule =
      let (_ : Action.t), dynamic_deps = Memo.peek_exn memo rule in
      Dep.Set.union (static_deps (Memoized rule)).action_deps dynamic_deps

    (* Same as the function just below, but with less parallelism. We keep this
       here only for documentation purposes as it is easier to read than the one
       below. The reader only has to check that the functions do the same thing. *)
    let _evaluate_and_wait_for_dynamic_dependencies t =
      let* result, deps = evaluate t in
      let+ () = build_deps deps in
      (result, deps)

    (* This function is equivalent to the function above but it starts building
       static dependencies before we know the final result and the dynamic
       dependencies. We do this to increase parallelism. *)
    let evaluate_and_wait_for_dynamic_dependencies (type a) (t : a t) =
      let static_deps = (static_deps t).action_deps in
      (* Build the static dependencies in parallel with evaluation of the result
         and dynamic dependencies. *)
      let* result, dynamic_deps =
        Fiber.fork_and_join_unit
          (fun () -> build_deps static_deps)
          (fun () -> evaluate_and_discover_dynamic_deps t)
      in
      build_deps dynamic_deps
      >>> Fiber.return (result, Dep.Set.union static_deps dynamic_deps)
  end

  let select_sandbox_mode (config : Sandbox_config.t) ~loc
      ~sandboxing_preference =
    let evaluate_sandboxing_preference preference =
      match Sandbox_mode.Set.mem config preference with
      | false -> None
      | true -> (
        match preference with
        | Some Symlink ->
          if Sandbox_mode.Set.mem config Sandbox_mode.copy then
            Some
              ( if Sys.win32 then
                Sandbox_mode.copy
              else
                Sandbox_mode.symlink )
          else
            User_error.raise ~loc
              [ Pp.text
                  "This rule requires sandboxing with symlinks, but that won't \
                   work on Windows."
              ]
        | _ -> Some preference )
    in
    match
      List.find_map sandboxing_preference ~f:evaluate_sandboxing_preference
    with
    | Some choice -> choice
    | None ->
      (* This is not trivial to reach because the user rules are checked at
         parse time and [sandboxing_preference] always includes all possible
         modes. However, it can still be reached if multiple sandbox config
         specs are combined into an unsatisfiable one. *)
      User_error.raise ~loc
        [ Pp.text
            "This rule forbids all sandboxing modes (but it also requires \
             sandboxing)"
        ]

  let start_rule t _rule = t.rule_total <- t.rule_total + 1

  (* Same as [rename] except that if the source doesn't exist we delete the
     destination *)
  let rename_optional_file ~src ~dst =
    let src = Path.Build.to_string src in
    let dst = Path.Build.to_string dst in
    match Unix.rename src dst with
    | () -> ()
    | exception Unix.Unix_error ((ENOENT | ENOTDIR), _, _) -> (
      match Unix.unlink dst with
      | exception Unix.Unix_error (ENOENT, _, _) -> ()
      | () -> () )

  let compute_rule_digest (rule : Rule.t) ~deps ~action ~sandbox_mode =
    let targets_as_list = Path.Build.Set.to_list rule.action.targets in
    let env = Rule.effective_env rule in
    let trace =
      ( Dep.Set.trace deps ~sandbox_mode ~env ~eval_pred
      , List.map targets_as_list ~f:(fun p -> Path.to_string (Path.build p))
      , Option.map rule.context ~f:(fun c -> c.name)
      , Action.for_shell action )
    in
    Digest.generic trace

  let compute_dependencies_digest deps ~sandbox_mode ~env ~eval_pred =
    Dep.Set.trace deps ~sandbox_mode ~env ~eval_pred
    |> (Digest.generic : Dep.Trace.t -> _)

  let execute_rule_impl rule =
    let t = t () in
    let { Rule.id = _; dir; env = _; context; mode; locks; action; info = _ } =
      rule
    in
    start_rule t rule;
    let targets = action.targets in
    let targets_as_list = Path.Build.Set.to_list targets in
    let head_target = List.hd targets_as_list in
    let* action, deps =
      Build_request.evaluate_and_wait_for_dynamic_dependencies (Memoized rule)
    in
    Stats.new_evaluated_rule ();
    Fs.mkdir_p dir;
    let env = Rule.effective_env rule in
    let loc = Rule.loc rule in
    let is_action_dynamic = Action.is_dynamic action in
    let sandbox_mode =
      match Action.is_useful_to_sandbox action with
      | Clearly_not ->
        let config = Dep.Set.sandbox_config deps in
        if Sandbox_config.mem config Sandbox_mode.none then
          Sandbox_mode.none
        else
          User_error.raise ~loc
            [ Pp.text
                "Rule dependencies are configured to require sandboxing, but \
                 the rule has no actions that could potentially require \
                 sandboxing."
            ]
      | Maybe ->
        select_sandbox_mode ~loc
          (Dep.Set.sandbox_config deps)
          ~sandboxing_preference:t.sandboxing_preference
    in
    let always_rerun =
      let force_rerun =
        !Clflags.force
        && List.exists targets_as_list ~f:Dpath.Build.is_alias_stamp_file
      and depends_on_universe = Dep.Set.has_universe deps in
      force_rerun || depends_on_universe
    in
    let rule_digest = compute_rule_digest rule ~deps ~action ~sandbox_mode in
    let () =
      (* FIXME: Rule hinting provide no relevant speed increase for now. Disable
         the overhead until we make a decision. *)
      if false then
        if Action.is_useful_to_distribute action = Maybe then
          let f { cache = (module Caching); _ } =
            match Caching.Cache.hint Caching.cache [ rule_digest ] with
            | Result.Ok _ -> ()
            | Result.Error e ->
              User_warning.emit [ Pp.textf "unable to hint the cache: %s" e ]
          in
          Option.iter ~f t.caching
    in
    let do_not_memoize =
      always_rerun || is_action_dynamic
      || Action.is_useful_to_memoize action = Clearly_not
    in
    (* Here we determine if we need to rerun the action based on information
       stored in Trace_db. *)
    let* rule_need_rerun =
      if always_rerun then
        Fiber.return true
      else
        (* [prev_trace] will be [None] if rule is run for the first time. *)
        let prev_trace = Trace_db.get (Path.build head_target) in
        (* [targets_digest] will be [None] if not all targets were build. *)
        let targets_digest = compute_targets_digest targets_as_list in
        let rule_or_targets_changed =
          match (prev_trace, targets_digest) with
          | Some prev_trace, Some targets_digest ->
            prev_trace.rule_digest <> rule_digest
            || prev_trace.targets_digest <> targets_digest
          | _ -> true
        in
        if rule_or_targets_changed then
          Fiber.return true
        else
          let prev_trace = Option.value_exn prev_trace in
          (* CR-someday aalekseyev: If there's a change at one of the last
             stages, we still re-run all the previous stages, which is a bit of
             a waste. We could remember what stage needs re-running and only
             re-run that (and later stages). *)
          let rec loop stages =
            match stages with
            | [] -> Fiber.return false
            | (deps, old_digest) :: rest ->
              let deps = Action_exec.Dynamic_dep.Set.to_dep_set deps in
              let* () = build_deps deps in
              let new_digest =
                compute_dependencies_digest deps ~sandbox_mode ~env ~eval_pred
              in
              if old_digest <> new_digest then
                Fiber.return true
              else
                loop rest
          in
          loop prev_trace.dynamic_deps_stages
    in
    let sandbox =
      Option.map sandbox_mode ~f:(fun mode ->
          let sandbox_suffix = rule_digest |> Digest.to_string in
          (Path.Build.relative sandbox_dir sandbox_suffix, mode))
    in
    let* () =
      if rule_need_rerun then (
        let from_Cache =
          match (do_not_memoize, t.caching) with
          | true, _
          | _, None ->
            None
          | false, Some { cache = (module Caching) as cache; _ } -> (
            match Caching.Cache.search Caching.cache rule_digest with
            | Ok (_, files) ->
              Log.info
                [ Pp.textf "cache hit for %s" (Digest.to_string rule_digest) ];
              Some (files, cache)
            | Error msg ->
              Log.info
                [ Pp.textf "cache miss for %s: %s"
                    (Digest.to_string rule_digest)
                    msg
                ];
              None )
        and cache_checking =
          match t.caching with
          | Some { check_probability; _ } -> Random.float 1. < check_probability
          | _ -> false
        in
        let remove_targets () =
          List.iter targets_as_list ~f:(fun target ->
              Cached_digest.remove (Path.build target);
              Path.unlink_no_err (Path.build target))
        in
        let pulled_from_cache =
          match from_Cache with
          | Some (files, (module Caching)) when not cache_checking -> (
            let () = remove_targets () in
            let retrieve (file : Cache.File.t) =
              let retrieved = Caching.Cache.retrieve Caching.cache file in
              Cached_digest.set retrieved file.digest;
              file.digest
            in
            match
              let digests = List.map files ~f:retrieve in
              Trace_db.set (Path.build head_target)
                (* We do not cache dynamic actions so [dynamic_deps_stages] is
                   always an empty list here. *)
                { rule_digest
                ; targets_digest = Digest.generic digests
                ; dynamic_deps_stages = []
                }
            with
            | exception Unix.(Unix_error (ENOENT, _, f)) ->
              Log.info
                [ Pp.textf "missing data file for cached rule %s: %s"
                    (Digest.to_string rule_digest)
                    f
                ];
              false
            | exception Sys_error m ->
              Log.info [ Pp.textf "error retrieving data file: %s" m ];
              false
            | () -> true )
          | _ -> false
        in
        if pulled_from_cache then
          Fiber.return ()
        else
          let () = remove_targets () in
          pending_targets := Path.Build.Set.union targets !pending_targets;
          let sandboxed, action =
            match sandbox with
            | None -> (None, action)
            | Some (sandbox_dir, sandbox_mode) ->
              Path.rm_rf (Path.build sandbox_dir);
              let sandboxed path : Path.Build.t =
                Path.Build.append_local sandbox_dir (Path.Build.local path)
              in
              Dep.Set.dirs deps
              |> Path.Set.iter ~f:(fun path ->
                     match Path.as_in_build_dir path with
                     | None -> Fs.assert_exists ~loc path
                     | Some path -> Fs.mkdir_p (sandboxed path));
              Fs.mkdir_p (sandboxed dir);
              ( Some sandboxed
              , Action.sandbox action ~sandboxed ~mode:sandbox_mode ~deps
                  ~eval_pred )
          in
          let chdirs = Action.chdirs action in
          Path.Set.iter chdirs ~f:Fs.(mkdir_p_or_check_exists ~loc);
          let+ exec_result =
            with_locks locks ~f:(fun () ->
                let copy_files_from_sandbox sandboxed =
                  List.iter targets_as_list ~f:(fun target ->
                      rename_optional_file ~src:(sandboxed target) ~dst:target)
                in
                let+ exec_result =
                  Action_exec.exec ~context ~env ~targets ~rule_loc:loc
                    ~build_deps action
                in
                Option.iter sandboxed ~f:copy_files_from_sandbox;
                exec_result)
          in
          Option.iter sandbox ~f:(fun (p, _mode) -> Path.rm_rf (Path.build p));
          (* All went well, these targets are no longer pending *)
          pending_targets := Path.Build.Set.diff !pending_targets targets;
          let targets, targets_digest =
            compute_targets_digest_or_raise_error ~loc targets_as_list
          in
          let () =
            (* Check cache. We don't check for missing file in the cache, since
               the file list is part of the rule hash this really never should
               happen. *)
            match from_Cache with
            | Some (cached, _) when cache_checking ->
              (* This being [false] is unexpected and means we have a hash
                 collision *)
              let data_are_ok =
                match
                  List.for_all2 targets cached
                    ~f:(fun (target, _) (c : Cache.File.t) ->
                      Path.Build.equal target c.path)
                with
                | Ok b -> b
                | Error `Length_mismatch -> false
              in
              if not data_are_ok then
                let open Pp.O in
                let pp x l ~f =
                  Pp.box ~indent:2
                    ( Pp.verbatim x
                    ++ Dyn.pp
                         (Dyn.Encoder.list Path.Build.to_dyn (List.map l ~f)) )
                in
                User_warning.emit
                  [ Pp.text "unexpected list of targets in the cache"
                  ; pp "expected: " targets ~f:fst
                  ; pp "got:      " cached ~f:(fun (c : Cache.File.t) -> c.path)
                  ]
              else
                List.iter2 targets cached
                  ~f:(fun (_, digest) (c : Cache.File.t) ->
                    if not (Digest.equal digest c.digest) then
                      User_warning.emit
                        [ Pp.textf "cache mismatch on %s: hash differ with %s"
                            (Path.Build.to_string_maybe_quoted c.path)
                            (Path.Build.to_string_maybe_quoted c.path)
                        ])
            | _ -> ()
          in
          let () =
            (* Promote *)
            match t.caching with
            | Some { cache = (module Caching : Cache.Caching); _ }
              when not do_not_memoize ->
              let report msg =
                let targets =
                  Path.Build.Set.to_list rule.action.targets
                  |> List.map ~f:Path.Build.to_string
                  |> String.concat ~sep:", "
                in
                Log.info [ Pp.textf "promotion failed for %s: %s" targets msg ]
              in
              let repository =
                let dir = Rule.find_source_dir rule in
                let open Option.O in
                let* vcs = File_tree.Dir.vcs dir in
                let f found = Path.equal found.Vcs.root vcs.Vcs.root in
                let+ _, i = get_vcs () |> List.findi ~f in
                i
              in
              Caching.Cache.promote Caching.cache targets rule_digest []
                ~repository ~duplication:None
              |> Result.map_error ~f:report |> ignore
            | _ -> ()
          in
          let dynamic_deps_stages =
            List.map exec_result.dynamic_deps_stages ~f:(fun deps ->
                ( deps
                , Action_exec.Dynamic_dep.Set.to_dep_set deps
                  |> compute_dependencies_digest ~sandbox_mode ~env ~eval_pred
                ))
          in
          Trace_db.set (Path.build head_target)
            { rule_digest; dynamic_deps_stages; targets_digest }
      ) else
        Fiber.return ()
    in
    let+ () =
      match (mode, !Clflags.promote) with
      | (Standard | Fallback | Ignore_source_files), _
      | Promote _, Some Never ->
        Fiber.return ()
      | Promote { lifetime; into; only }, (Some Automatically | None) ->
        Fiber.sequential_iter targets_as_list ~f:(fun path ->
            let consider_for_promotion =
              match only with
              | None -> true
              | Some pred ->
                Predicate_lang.Glob.exec pred
                  (Path.reach (Path.build path) ~from:(Path.build dir))
                  ~standard:Predicate_lang.any
            in
            match consider_for_promotion with
            | false -> Fiber.return ()
            | true -> (
              let in_source_tree = Path.Build.drop_build_context_exn path in
              let in_source_tree =
                match into with
                | None -> in_source_tree
                | Some { loc; dir } ->
                  Path.Source.relative
                    (Path.Source.relative
                       (Path.Source.parent_exn in_source_tree)
                       dir ~error_loc:loc)
                    (Path.Source.basename in_source_tree)
              in
              let path = Path.build path in
              let () =
                let dir = Path.Source.parent_exn in_source_tree in
                match File_tree.find_dir dir with
                | Some _ -> ()
                | None ->
                  let loc =
                    match into with
                    | Some into -> into.loc
                    | None ->
                      Code_error.raise
                        "promoting into directory that does not exist"
                        [ ("in_source_tree", Path.Source.to_dyn in_source_tree)
                        ]
                  in
                  User_error.raise ~loc
                    [ Pp.textf "directory %S does not exist"
                        (Path.Source.to_string_maybe_quoted dir)
                    ]
              in
              let in_source_tree = Path.source in_source_tree in
              match
                Path.exists in_source_tree
                && Cached_digest.file path = Cached_digest.file in_source_tree
              with
              | true -> Fiber.return ()
              | false ->
                if lifetime = Until_clean then
                  Promoted_to_delete.add in_source_tree;
                Scheduler.ignore_for_watch in_source_tree;
                (* The file in the build directory might be read-only if it
                   comes from the shared cache. However, we want the file in the
                   source tree to be writable by the user, so we explicitly set
                   the user writable bit. *)
                let chmod n = n lor 0o200 in
                let conf : Artifact_substitution.conf =
                  Artifact_substitution.conf_of_context context
                in
                Artifact_substitution.copy_file () ~src:path ~dst:in_source_tree
                  ~conf ~chmod ))
    in
    t.rule_done <- t.rule_done + 1

  (* a rule can have multiple files, but rule.run_rule may only be called once *)
  let build_file_impl path =
    let t = t () in
    let on_error exn = Dep_path.reraise exn (Path path) in
    Fiber.with_error_handler ~on_error (fun () ->
        get_rule_or_source t path >>= function
        | Source -> Fiber.return ()
        | Rule rule -> execute_rule rule)

  module Pred = struct
    let build_impl g =
      Pred.eval g |> Path.Set.to_list |> Fiber.parallel_iter ~f:build_file

    let eval_impl g =
      let dir = File_selector.dir g in
      Path.Set.filter (targets_of ~dir) ~f:(File_selector.test g)

    let eval =
      Memo.exec
        (Memo.create "eval-pred" ~doc:"Evaluate a predicate in a directory"
           ~input:(module File_selector)
           ~output:(Allow_cutoff (module Path.Set))
           ~visibility:Hidden Sync eval_impl)

    let build =
      Memo.exec
        (Memo.create "build-pred" ~doc:"build a predicate"
           ~input:(module File_selector)
           ~output:(Allow_cutoff (module Unit))
           ~visibility:Hidden Async build_impl)
  end

  let build_file_memo =
    Memo.create "build-file"
      ~output:(Allow_cutoff (module Unit))
      ~doc:"Build a file."
      ~input:(module Path)
      ~visibility:(Public Dpath.decode) Async build_file_impl

  let build_file = Memo.exec build_file_memo

  let execute_rule_memo =
    Memo.create "execute-rule"
      ~output:(Allow_cutoff (module Unit))
      ~doc:"-"
      ~input:(module Rule)
      ~visibility:Hidden Async execute_rule_impl

  let execute_rule = Memo.exec execute_rule_memo

  let () =
    Fdecl.set Rule_fn.loc_decl (fun () ->
        let stack = Memo.get_call_stack () in
        List.find_map stack ~f:(fun frame ->
            match
              Memo.Stack_frame.as_instance_of frame ~of_:execute_rule_memo
            with
            | Some input -> Some input
            | None ->
              Memo.Stack_frame.as_instance_of frame ~of_:Build_request.memo)
        |> Option.map ~f:Rule.loc)
end

open Exported

let eval_pred = Pred.eval

let process_memcycle exn =
  let cycle =
    Memo.Cycle_error.get exn
    |> List.filter_map ~f:(Memo.Stack_frame.as_instance_of ~of_:build_file_memo)
  in
  match List.last cycle with
  | None ->
    let frames = Memo.Cycle_error.get exn in
    Code_error.raise "dependency cycle that does not involve any files"
      [ ("frames", Dyn.Encoder.(list Memo.Stack_frame.to_dyn) frames) ]
  | Some last ->
    let first = List.hd cycle in
    let cycle =
      if last = first then
        cycle
      else
        last :: cycle
    in
    User_error.raise
      [ Pp.text "Dependency cycle between the following files:"
      ; Pp.chain cycle ~f:(fun p -> Pp.verbatim (Path.to_string_maybe_quoted p))
      ]

let set_packages f =
  let t = t () in
  Fdecl.set t.packages f

let package_deps (pkg : Package.t) files =
  let t = t () in
  let rules_seen = ref Rule.Set.empty in
  let rec loop fn acc =
    match Path.as_in_build_dir fn with
    | None ->
      (* if this file isn't in the build dir, it doesn't belong to any package
         and it doesn't have dependencies that do *)
      acc
    | Some fn ->
      let pkgs = Fdecl.get t.packages fn in
      if Package.Id.Set.is_empty pkgs || Package.Id.Set.mem pkgs pkg.id then
        loop_deps fn acc
      else
        Package.Id.Set.union acc pkgs
  and loop_deps fn acc =
    match get_rule (Path.build fn) with
    | None -> acc
    | Some ir ->
      if Rule.Set.mem !rules_seen ir then
        acc
      else (
        rules_seen := Rule.Set.add !rules_seen ir;
        (* We know that at this point of execution, all the action deps have
           been computed and memoized (see the call to [Build.paths_for_rule]
           below), so the following call to [Build_request.peek_deps_exn] cannot
           raise. *)
        (* CR-someday amokhov: It would be nice to statically rule out such
           potential race conditions between [Sync] and [Async] functions, e.g.
           by moving this code into a fiber. *)
        let action_deps = Build_request.peek_deps_exn ir in
        let action_deps = Dep.Set.paths action_deps ~eval_pred in
        Path.Set.fold action_deps ~init:acc ~f:loop
      )
  in
  let open Build.O in
  let+ () = Build.paths_for_rule files in
  (* We know that after [Build.paths_for_rule], all transitive dependencies of
     [files] are computed and memoized and so the above call to
     [Build_request.peek_deps_exn] is safe. *)
  Path.Set.fold files ~init:Package.Id.Set.empty ~f:(fun fn acc ->
      match Path.as_in_build_dir fn with
      | None -> acc
      | Some fn -> loop_deps fn acc)

let prefix_rules (prefix : unit Build.t) ~f =
  let res, rules = Rules.collect f in
  Rules.produce (Rules.map_rules rules ~f:(Rule.with_prefix ~build:prefix));
  res

module Alias = Alias0

let assert_not_in_memoized_function () =
  match Memo.get_call_stack () with
  | [] -> ()
  | stack ->
    Code_error.raise
      "Build_system.entry_point: called inside a memoized function"
      [ ("stack", Dyn.Encoder.list Memo.Stack_frame.to_dyn stack) ]

let process_exn_and_reraise =
  Exn_with_backtrace.map_and_reraise
    ~f:
      (Dep_path.map ~f:(function
        | Memo.Cycle_error.E exn -> process_memcycle exn
        | _ as exn -> exn))

let entry_point_async ~f =
  assert_not_in_memoized_function ();
  Fiber.with_error_handler f ~on_error:process_exn_and_reraise

let entry_point_sync ~f =
  assert_not_in_memoized_function ();
  match Exn_with_backtrace.try_with f with
  | Ok x -> x
  | Error exn -> process_exn_and_reraise exn

let do_build ~request =
  Hooks.End_of_build.once Promotion.finalize;
  entry_point_async ~f:(fun () ->
      let+ result, (_ : Dep.Set.t) =
        Build_request.evaluate_and_wait_for_dynamic_dependencies
          (Non_memoized request)
      in
      result)

let all_targets () =
  let t = t () in
  entry_point_sync ~f:(fun () -> all_targets t)

let targets_of ~dir = entry_point_sync ~f:(fun () -> targets_of ~dir)

let is_target file = Path.Set.mem (targets_of ~dir:(Path.parent_exn file)) file

module Evaluated_rule = struct
  module T = struct
    type t =
      { id : Rule.Id.t
      ; dir : Path.Build.t
      ; deps : Dep.Set.t
      ; targets : Path.Build.Set.t
      ; context : Build_context.t option
      ; action : Action.t
      }

    let compare a b = Rule.Id.compare a.id b.id

    let to_dyn _ = Dyn.opaque
  end

  module O = Comparable.Make (T)
  module Set = O.Set
  include T

  let rules_for_deps rules deps =
    Dep.Set.paths deps ~eval_pred
    |> Path.Set.fold ~init:Set.empty ~f:(fun path acc ->
           match
             Path.as_in_build_dir path
             |> Option.bind ~f:(Path.Build.Map.find rules)
           with
           | None -> acc
           | Some rule -> Set.add acc rule)
    |> Set.to_list
end

module Rule_top_closure = Top_closure.Make (Rule.Id.Set) (Monad.Id)

let evaluate_rules ~recursive ~request =
  entry_point_sync ~f:(fun () ->
      let rules = ref Rule.Id.Map.empty in
      let rec run_rule (rule : Rule.t) =
        if Rule.Id.Map.mem !rules rule.id then
          Fiber.return ()
        else
          let* action, deps = Build_request.evaluate (Memoized rule) in
          let (rule : Evaluated_rule.t) =
            { id = rule.id
            ; dir = rule.dir
            ; deps
            ; targets = rule.action.targets
            ; context = rule.context
            ; action
            }
          in
          rules := Rule.Id.Map.set !rules rule.id rule;
          if recursive then
            Dep.Set.parallel_iter_files deps ~f:run_dep ~eval_pred
          else
            Fiber.return ()
      and run_dep dep =
        match get_rule dep with
        | None -> Fiber.return () (* external files *)
        | Some rule -> run_rule rule
      in
      let* (), deps = Build_request.evaluate (Non_memoized request) in
      let+ () = Dep.Set.parallel_iter_files deps ~f:run_dep ~eval_pred in
      let rules =
        Rule.Id.Map.fold !rules ~init:Path.Build.Map.empty ~f:(fun r acc ->
            Path.Build.Set.fold r.targets ~init:acc ~f:(fun fn acc ->
                Path.Build.Map.set acc fn r))
      in
      match
        Rule_top_closure.top_closure
          (Evaluated_rule.rules_for_deps rules deps)
          ~key:(fun r -> r.Evaluated_rule.id)
          ~deps:(fun r -> Evaluated_rule.rules_for_deps rules r.deps)
      with
      | Ok l -> l
      | Error cycle ->
        User_error.raise
          [ Pp.text "Dependency cycle detected:"
          ; Pp.chain cycle ~f:(fun rule ->
                Pp.verbatim
                  (Path.to_string_maybe_quoted
                     (Path.build (Path.Build.Set.choose_exn rule.targets))))
          ])

let static_deps_of_request request =
  Static_deps.paths ~eval_pred @@ Build.static_deps request

let rules_for_files paths =
  Path.Set.fold paths ~init:[] ~f:(fun path acc ->
      match get_rule path with
      | None -> acc
      | Some rule -> rule :: acc)
  |> Rule.Set.of_list |> Rule.Set.to_list

let rules_for_transitive_closure targets =
  Rule_top_closure.top_closure (rules_for_files targets)
    ~key:(fun r -> r.Rule.id)
    ~deps:(fun r ->
      Build.static_deps r.action.build
      |> Static_deps.paths ~eval_pred
      |> rules_for_files)
  |> function
  | Ok l -> l
  | Error cycle ->
    User_error.raise
      [ Pp.text "Dependency cycle detected:"
      ; Pp.chain cycle ~f:(fun rule ->
            Pp.verbatim
              (Path.to_string_maybe_quoted
                 (Path.build (Path.Build.Set.choose_exn rule.action.targets))))
      ]

let load_dir_and_produce_its_rules ~dir =
  let loaded = load_dir ~dir in
  match loaded with
  | Non_build _ -> ()
  | Build loaded -> Rules.produce loaded.rules_produced

let load_dir ~dir = load_dir_and_produce_its_rules ~dir

let init ~contexts ?caching ~sandboxing_preference () =
  let contexts =
    Context_name.Map.of_list_map_exn contexts ~f:(fun c ->
        (c.Build_context.name, c))
  in
  let caching =
    let open Option.O in
    let* ({ cache = (module Caching : Cache.Caching); _ } as v) = caching in
    let open Result.O in
    let res =
      let+ cache = Caching.Cache.set_build_dir Caching.cache Path.build_dir in
      ( module struct
        module Cache = Caching.Cache

        let cache = cache
      end : Cache.Caching )
    in
    match res with
    | Result.Ok cache -> Some { v with cache }
    | Result.Error e ->
      User_warning.emit
        [ Pp.text "Unable to set cache build directory"
        ; Pp.textf "Reason: %s" e
        ];
      None
  in
  let t =
    { contexts
    ; packages = Fdecl.create Dyn.Encoder.opaque
    ; gen_rules = Fdecl.create Dyn.Encoder.opaque
    ; init_rules = Fdecl.create Dyn.Encoder.opaque
    ; vcs = Fdecl.create Dyn.Encoder.opaque
    ; caching
    ; sandboxing_preference = sandboxing_preference @ Sandbox_mode.all
    ; rule_done = 0
    ; rule_total = 0
    }
  in
  Console.Status_line.set (fun () ->
      Some
        (Pp.verbatim
           (sprintf "Done: %u/%u (jobs: %u)" t.rule_done t.rule_total
              (Scheduler.running_jobs_count ()))));
  set t
