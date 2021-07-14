open! Dune_engine
open! Stdune
open Import

let default_context_flags (ctx : Context.t) ~project =
  let cflags = Ocaml_config.ocamlc_cflags ctx.ocaml_config in
  let cxxflags =
    List.filter cflags ~f:(fun s -> not (String.is_prefix s ~prefix:"-std="))
  in
  let c, cxx =
    match Dune_project.use_standard_c_and_cxx_flags project with
    | None
    | Some false ->
      (Action_builder.return cflags, Action_builder.return cxxflags)
    | Some true ->
      let c = cflags @ Ocaml_config.ocamlc_cppflags ctx.ocaml_config in
      let cxx =
        let open Action_builder.O in
        let+ db_flags = Cxx_flags.get_flags ctx.build_dir in
        db_flags @ cxxflags
      in
      (Action_builder.return c, cxx)
  in
  Foreign_language.Dict.make ~c ~cxx

module Env_tree : sig
  type t

  val get_node : t -> dir:Path.Build.t -> Env_node.t Memo.Build.t

  val get_context_env : t -> Env.t

  val create :
       context:Context.t
    -> host_env_tree:t option
    -> scopes:Scope.DB.t
    -> default_env:Env_node.t Memo.Lazy.t
    -> stanzas_per_dir:Stanza.t list Dir_with_dune.t Path.Build.Map.t
    -> root_expander:Expander.t
    -> bin_artifacts:Artifacts.Bin.t
    -> context_env:Env.t
    -> t

  val bin_artifacts_host : t -> dir:Path.Build.t -> Artifacts.Bin.t Memo.Build.t

  val expander : t -> dir:Path.Build.t -> Expander.t Memo.Build.t
end = struct
  open Memo.Build.O

  type t =
    { context : Context.t
    ; context_env : Env.t  (** context env with additional variables *)
    ; scopes : Scope.DB.t
    ; default_env : Env_node.t Memo.Lazy.t
    ; stanzas_per_dir : Stanza.t list Dir_with_dune.t Path.Build.Map.t
    ; host : t option
    ; root_expander : Expander.t
    ; bin_artifacts : Artifacts.Bin.t
    ; get_node : Path.Build.t -> Env_node.t Memo.Build.t
    }

  let get_node t ~dir = t.get_node dir

  let get_context_env t = t.context_env

  let bin_artifacts_host t ~dir =
    let bin_artifacts t ~dir = get_node t ~dir >>= Env_node.bin_artifacts in
    match t.host with
    | None -> bin_artifacts t ~dir
    | Some host ->
      let dir =
        Path.Build.drop_build_context_exn dir
        |> Path.Build.append_source host.context.build_dir
      in
      bin_artifacts host ~dir

  let external_env t ~dir = get_node t ~dir >>= Env_node.external_env

  let expander_for_artifacts ~scope ~external_env ~root_expander ~dir =
    Expander.extend_env root_expander ~env:external_env
    |> Expander.set_scope ~scope |> Expander.set_dir ~dir

  let extend_expander t ~dir ~expander_for_artifacts =
    let* bin_artifacts_host = bin_artifacts_host t ~dir in
    let+ bindings =
      let+ inline_tests = get_node t ~dir >>= Env_node.inline_tests in
      let str = Dune_env.Stanza.Inline_tests.to_string inline_tests in
      Pform.Map.singleton (Var Inline_tests) [ Value.String str ]
    in
    expander_for_artifacts
    |> Expander.add_bindings ~bindings
    |> Expander.set_bin_artifacts ~bin_artifacts_host

  let expander t ~dir =
    let* node = get_node t ~dir in
    let* external_env = external_env t ~dir in
    let scope = Env_node.scope node in
    let expander_for_artifacts =
      expander_for_artifacts ~scope ~external_env ~root_expander:t.root_expander
        ~dir
    in
    let+ expander = extend_expander t ~dir ~expander_for_artifacts in
    Expander.set_foreign_flags expander ~f:(fun ~dir ->
        get_node t ~dir >>| Env_node.foreign_flags)

  let get_env_stanza t ~dir =
    Option.value ~default:Dune_env.Stanza.empty
    @@
    let open Option.O in
    let* stanza = Path.Build.Map.find t.stanzas_per_dir dir in
    List.find_map stanza.data ~f:(function
      | Dune_env.T config -> Some config
      | _ -> None)

  let get_impl t dir =
    (* We recompute the scope on every recursive call, even though it should be
       unchanged. If this becomes a problem, we can memoize [find_by_dir]. *)
    let scope = Scope.DB.find_by_dir t.scopes dir in
    let inherit_from =
      if Path.Build.equal dir (Scope.root scope) then
        let format_config = Dune_project.format_config (Scope.project scope) in
        Memo.lazy_ (fun () ->
            let+ default_env = Memo.Lazy.force t.default_env in
            Env_node.set_format_config default_env format_config)
      else
        match Path.Build.parent dir with
        | None ->
          Code_error.raise "Super_context.Env.get called on invalid directory"
            [ ("dir", Path.Build.to_dyn dir) ]
        | Some parent -> Memo.lazy_ (fun () -> get_node t ~dir:parent)
    in
    let config_stanza = get_env_stanza t ~dir in
    let project = Scope.project scope in
    let default_context_flags = default_context_flags t.context ~project in
    let expander_for_artifacts =
      Memo.lazy_ (fun () ->
          let+ external_env = external_env t ~dir in
          expander_for_artifacts ~scope ~root_expander:t.root_expander
            ~external_env ~dir)
    in
    let expander =
      Memo.lazy_ (fun () ->
          let* expander_for_artifacts =
            Memo.Lazy.force expander_for_artifacts
          in
          extend_expander t ~dir ~expander_for_artifacts)
    in
    Env_node.make ~dir ~scope ~config_stanza ~inherit_from:(Some inherit_from)
      ~profile:t.context.profile ~expander ~expander_for_artifacts
      ~default_context_flags ~default_env:t.context_env
      ~default_bin_artifacts:t.bin_artifacts

  (* Here we jump through some hoops to construct [t] as well as create a
     memoization table that has access to [t] and is used in [t.get_node].

     Morally, the code below is just:

     let rec env_tree = ... and memo = ... in env_tree

     However, the right-hand side of [memo] is not allowed in a recursive let
     binding. To work around this limitation, we place the functions into a
     recursive module [Rec]. Since recursive let-modules are not allowed either,
     we need to also wrap [Rec] inside a non-recursive module [Non_rec]. *)
  let create ~context ~host_env_tree ~scopes ~default_env ~stanzas_per_dir
      ~root_expander ~bin_artifacts ~context_env =
    let module Non_rec = struct
      module rec Rec : sig
        val env_tree : unit -> t

        val memo : Path.Build.t -> Env_node.t Memo.Build.t
      end = struct
        let env_tree =
          { context
          ; context_env
          ; scopes
          ; default_env
          ; stanzas_per_dir
          ; host = host_env_tree
          ; root_expander
          ; bin_artifacts
          ; get_node = Rec.memo
          }

        let memo =
          Memo.exec
            (Memo.create "env-nodes-memo"
               ~input:(module Path.Build)
               (fun path -> Memo.Build.return (get_impl env_tree path)))

        let env_tree () = env_tree
      end
    end in
    Non_rec.Rec.env_tree ()
end

module Lib_entry = struct
  type t =
    | Library of Lib.Local.t
    | Deprecated_library_name of Dune_file.Deprecated_library_name.t

  let name = function
    | Library lib -> Lib.Local.to_lib lib |> Lib.name
    | Deprecated_library_name { old_name = old_public_name, _; _ } ->
      Dune_file.Public_lib.name old_public_name
end

type t =
  { context : Context.t
  ; scopes : Scope.DB.t
  ; public_libs : Lib.DB.t
  ; installed_libs : Lib.DB.t
  ; stanzas : Dune_file.Stanzas.t Dir_with_dune.t list
  ; stanzas_per_dir : Dune_file.Stanzas.t Dir_with_dune.t Path.Build.Map.t
  ; packages : Package.t Package.Name.Map.t
  ; artifacts : Artifacts.t
  ; root_expander : Expander.t
  ; host : t option
  ; lib_entries_by_package : Lib_entry.t list Package.Name.Map.t
  ; env_tree : Env_tree.t
  ; dir_status_db : Dir_status.DB.t
  ; (* Env node that represents the environment configured for the workspace. It
       is used as default at the root of every project in the workspace. *)
    default_env : Env_node.t Memo.Lazy.t
  ; projects_by_key : Dune_project.t Dune_project.File_key.Map.t
  }

let context t = t.context

let context_env t = Env_tree.get_context_env t.env_tree

let stanzas t = t.stanzas

let stanzas_in t ~dir = Path.Build.Map.find t.stanzas_per_dir dir

let packages t = t.packages

let equal = (( == ) : t -> t -> bool)

let hash t = Context.hash t.context

let to_dyn_concise t = Context.to_dyn_concise t.context

let to_dyn t = Context.to_dyn t.context

let host t = Option.value t.host ~default:t

let any_package_aux ~packages ~context pkg =
  match Package.Name.Map.find packages pkg with
  | Some p -> Memo.Build.return (Some (Expander.Local p))
  | None -> (
    let open Memo.Build.O in
    Findlib.find_root_package context.Context.findlib pkg >>| function
    | Ok p -> Some (Expander.Installed p)
    | Error Not_found -> None
    | Error (Invalid_dune_package exn) -> Exn.raise exn)

let any_package t pkg =
  any_package_aux ~packages:t.packages ~context:t.context pkg

let get_site_of_packages_aux ~loc ~any_package ~pkg ~site =
  let find_site sites ~pkg ~site =
    match Section.Site.Map.find sites site with
    | Some section -> section
    | None ->
      User_error.raise ~loc
        [ Pp.textf "Package %s doesn't define a site %s"
            (Package.Name.to_string pkg)
            (Section.Site.to_string site)
        ]
  in
  let open Memo.Build.O in
  any_package pkg >>| function
  | Some (Expander.Local p) -> find_site p.Package.sites ~pkg ~site
  | Some (Expander.Installed p) -> find_site p.sites ~pkg ~site
  | None ->
    User_error.raise ~loc
      [ Pp.textf "The package %s is not found" (Package.Name.to_string pkg) ]

let get_site_of_packages t ~loc ~pkg ~site =
  get_site_of_packages_aux ~loc ~any_package:(any_package t) ~pkg ~site

let lib_entries_of_package t pkg_name =
  Package.Name.Map.find t.lib_entries_by_package pkg_name
  |> Option.value ~default:[]

let internal_lib_names t =
  List.fold_left t.stanzas ~init:Lib_name.Set.empty
    ~f:(fun acc { Dir_with_dune.data = stanzas; _ } ->
      List.fold_left stanzas ~init:acc ~f:(fun acc -> function
        | Dune_file.Library lib ->
          Lib_name.Set.add
            (match lib.visibility with
            | Private _ -> acc
            | Public public ->
              Lib_name.Set.add acc (Dune_file.Public_lib.name public))
            (Lib_name.of_local lib.name)
        | _ -> acc))

let public_libs t = t.public_libs

let installed_libs t = t.installed_libs

let find_scope_by_dir t dir = Scope.DB.find_by_dir t.scopes dir

let find_scope_by_project t = Scope.DB.find_by_project t.scopes

let find_project_by_key t = Dune_project.File_key.Map.find_exn t.projects_by_key

let expander t ~dir = Env_tree.expander t.env_tree ~dir

let get_node t = Env_tree.get_node t

open Memo.Build.O

let make_full_action t ~dir ~locks build =
  let open Action_builder.O in
  let+ (action : Action.t) = build
  and+ env =
    Action_builder.memo_build
      (let open Memo.Build.O in
      get_node t.env_tree ~dir >>= Env_node.external_env)
  in
  let action =
    match action with
    | Chdir _ -> action
    | _ -> Chdir (Path.build t.context.build_dir, action)
  in
  { Action.Full.action; env; locks; can_go_in_shared_cache = true }

let make_rule t ?sandbox ?mode ?(locks = []) ?loc ~dir
    { Action_builder.With_targets.build; targets } =
  let build = make_full_action t build ~locks ~dir in
  Rule.make ?sandbox ?mode ~info:(Rule.Info.of_loc_opt loc)
    ~context:(Some (Context.build_context t.context))
    ~targets build

let add_rule t ?sandbox ?mode ?locks ?loc ~dir build =
  let rule = make_rule t ?sandbox ?mode ?locks ?loc ~dir build in
  Rules.Produce.rule rule

let add_rule_get_targets t ?sandbox ?mode ?locks ?loc ~dir build =
  let rule = make_rule t ?sandbox ?mode ?locks ?loc ~dir build in
  let+ () = Rules.Produce.rule rule in
  rule.targets

let add_rules t ?sandbox ~dir builds =
  Memo.Build.parallel_iter builds ~f:(add_rule t ?sandbox ~dir)

let add_alias_action t alias ~dir ~loc ?(locks = []) action =
  let build = make_full_action t action ~locks ~dir in
  Rules.Produce.Alias.add_action
    ~context:(Context.build_context t.context)
    alias ~loc build

let build_dir_is_vendored build_dir =
  match Path.Build.drop_build_context build_dir with
  | Some src_dir -> Dune_engine.Source_tree.is_vendored src_dir
  | None -> Memo.Build.return false

let ocaml_flags t ~dir (spec : Ocaml_flags.Spec.t) =
  let* expander = Env_tree.expander t.env_tree ~dir in
  let* flags =
    let+ ocaml_flags = get_node t.env_tree ~dir >>= Env_node.ocaml_flags in
    Ocaml_flags.make ~spec ~default:ocaml_flags
      ~eval:(Expander.expand_and_eval_set expander)
  in
  build_dir_is_vendored dir >>| function
  | true -> Ocaml_flags.with_vendored_warnings flags
  | false -> flags

let foreign_flags t ~dir ~expander ~flags ~language =
  let ccg = Context.cc_g t.context in
  let default =
    get_node t.env_tree ~dir >>| Env_node.foreign_flags >>| fun dict ->
    Foreign_language.Dict.get dict language
  in
  let open Action_builder.O in
  let name = Foreign_language.proper_name language in
  let flags =
    let* default = Action_builder.memo_build default in
    let+ l = Expander.expand_and_eval_set expander flags ~standard:default in
    l @ ccg
  in
  Action_builder.memoize (sprintf "%s flags" name) flags

let menhir_flags t ~dir ~expander ~flags =
  let t = t.env_tree in
  let default =
    get_node t ~dir >>| Env_node.menhir_flags |> Action_builder.memo_build_join
  in
  Action_builder.memoize "menhir flags"
    (Expander.expand_and_eval_set expander flags ~standard:default)

let local_binaries t ~dir = get_node t.env_tree ~dir >>= Env_node.local_binaries

let odoc t ~dir = get_node t.env_tree ~dir >>= Env_node.odoc

let coq t ~dir = get_node t.env_tree ~dir >>= Env_node.coq

let format_config t ~dir = get_node t.env_tree ~dir >>= Env_node.format_config

let dump_env t ~dir =
  let t = t.env_tree in
  let ocaml_flags = get_node t ~dir >>= Env_node.ocaml_flags in
  let foreign_flags = get_node t ~dir >>| Env_node.foreign_flags in
  let menhir_flags = get_node t ~dir >>| Env_node.menhir_flags in
  let coq_flags = get_node t ~dir >>= Env_node.coq in
  let open Action_builder.O in
  let+ o_dump =
    let* ocaml_flags = Action_builder.memo_build ocaml_flags in
    Ocaml_flags.dump ocaml_flags
  and+ c_dump =
    let* foreign_flags = Action_builder.memo_build foreign_flags in
    let+ c_flags = foreign_flags.c
    and+ cxx_flags = foreign_flags.cxx in
    List.map
      ~f:Dune_lang.Encoder.(pair string (list string))
      [ ("c_flags", c_flags); ("cxx_flags", cxx_flags) ]
  and+ menhir_dump =
    let+ flags = Action_builder.memo_build_join menhir_flags in
    [ ("menhir_flags", flags) ]
    |> List.map ~f:Dune_lang.Encoder.(pair string (list string))
  and+ coq_dump =
    let+ flags = Action_builder.memo_build_join coq_flags in
    [ ("coq_flags", flags) ]
    |> List.map ~f:Dune_lang.Encoder.(pair string (list string))
  in
  List.concat [ o_dump; c_dump; menhir_dump; coq_dump ]

let resolve_program t ~dir ?hint ~loc bin =
  let t = t.env_tree in
  let* bin_artifacts = Env_tree.bin_artifacts_host t ~dir in
  Artifacts.Bin.binary ?hint ~loc bin_artifacts bin

let get_installed_binaries stanzas ~(context : Context.t) =
  let open Memo.Build.O in
  let install_dir = Local_install_path.bin_dir ~context:context.name in
  let expand_str ~dir sw =
    Expander.With_reduced_var_set.expand_str ~context ~dir sw
  in
  let expand_str_partial ~dir sw =
    Expander.With_reduced_var_set.expand_str_partial ~context ~dir sw
  in
  Memo.Build.List.map stanzas ~f:(fun (d : _ Dir_with_dune.t) ->
      Memo.Build.List.map d.data ~f:(fun stanza ->
          let binaries_from_install files =
            Memo.Build.List.map files ~f:(fun fb ->
                let+ p =
                  File_binding.Unexpanded.destination_relative_to_install_path
                    fb ~section:Bin
                    ~expand:(expand_str ~dir:d.ctx_dir)
                    ~expand_partial:(expand_str_partial ~dir:d.ctx_dir)
                in
                let p = Path.Local.of_string (Install.Dst.to_string p) in
                if Path.Local.is_root (Path.Local.parent_exn p) then
                  Some (Path.Build.append_local install_dir p)
                else
                  None)
            >>| List.filter_map ~f:Fun.id >>| Path.Build.Set.of_list
          in
          match (stanza : Stanza.t) with
          | Dune_file.Install { section = Section Bin; files; _ } ->
            binaries_from_install files
          | Dune_file.Executables
              ({ install_conf = Some { section = Section Bin; files; _ }; _ } as
              exes) ->
            let* compile_info =
              let project = Scope.project d.scope in
              let dune_version = Dune_project.dune_version project in
              let+ pps =
                Resolve.Build.read_memo_build
                  (Preprocess.Per_module.with_instrumentation
                     exes.buildable.preprocess
                     ~instrumentation_backend:
                       (Lib.DB.instrumentation_backend (Scope.libs d.scope)))
                >>| Preprocess.Per_module.pps
              in
              Lib.DB.resolve_user_written_deps_for_exes (Scope.libs d.scope)
                exes.names exes.buildable.libraries ~pps ~dune_version
                ~allow_overlaps:exes.buildable.allow_overlapping_dependencies
            in
            let* available =
              let open Memo.Build.O in
              let+ available = Lib.Compile.direct_requires compile_info in
              Resolve.is_ok available
            in
            if available then
              binaries_from_install files
            else
              Memo.Build.return Path.Build.Set.empty
          | _ -> Memo.Build.return Path.Build.Set.empty)
      >>| Path.Build.Set.union_all)
  >>| Path.Build.Set.union_all

let create_lib_entries_by_package ~public_libs stanzas =
  Dir_with_dune.Memo.deep_fold stanzas ~init:[] ~f:(fun d stanza acc ->
      match stanza with
      | Dune_file.Library ({ visibility = Private (Some pkg); _ } as lib) -> (
        let+ lib =
          let db = Scope.libs d.scope in
          Lib.DB.find db (Dune_file.Library.best_name lib)
        in
        match lib with
        | None -> acc
        | Some lib ->
          let name = Package.name pkg in
          (name, Lib_entry.Library (Lib.Local.of_lib_exn lib)) :: acc)
      | Dune_file.Library { visibility = Public pub; _ } -> (
        let+ lib = Lib.DB.find public_libs (Dune_file.Public_lib.name pub) in
        match lib with
        | None ->
          (* Skip hidden or unavailable libraries. TODO we should assert that
             the library name is always found somehow *)
          acc
        | Some lib ->
          let package = Dune_file.Public_lib.package pub in
          let name = Package.name package in
          (name, Lib_entry.Library (Lib.Local.of_lib_exn lib)) :: acc)
      | Dune_file.Deprecated_library_name
          ({ old_name = old_public_name, _; _ } as d) ->
        let package = Dune_file.Public_lib.package old_public_name in
        let name = Package.name package in
        Memo.Build.return ((name, Lib_entry.Deprecated_library_name d) :: acc)
      | _ -> Memo.Build.return acc)
  >>| fun libs ->
  Package.Name.Map.of_list_multi libs
  |> Package.Name.Map.map
       ~f:
         (List.sort ~compare:(fun a b ->
              Lib_name.compare (Lib_entry.name a) (Lib_entry.name b)))

let create_projects_by_package projects : Dune_project.t Package.Name.Map.t =
  List.concat_map projects ~f:(fun project ->
      Dune_project.packages project
      |> Package.Name.Map.values
      |> List.map ~f:(fun (pkg : Package.t) ->
             let name = Package.name pkg in
             (name, project)))
  |> Package.Name.Map.of_list_exn

let modules_of_lib = Fdecl.create Dyn.Encoder.opaque

let create ~(context : Context.t) ~host ~projects ~packages ~stanzas =
  let lib_config = Context.lib_config context in
  let projects_by_package = create_projects_by_package projects in
  let installed_libs =
    Lib.DB.create_from_findlib context.findlib ~lib_config ~projects_by_package
  in
  let modules_of_lib_for_scope = Fdecl.create Dyn.Encoder.opaque in
  let* scopes, public_libs =
    Scope.DB.create_from_stanzas ~projects ~projects_by_package ~context
      ~installed_libs ~modules_of_lib:modules_of_lib_for_scope stanzas
  in
  let stanzas =
    List.map stanzas ~f:(fun { Dune_file.dir; project; stanzas } ->
        let ctx_dir = Path.Build.append_source context.build_dir dir in
        let dune_version = Dune_project.dune_version project in
        { Dir_with_dune.src_dir = dir
        ; ctx_dir
        ; data = stanzas
        ; scope = Scope.DB.find_by_project scopes project
        ; dune_version
        })
  in
  let stanzas_per_dir =
    Path.Build.Map.of_list_map_exn stanzas ~f:(fun stanzas ->
        (stanzas.Dir_with_dune.ctx_dir, stanzas))
  in
  let* artifacts =
    let+ local_bins = get_installed_binaries ~context stanzas in
    Artifacts.create context ~public_libs ~local_bins
  in
  let any_package = any_package_aux ~packages ~context in
  let root_expander =
    let scopes_host, artifacts_host, context_host =
      match host with
      | None -> (scopes, artifacts, context)
      | Some host -> (host.scopes, host.artifacts, host.context)
    in
    Expander.make
      ~scope:(Scope.DB.find_by_dir scopes context.build_dir)
      ~scope_host:(Scope.DB.find_by_dir scopes_host context_host.build_dir)
      ~context ~lib_artifacts:artifacts.public_libs
      ~bin_artifacts_host:artifacts_host.bin
      ~lib_artifacts_host:artifacts_host.public_libs ~find_package:any_package
  in
  let dune_dir_locations_var : Stdune.Env.Var.t = "DUNE_DIR_LOCATIONS" in
  (* Add the section of the site mentioned in stanzas (it could be a site of an
     external package) *)
  let add_in_package_section m pkg section =
    Package.Name.Map.update m pkg ~f:(function
      | None -> Some (Section.Set.singleton section)
      | Some s -> Some (Section.Set.add s section))
  in
  let* package_sections =
    Dir_with_dune.Memo.deep_fold stanzas ~init:Package.Name.Map.empty
      ~f:(fun _ stanza acc ->
        let add_in_package_sites acc pkg site loc =
          let+ section =
            get_site_of_packages_aux ~loc ~any_package ~pkg ~site
          in
          add_in_package_section acc pkg section
        in
        match stanza with
        | Dune_file.Install { section = Site { pkg; site; loc }; _ } ->
          add_in_package_sites acc pkg site loc
        | Dune_file.Plugin { site = loc, (pkg, site); _ } ->
          add_in_package_sites acc pkg site loc
        | _ -> Memo.Build.return acc)
  in
  (* Add the site of the local package: it should only useful for making sure
     that at least one location is given to the site of local package because if
     the site is used it should already be in [packages_sections] *)
  let* package_sections =
    Package.Name.Map.to_list packages
    |> Memo.Build.List.fold_left ~init:package_sections
         ~f:(fun acc (package_name, package) ->
           Section.Site.Map.to_list package.Package.sites
           |> Memo.Build.List.fold_left ~init:acc ~f:(fun acc (_, section) ->
                  Memo.Build.return
                    (add_in_package_section acc package_name section)))
  in
  let env_dune_dir_locations =
    let install_dir = Local_install_path.dir ~context:context.Context.name in
    let install_dir = Path.build install_dir in
    let v =
      Option.value
        (Stdune.Env.get context.env dune_dir_locations_var)
        ~default:""
    in
    Package.Name.Map.foldi ~init:v package_sections
      ~f:(fun package_name sections init ->
        let paths =
          Install.Section.Paths.make ~package:package_name ~destdir:install_dir
            ()
        in
        Section.Set.fold sections ~init ~f:(fun section acc ->
            sprintf "%s%c%s%c%s%s"
              (Package.Name.to_string package_name)
              Stdune.Bin.path_sep
              (Section.to_string section)
              Stdune.Bin.path_sep
              (Path.to_absolute_filename
                 (Install.Section.Paths.get paths section))
              (if String.is_empty acc then
                acc
              else
                sprintf "%c%s" Stdune.Bin.path_sep acc)))
  in
  let context_env =
    if String.is_empty env_dune_dir_locations then
      context.env
    else
      Stdune.Env.add context.env ~var:dune_dir_locations_var
        ~value:env_dune_dir_locations
  in
  let default_env =
    Memo.lazy_ (fun () ->
        let make ~inherit_from ~config_stanza =
          let dir = context.build_dir in
          let scope = Scope.DB.find_by_dir scopes dir in
          let project = Scope.project scope in
          let default_context_flags = default_context_flags context ~project in
          let expander_for_artifacts =
            Memo.lazy_ (fun () ->
                Code_error.raise
                  "[expander_for_artifacts] in [default_env] is undefined" [])
          in
          let expander = Memo.Lazy.of_val root_expander in
          Env_node.make ~dir ~scope ~inherit_from ~config_stanza
            ~profile:context.profile ~expander ~expander_for_artifacts
            ~default_context_flags ~default_env:context_env
            ~default_bin_artifacts:artifacts.bin
        in
        Memo.Build.return
          (make ~config_stanza:context.env_nodes.context
             ~inherit_from:
               (Some
                  (Memo.lazy_ (fun () ->
                       Memo.Build.return
                         (make ~inherit_from:None
                            ~config_stanza:context.env_nodes.workspace))))))
  in
  let env_tree =
    Env_tree.create ~context ~scopes ~default_env ~stanzas_per_dir
      ~host_env_tree:(Option.map host ~f:(fun x -> x.env_tree))
      ~root_expander ~bin_artifacts:artifacts.bin ~context_env
  in
  let dir_status_db = Dir_status.DB.make ~stanzas_per_dir in
  let projects_by_key =
    Dune_project.File_key.Map.of_list_map_exn projects ~f:(fun project ->
        (Dune_project.file_key project, project))
  in
  let+ lib_entries_by_package =
    create_lib_entries_by_package ~public_libs stanzas
  in
  let t =
    { context
    ; root_expander
    ; host
    ; scopes
    ; public_libs
    ; installed_libs
    ; stanzas
    ; stanzas_per_dir
    ; packages
    ; artifacts
    ; lib_entries_by_package
    ; env_tree
    ; default_env
    ; dir_status_db
    ; projects_by_key
    }
  in
  Fdecl.set modules_of_lib_for_scope (fun ~dir ~name ->
      Fdecl.get modules_of_lib t ~dir ~name);
  t

let filter_out_stanzas_from_hidden_packages ~visible_pkgs =
  List.filter_map ~f:(fun stanza ->
      let include_stanza =
        match Dune_file.stanza_package stanza with
        | None -> true
        | Some package ->
          let name = Package.name package in
          Package.Name.Map.mem visible_pkgs name
      in
      if include_stanza then
        Some stanza
      else
        match stanza with
        | Dune_file.Library l ->
          let open Option.O in
          let+ redirect = Dune_file.Library_redirect.Local.of_private_lib l in
          Dune_file.Library_redirect redirect
        | _ -> None)

let all =
  Memo.lazy_ (fun () ->
      let open Memo.Build.O in
      let* { Dune_load.dune_files; packages; projects } = Dune_load.load ()
      and* contexts = Context.DB.all ()
      and* only_packages = Only_packages.get () in
      let packages = Option.value only_packages ~default:packages in
      let rec sctxs =
        lazy
          (Context_name.Map.of_list_map_exn contexts ~f:(fun (c : Context.t) ->
               (c.name, Memo.Lazy.create (fun () -> make_sctx c))))
      and make_sctx (context : Context.t) =
        let host () =
          match context.for_host with
          | None -> Memo.Build.return None
          | Some h ->
            let+ sctx =
              Memo.Lazy.force
                (Context_name.Map.find_exn (Lazy.force sctxs) h.name)
            in
            Some sctx
        in
        let stanzas () =
          let+ stanzas = Dune_load.Dune_files.eval ~context dune_files in
          match only_packages with
          | None -> stanzas
          | Some visible_pkgs ->
            List.map stanzas ~f:(fun (dir_conf : Dune_file.t) ->
                { dir_conf with
                  stanzas =
                    filter_out_stanzas_from_hidden_packages ~visible_pkgs
                      dir_conf.stanzas
                })
        in
        let* host, stanzas = Memo.Build.fork_and_join host stanzas in
        create ~host ~context ~projects ~packages ~stanzas
      in
      Lazy.force sctxs |> Context_name.Map.to_list
      |> Memo.Build.parallel_map ~f:(fun (name, sctx) ->
             let+ sctx = Memo.Lazy.force sctx in
             (name, sctx))
      >>| Context_name.Map.of_list_exn)

let find name =
  let open Memo.Build.O in
  let+ all = Memo.Lazy.force all in
  Context_name.Map.find all name

let dir_status_db t = t.dir_status_db

module As_memo_key = struct
  type nonrec t = t

  let equal = equal

  let hash = hash

  let to_dyn = to_dyn_concise
end
