open! Stdune
open Import
module Menhir_rules = Menhir
module Toplevel_rules = Toplevel.Stanza
open Dune_file
open! No_io

module For_stanza : sig
  type ('merlin, 'cctx, 'js, 'source_dirs) t =
    { merlin : 'merlin
    ; cctx : 'cctx
    ; js : 'js
    ; source_dirs : 'source_dirs
    }

  val of_stanzas :
       Stanza.t list
    -> cctxs:(Loc.t * Compilation_context.t) list
    -> sctx:Super_context.t
    -> src_dir:Path.Source.t
    -> ctx_dir:Path.Build.t
    -> scope:Scope.t
    -> dir_contents:Dir_contents.t
    -> expander:Expander.t
    -> files_to_install:(File_binding.Unexpanded.t Install_conf.t -> unit)
    -> ( Merlin.t list
       , (Loc.t * Compilation_context.t) list
       , Path.Build.t list
       , Path.Source.t list )
       t
end = struct
  type ('merlin, 'cctx, 'js, 'source_dirs) t =
    { merlin : 'merlin
    ; cctx : 'cctx
    ; js : 'js
    ; source_dirs : 'source_dirs
    }

  let empty_none = { merlin = None; cctx = None; js = None; source_dirs = None }

  let empty_list = { merlin = []; cctx = []; js = []; source_dirs = [] }

  let cons_maybe hd_o tl =
    match hd_o with
    | Some hd -> hd :: tl
    | None -> tl

  let cons acc x =
    { merlin = cons_maybe x.merlin acc.merlin
    ; cctx = cons_maybe x.cctx acc.cctx
    ; source_dirs = cons_maybe x.source_dirs acc.source_dirs
    ; js =
        ( match x.js with
        | None -> acc.js
        | Some js -> List.rev_append acc.js js )
    }

  let rev t =
    { t with
      merlin = List.rev t.merlin
    ; cctx = List.rev t.cctx
    ; source_dirs = List.rev t.source_dirs
    }

  let of_stanza stanza ~sctx ~src_dir ~ctx_dir ~scope ~dir_contents ~expander
      ~files_to_install =
    let dir = ctx_dir in
    match stanza with
    | Toplevel toplevel ->
      Toplevel_rules.setup ~sctx ~dir ~toplevel;
      empty_none
    | Library lib
      when Lib.DB.available (Scope.libs scope) (Dune_file.Library.best_name lib)
      ->
      let cctx, merlin =
        Lib_rules.rules lib ~sctx ~dir ~scope ~dir_contents ~expander
      in
      { merlin = Some merlin
      ; cctx = Some (lib.buildable.loc, cctx)
      ; js = None
      ; source_dirs = None
      }
    | Foreign_library lib ->
      Lib_rules.foreign_rules lib ~sctx ~dir ~dir_contents ~expander;
      empty_none
    | Executables exes ->
      Option.iter exes.install_conf ~f:files_to_install;
      let cctx, merlin =
        Exe_rules.rules exes ~sctx ~dir ~scope ~expander ~dir_contents
      in
      { merlin = Some merlin
      ; cctx = Some (exes.buildable.loc, cctx)
      ; js =
          Some
            (List.concat_map exes.names ~f:(fun (_, exe) ->
                 List.map
                   [ exe ^ ".bc.js"; exe ^ ".bc.runtime.js" ]
                   ~f:(Path.Build.relative dir)))
      ; source_dirs = None
      }
    | Alias alias ->
      Simple_rules.alias sctx alias ~dir ~expander;
      empty_none
    | Tests tests ->
      let cctx, merlin =
        Test_rules.rules tests ~sctx ~dir ~scope ~expander ~dir_contents
      in
      { merlin = Some merlin
      ; cctx = Some (tests.exes.buildable.loc, cctx)
      ; js = None
      ; source_dirs = None
      }
    | Copy_files { glob; _ } ->
      let source_dir =
        let loc = String_with_vars.loc glob in
        let src_glob = Expander.expand_str expander glob in
        Path.Source.relative src_dir src_glob ~error_loc:loc
        |> Path.Source.parent_exn
      in
      { merlin = None; cctx = None; js = None; source_dirs = Some source_dir }
    | Install i ->
      files_to_install i;
      empty_none
    | Cinaps.T cinaps ->
      Cinaps.gen_rules sctx cinaps ~dir ~scope;
      empty_none
    | _ -> empty_none

  let of_stanzas stanzas ~cctxs ~sctx ~src_dir ~ctx_dir ~scope ~dir_contents
      ~expander ~files_to_install =
    let of_stanza =
      of_stanza ~sctx ~src_dir ~ctx_dir ~scope ~dir_contents ~expander
        ~files_to_install
    in
    List.fold_left stanzas ~init:{ empty_list with cctx = cctxs }
      ~f:(fun acc a -> cons acc (of_stanza a))
    |> rev
end

(* We need to instantiate Install_rules earlier to avoid issues whenever
 * Super_context is used too soon.
 * See: https://github.com/ocaml/dune/pull/1354#issuecomment-427922592 *)

let with_format sctx ~dir ~f =
  Super_context.find_scope_by_dir sctx dir
  |> Scope.project |> Dune_project.format_config |> Option.iter ~f

let gen_format_rules sctx ~expander ~output_dir =
  let scope = Super_context.find_scope_by_dir sctx output_dir in
  let project = Scope.project scope in
  let dialects = Dune_project.dialects project in
  with_format sctx ~dir:output_dir
    ~f:(Format_rules.gen_rules_output sctx ~dialects ~expander ~output_dir)

(* This is used to determine the list of source directories to give to Merlin.
   This serves the same purpose as [Merlin.lib_src_dirs] and has a similar
   implementation, but this definition is used for the current library, while
   [Merlin.lib_src_dirs] is used for the dependencies. It would be nice to unify
   them at some point. *)
let lib_src_dirs ~dir_contents =
  Dir_contents.dirs dir_contents
  |> List.map ~f:(fun dc ->
         Path.Build.drop_build_context_exn (Dir_contents.dir dc))

(* Stanza *)

let gen_rules sctx dir_contents cctxs
    { Dir_with_dune.src_dir; ctx_dir; data = stanzas; scope; dune_version = _ }
    =
  let expander = Super_context.expander sctx ~dir:ctx_dir in
  let expander =
    let lookup_module ~dir name =
      Dir_contents.Dir_artifacts.lookup_module
        (Dir_contents.artifacts (Dir_contents.get sctx ~dir))
        name
    in
    let lookup_library ~dir name =
      Dir_contents.Dir_artifacts.lookup_library
        (Dir_contents.artifacts (Dir_contents.get sctx ~dir))
        name
    in
    Expander.set_lookup_library
      (Expander.set_lookup_module expander ~lookup_module)
      ~lookup_library
  in
  let files_to_install { Install_conf.section = _; files; package = _ } =
    List.map files ~f:(fun fb ->
        File_binding.Unexpanded.expand_src ~dir:ctx_dir fb
          ~f:(Expander.expand_str expander)
        |> Path.build)
    |> Path.Set.of_list
    |> Rules.Produce.Alias.add_deps (Alias.all ~dir:ctx_dir)
  in
  let { For_stanza.merlin = merlins
      ; cctx = cctxs
      ; js = js_targets
      ; source_dirs
      } =
    For_stanza.of_stanzas stanzas ~cctxs ~sctx ~src_dir ~ctx_dir ~scope
      ~dir_contents ~expander ~files_to_install
  in
  let allow_approx_merlin =
    let dune_project = Scope.project scope in
    let dir_is_vendored = Super_context.dir_is_vendored src_dir in
    dir_is_vendored || Dune_project.allow_approx_merlin dune_project
  in
  Option.iter (Merlin.merge_all ~allow_approx_merlin merlins) ~f:(fun m ->
      let more_src_dirs =
        lib_src_dirs ~dir_contents |> List.rev_append source_dirs
      in
      Merlin.add_rules sctx ~dir:ctx_dir ~more_src_dirs ~expander
        (Merlin.add_source_dir m src_dir));
  let build_dir = Super_context.build_dir sctx in
  List.iter stanzas ~f:(fun stanza ->
      match (stanza : Stanza.t) with
      | Menhir.T m when Expander.eval_blang expander m.enabled_if -> (
        match
          List.find_map (Menhir_rules.module_names m) ~f:(fun name ->
              Option.bind (Dir_contents.lookup_module dir_contents name)
                ~f:(fun buildable ->
                  List.find_map cctxs ~f:(fun (loc, cctx) ->
                      Option.some_if (Loc.equal loc buildable.loc) cctx)))
        with
        | None ->
          (* This happens often when passing a [-p ...] option that hides a
             library *)
          let targets =
            List.map (Menhir_rules.targets m) ~f:(Path.Build.relative ctx_dir)
          in
          Super_context.add_rule sctx ~dir:ctx_dir
            ( Build.fail
                { fail =
                    (fun () ->
                      User_error.raise ~loc:m.loc
                        [ Pp.text
                            "I can't determine what library/executable the \
                             files produced by this stanza are part of."
                        ])
                }
            |> Build.with_targets ~targets )
        | Some cctx -> Menhir_rules.gen_rules cctx m ~build_dir ~dir:ctx_dir )
      | Coq.T m when Expander.eval_blang expander m.enabled_if ->
        Coq_rules.setup_rules ~sctx ~dir:ctx_dir ~dir_contents m
        |> Super_context.add_rules ~dir:ctx_dir sctx
      | Coqpp.T m ->
        Coq_rules.coqpp_rules ~sctx ~build_dir ~dir:ctx_dir m
        |> Super_context.add_rules ~dir:ctx_dir sctx
      | _ -> ());
  let dyn_deps =
    let pred =
      let id =
        lazy
          (let open Dyn.Encoder in
          constr "exclude"
            (List.map ~f:(fun p -> Path.Build.to_dyn p) js_targets))
      in
      List.iter js_targets ~f:(fun js_target ->
          assert (Path.Build.equal (Path.Build.parent_exn js_target) ctx_dir));
      let f =
        if Dune_project.explicit_js_mode (Scope.project scope) then
          fun _ ->
        true
        else
          fun basename ->
        not
          (List.exists js_targets ~f:(fun js_target ->
               String.equal (Path.Build.basename js_target) basename))
      in
      Predicate.create ~id ~f
    in
    File_selector.create ~dir:(Path.build ctx_dir) pred
    |> Build.paths_matching ~loc:Loc.none
  in
  Rules.Produce.Alias.add_deps ~dyn_deps (Alias.all ~dir:ctx_dir) Path.Set.empty;
  cctxs

let gen_rules sctx dir_contents cctxs ~dir :
    (Loc.t * Compilation_context.t) list =
  with_format sctx ~dir ~f:(fun _ -> Format_rules.gen_rules ~dir);
  match Super_context.stanzas_in sctx ~dir with
  | None -> []
  | Some d -> gen_rules sctx dir_contents cctxs d

let gen_rules ~sctx ~dir components :
    Build_system.extra_sub_directories_to_keep Fiber.t =
  Install_rules.meta_and_dune_package_rules sctx ~dir;
  let subdirs_to_keep1 = Install_rules.gen_rules sctx ~dir in
  Opam_create.add_rules sctx ~dir;
  let subdirs_to_keep2 : Build_system.extra_sub_directories_to_keep Fiber.t =
    let open Fiber.O in
    match components with
    | ".js" :: rest ->
      Js_of_ocaml_rules.setup_separate_compilation_rules sctx rest;
      Fiber.return
        ( match rest with
        | [] -> Build_system.Subdir_set.All
        | _ -> These String.Set.empty )
    | "_doc" :: rest -> (
      let+ () = Odoc.gen_rules sctx rest ~dir in
      match rest with
      | [] -> Build_system.Subdir_set.All
      | _ -> These String.Set.empty )
    | ".ppx" :: rest ->
      Preprocessing.gen_rules sctx rest;
      Fiber.return
        ( match rest with
        | [] -> Build_system.Subdir_set.All
        | _ -> These String.Set.empty )
    | comps ->
      let subdirs = [ ".formatted"; ".bin"; ".utop" ] in
      let open Fiber.O in
      let+ () =
        match List.last comps with
        | Some ".formatted" ->
          let expander = Super_context.expander sctx ~dir in
          Fiber.return (gen_format_rules sctx ~expander ~output_dir:dir)
        | Some ".bin" ->
          let src_dir = Path.Build.parent_exn dir in
          Super_context.local_binaries sctx ~dir:src_dir
          |> List.iter ~f:(fun t ->
                 let loc = File_binding.Expanded.src_loc t in
                 let src = Path.build (File_binding.Expanded.src t) in
                 let dst = File_binding.Expanded.dst_path t ~dir in
                 Super_context.add_rule sctx ~loc ~dir (Build.symlink ~src ~dst))
          |> Fiber.return
        | _ -> (
          match File_tree.find_dir (Path.Build.drop_build_context_exn dir) with
          | None ->
            (* We get here when [dir] is a generated directory, such as [.utop]
               or [.foo.objs]. *)
            if Utop.is_utop_dir dir then
              Fiber.return (Utop.setup sctx ~dir:(Path.Build.parent_exn dir))
            else if components <> [] then
              Build_system.load_dir ~dir:(Path.parent_exn (Path.build dir))
            else
              Fiber.return ()
          | Some _ -> (
            (* This interprets "rule" and "copy_files" stanzas. *)
            let* contents = Dir_contents.gen_rules sctx ~dir in
            match contents with
            | Group_part root -> Build_system.load_dir ~dir:(Path.build root)
            | Standalone_or_root (dir_contents, subs) ->
              let cctxs = gen_rules sctx dir_contents [] ~dir in
              List.iter subs ~f:(fun dc ->
                  ignore
                    ( gen_rules sctx dir_contents cctxs
                        ~dir:(Dir_contents.dir dc)
                      : _ list ))
              |> Fiber.return ) )
      in
      Build_system.Subdir_set.These (String.Set.of_list subdirs)
  in
  let subdirs_to_keep3 =
    match components with
    | [] ->
      Build_system.Subdir_set.These
        (String.Set.of_list [ ".js"; "_doc"; ".ppx" ])
    | _ -> These String.Set.empty
  in
  let open Fiber.O in
  let+ subdirs_to_keep2 = subdirs_to_keep2 in
  Build_system.Subdir_set.union_all
    [ subdirs_to_keep1; subdirs_to_keep2; subdirs_to_keep3 ]

let filter_out_stanzas_from_hidden_packages ~visible_pkgs =
  List.filter_map ~f:(fun stanza ->
      match Dune_file.stanza_package stanza with
      | None -> Some stanza
      | Some package -> (
        if Package.Name.Map.mem visible_pkgs package.name then
          Some stanza
        else
          (* If the stanza is a hidden public implementation of a visible
             library, turn it into an external variant so that we can correctly
             compute the set of "knonw implementation" when generating the
             dune-package file. *)
          match stanza with
          | Library
              { public = Some { name; _ }
              ; variant = Some variant
              ; implements = Some virtual_lib
              ; project
              ; buildable = { loc; _ }
              ; _
              }
            when Package.Name.Map.mem visible_pkgs
                   (Lib_name.package_name (snd virtual_lib)) ->
            Some
              (External_variant
                 { implementation = name; virtual_lib; variant; project; loc })
          | _ -> None ))

let gen ~contexts ?(external_lib_deps_mode = false) ?only_packages conf =
  let open Fiber.O in
  let { Dune_load.dune_files; packages; projects } = conf in
  let packages = Option.value only_packages ~default:packages in
  let sctxs = Table.create (module Context_name) 4 in
  List.iter contexts ~f:(fun c ->
      Table.add_exn sctxs c.Context.name (Fiber.Ivar.create ()));
  let make_sctx (context : Context.t) : _ Fiber.t =
    let host () =
      match context.for_host with
      | None -> Fiber.return None
      | Some h -> Fiber.Ivar.read (Table.find_exn sctxs h.name) >>| Option.some
    in
    let stanzas () =
      let+ stanzas = Dune_load.Dune_files.eval ~context dune_files in
      match only_packages with
      | None -> stanzas
      | Some visible_pkgs ->
        List.map stanzas ~f:(fun (dir_conf : Dune_load.Dune_file.t) ->
            { dir_conf with
              stanzas =
                filter_out_stanzas_from_hidden_packages ~visible_pkgs
                  dir_conf.stanzas
            })
    in
    let* host, stanzas = Fiber.fork_and_join host stanzas in
    let sctx =
      Super_context.create ?host ~context ~projects ~packages
        ~external_lib_deps_mode ~stanzas
    in
    let+ () = Fiber.Ivar.fill (Table.find_exn sctxs context.name) sctx in
    (context.name, sctx)
  in
  let+ contexts = Fiber.parallel_map contexts ~f:make_sctx in
  let sctxs = Context_name.Map.of_list_exn contexts in
  let () =
    Build_system.set_packages (fun path ->
        let open Option.O in
        Option.value ~default:Package.Name.Set.empty
          (let* ctx_name, _ = Path.Build.extract_build_context path in
           let* ctx_name = Context_name.of_string_opt ctx_name in
           let* sctx = Context_name.Map.find sctxs ctx_name in
           Path.Build.Map.find (Install_rules.packages sctx) path))
  in
  Build_system.set_rule_generators
    ~init:(fun () ->
      Context_name.Map.to_list sctxs
      |> Fiber.parallel_iter ~f:(fun (_, sctx) ->
             Odoc.init sctx |> Fiber.return))
    ~gen_rules:(function
      | Install ctx ->
        Option.map (Context_name.Map.find sctxs ctx) ~f:(fun sctx ~dir _ ->
            Fiber.return (Install_rules.gen_rules sctx ~dir))
      | Context ctx ->
        Context_name.Map.find sctxs ctx
        |> Option.map ~f:(fun sctx -> gen_rules ~sctx));
  sctxs
