open! Dune_engine
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
    -> files_to_install:(Install_conf.t -> unit)
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
        (match x.js with
        | None -> acc.js
        | Some js -> List.rev_append acc.js js)
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
    | Executables exes when Expander.eval_blang expander exes.enabled_if ->
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
    | Copy_files { files = glob; _ } ->
      let source_dirs =
        let loc = String_with_vars.loc glob in
        let src_glob = Expander.expand_str expander glob in
        if Filename.is_relative src_glob then
          Some
            (Path.Source.relative src_dir src_glob ~error_loc:loc
            |> Path.Source.parent_exn)
        else
          None
      in
      { merlin = None; cctx = None; js = None; source_dirs }
    | Install i ->
      files_to_install i;
      empty_none
    | Plugin p ->
      Plugin_rules.setup_rules ~sctx ~dir p;
      empty_none
    | Cinaps.T cinaps ->
      Cinaps.gen_rules sctx cinaps ~dir ~scope;
      empty_none
    | Mdx.T mdx ->
      Mdx.gen_rules ~sctx ~dir ~expander mdx;
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
  let f config = if not (Format_config.is_empty config) then f config in
  Super_context.format_config sctx ~dir |> f

let gen_format_rules sctx ~expander ~output_dir =
  let scope = Super_context.find_scope_by_dir sctx output_dir in
  let project = Scope.project scope in
  let dialects = Dune_project.dialects project in
  let version = Dune_project.dune_version project in
  with_format sctx ~dir:output_dir
    ~f:
      (Format_rules.gen_rules_output sctx ~version ~dialects ~expander
         ~output_dir)

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

let define_all_alias ~dir ~scope ~js_targets =
  let dyn_deps =
    let pred =
      let id =
        lazy
          (let open Dyn.Encoder in
          constr "exclude"
            (List.map ~f:(fun p -> Path.Build.to_dyn p) js_targets))
      in
      List.iter js_targets ~f:(fun js_target ->
          assert (Path.Build.equal (Path.Build.parent_exn js_target) dir));
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
    File_selector.create ~dir:(Path.build dir) pred
    |> Action_builder.paths_matching ~loc:Loc.none
  in
  Rules.Produce.Alias.add_deps ~dyn_deps (Alias.all ~dir) Path.Set.empty

let gen_rules sctx dir_contents cctxs expander
    { Dir_with_dune.src_dir; ctx_dir; data = stanzas; scope; dune_version = _ }
    =
  let files_to_install
      { Install_conf.section = _; files; package = _; enabled_if = _ } =
    Path.Set.of_list_map files ~f:(fun fb ->
        File_binding.Unexpanded.expand_src ~dir:ctx_dir fb
          ~f:(Expander.expand_str expander)
        |> Path.build)
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
  List.iter merlins ~f:(fun merlin ->
      let more_src_dirs =
        lib_src_dirs ~dir_contents |> List.rev_append (src_dir :: source_dirs)
      in
      Merlin.add_rules sctx ~dir:ctx_dir ~more_src_dirs ~expander merlin);
  List.iter stanzas ~f:(fun stanza ->
      match (stanza : Stanza.t) with
      | Menhir.T m when Expander.eval_blang expander m.enabled_if -> (
        let ml_sources = Dir_contents.ocaml dir_contents in
        match
          List.find_map (Menhir_rules.module_names m) ~f:(fun name ->
              Option.bind (Ml_sources.lookup_module ml_sources name)
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
            (Action_builder.fail
               { fail =
                   (fun () ->
                     User_error.raise ~loc:m.loc
                       [ Pp.text
                           "I can't determine what library/executable the \
                            files produced by this stanza are part of."
                       ])
               }
            |> Action_builder.with_targets ~targets)
        | Some cctx -> Menhir_rules.gen_rules cctx m ~dir:ctx_dir)
      | Coq_stanza.Theory.T m when Expander.eval_blang expander m.enabled_if ->
        Coq_rules.setup_rules ~sctx ~dir:ctx_dir ~dir_contents m
        |> Super_context.add_rules ~dir:ctx_dir sctx
      | Coq_stanza.Extraction.T m ->
        Coq_rules.extraction_rules ~sctx ~dir:ctx_dir ~dir_contents m
        |> Super_context.add_rules ~dir:ctx_dir sctx
      | Coq_stanza.Coqpp.T m ->
        Coq_rules.coqpp_rules ~sctx ~dir:ctx_dir m
        |> Super_context.add_rules ~dir:ctx_dir sctx
      | _ -> ());
  define_all_alias ~dir:ctx_dir ~scope ~js_targets;
  cctxs

let gen_rules sctx dir_contents cctxs ~source_dir ~dir :
    (Loc.t * Compilation_context.t) list =
  with_format sctx ~dir ~f:(fun _ -> Format_rules.gen_rules ~dir);
  let expander =
    let expander = Super_context.expander sctx ~dir in
    Dir_contents.add_sources_to_expander sctx expander
  in
  (let tests = File_tree.Dir.cram_tests source_dir in
   Cram_rules.rules ~sctx ~expander ~dir tests);
  match Super_context.stanzas_in sctx ~dir with
  | Some d -> gen_rules sctx dir_contents cctxs expander d
  | None ->
    define_all_alias ~dir ~js_targets:[]
      ~scope:(Super_context.find_scope_by_dir sctx dir);
    []

let gen_rules ~sctx ~dir components : Build_system.extra_sub_directories_to_keep
    =
  Install_rules.meta_and_dune_package_rules sctx ~dir;
  let subdirs_to_keep1 = Install_rules.gen_rules sctx ~dir in
  Opam_create.add_rules sctx ~dir;
  let subdirs_to_keep2 : Build_system.extra_sub_directories_to_keep =
    match components with
    | ".dune" :: _ ->
      (* Dummy rule to prevent dune from deleting this file. See comment
         attached to [write_dot_dune_dir] in context.ml *)
      Super_context.add_rule sctx ~dir
        (Action_builder.write_file (Path.Build.relative dir "configurator") "");
      (* Add rules for C compiler detection *)
      Cxx_rules.rules ~sctx ~dir;
      These String.Set.empty
    | ".js" :: rest -> (
      Jsoo_rules.setup_separate_compilation_rules sctx rest;
      match rest with
      | [] -> All
      | _ -> These String.Set.empty)
    | "_doc" :: rest -> (
      Odoc.gen_rules sctx rest ~dir;
      match rest with
      | [] -> All
      | _ -> These String.Set.empty)
    | ".ppx" :: rest -> (
      Preprocessing.gen_rules sctx rest;
      match rest with
      | [] -> All
      | _ -> These String.Set.empty)
    | comps ->
      let subdirs = [ ".formatted"; ".bin"; ".utop" ] in
      (match List.last comps with
      | Some ".formatted" ->
        let expander = Super_context.expander sctx ~dir in
        gen_format_rules sctx ~expander ~output_dir:dir
      | Some ".bin" ->
        let src_dir = Path.Build.parent_exn dir in
        Super_context.local_binaries sctx ~dir:src_dir
        |> List.iter ~f:(fun t ->
               let loc = File_binding.Expanded.src_loc t in
               let src = Path.build (File_binding.Expanded.src t) in
               let dst = File_binding.Expanded.dst_path t ~dir in
               Super_context.add_rule sctx ~loc ~dir
                 (Action_builder.symlink ~src ~dst))
      | _ -> (
        match File_tree.find_dir (Path.Build.drop_build_context_exn dir) with
        | None ->
          (* We get here when [dir] is a generated directory, such as [.utop] or
             [.foo.objs]. *)
          if Utop.is_utop_dir dir then
            Utop.setup sctx ~dir:(Path.Build.parent_exn dir)
          else if components <> [] then
            Build_system.load_dir ~dir:(Path.parent_exn (Path.build dir))
        | Some source_dir -> (
          (* This interprets "rule" and "copy_files" stanzas. *)
          match Dir_contents.gen_rules sctx ~dir with
          | Group_part root -> Build_system.load_dir ~dir:(Path.build root)
          | Standalone_or_root (dir_contents, subs) ->
            let cctxs = gen_rules sctx dir_contents [] ~source_dir ~dir in
            List.iter subs ~f:(fun dc ->
                ignore
                  (gen_rules sctx dir_contents cctxs ~source_dir
                     ~dir:(Dir_contents.dir dc)
                    : _ list)))));
      These (String.Set.of_list subdirs)
  in
  let subdirs_to_keep3 =
    match components with
    | [] ->
      Build_system.Subdir_set.These
        (String.Set.of_list [ ".js"; "_doc"; ".ppx"; ".dune" ])
    | _ -> These String.Set.empty
  in
  Build_system.Subdir_set.union_all
    [ subdirs_to_keep1; subdirs_to_keep2; subdirs_to_keep3 ]

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
        | Library l ->
          let open Option.O in
          let+ redirect = Dune_file.Library_redirect.Local.of_private_lib l in
          Dune_file.Library_redirect redirect
        | _ -> None)

let gen ~contexts ?only_packages conf =
  let open Memo.Build.O in
  let { Dune_load.dune_files; packages; projects; vcs } = conf in
  let packages = Option.value only_packages ~default:packages in
  let rec sctxs =
    (* This lazy is just here for the need of [let rec]. We force it straight
       away, so it is safe regarding [Memo]. *)
    lazy
      (Context_name.Map.of_list_map_exn contexts ~f:(fun (c : Context.t) ->
           (c.name, Memo.Lazy.Async.create (fun () -> make_sctx c))))
  and make_sctx (context : Context.t) =
    let host () =
      match context.for_host with
      | None -> Memo.Build.return None
      | Some h ->
        let+ sctx =
          Memo.Lazy.Async.force
            (Context_name.Map.find_exn (Lazy.force sctxs) h.name)
        in
        Some sctx
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
    let+ host, stanzas = Memo.Build.fork_and_join host stanzas in
    let sctx =
      Super_context.create ?host ~context ~projects ~packages ~stanzas ()
    in
    sctx
  in
  let* sctxs =
    Lazy.force sctxs |> Context_name.Map.to_list
    |> Memo.Build.parallel_map ~f:(fun (name, sctx) ->
           let+ sctx = Memo.Lazy.Async.force sctx in
           (name, sctx))
    >>| Context_name.Map.of_list_exn
  in
  let () =
    Build_system.set_packages (fun path ->
        let open Option.O in
        Option.value ~default:Package.Id.Set.empty
          (let* ctx_name, _ = Path.Build.extract_build_context path in
           let* ctx_name = Context_name.of_string_opt ctx_name in
           let* sctx = Context_name.Map.find sctxs ctx_name in
           Path.Build.Map.find (Install_rules.packages sctx) path))
  in
  Build_system.set_rule_generators
    ~init:(fun () -> Context_name.Map.iter sctxs ~f:Odoc.init)
    ~gen_rules:(function
      | Install ctx ->
        Option.map (Context_name.Map.find sctxs ctx) ~f:(fun sctx ~dir _ ->
            Install_rules.gen_rules sctx ~dir)
      | Context ctx ->
        Context_name.Map.find sctxs ctx
        |> Option.map ~f:(fun sctx -> gen_rules ~sctx));
  let+ () = Build_system.set_vcs vcs in
  sctxs
