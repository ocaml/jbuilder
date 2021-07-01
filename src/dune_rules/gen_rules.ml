open! Dune_engine
open! Stdune
open Import
module Menhir_rules = Menhir
module Toplevel_rules = Toplevel.Stanza
open Dune_file
open! No_io
open Memo.Build.O

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
    -> files_to_install:(Install_conf.t -> unit Memo.Build.t)
    -> ( Merlin.t list
       , (Loc.t * Compilation_context.t) list
       , Path.Build.t list
       , Path.Source.t list )
       t
       Memo.Build.t
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
      let+ () = Toplevel_rules.setup ~sctx ~dir ~toplevel in
      empty_none
    | Library lib ->
      let* available =
        Lib.DB.available (Scope.libs scope) (Dune_file.Library.best_name lib)
      in
      if available then
        let+ cctx, merlin =
          Lib_rules.rules lib ~sctx ~dir ~scope ~dir_contents ~expander
        in
        { merlin = Some merlin
        ; cctx = Some (lib.buildable.loc, cctx)
        ; js = None
        ; source_dirs = None
        }
      else
        Memo.Build.return empty_none
    | Foreign_library lib ->
      let+ () =
        Lib_rules.foreign_rules lib ~sctx ~dir ~dir_contents ~expander
      in
      empty_none
    | Executables exes -> (
      Expander.eval_blang expander exes.enabled_if >>= function
      | false -> Memo.Build.return empty_none
      | true ->
        let* () =
          Memo.Build.Option.iter exes.install_conf ~f:files_to_install
        in
        let+ cctx, merlin =
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
        })
    | Alias alias ->
      let+ () = Simple_rules.alias sctx alias ~dir ~expander in
      empty_none
    | Tests tests ->
      let+ cctx, merlin =
        Test_rules.rules tests ~sctx ~dir ~scope ~expander ~dir_contents
      in
      { merlin = Some merlin
      ; cctx = Some (tests.exes.buildable.loc, cctx)
      ; js = None
      ; source_dirs = None
      }
    | Copy_files { files = glob; _ } ->
      let* source_dirs =
        let loc = String_with_vars.loc glob in
        let+ src_glob = Expander.No_deps.expand_str expander glob in
        if Filename.is_relative src_glob then
          Some
            (Path.Source.relative src_dir src_glob ~error_loc:loc
            |> Path.Source.parent_exn)
        else
          None
      in
      Memo.Build.return { merlin = None; cctx = None; js = None; source_dirs }
    | Install i ->
      let+ () = files_to_install i in
      empty_none
    | Plugin p ->
      let+ () = Plugin_rules.setup_rules ~sctx ~dir p in
      empty_none
    | Cinaps.T cinaps ->
      let+ () = Cinaps.gen_rules sctx cinaps ~dir ~scope in
      empty_none
    | Mdx.T mdx -> (
      Expander.eval_blang expander (Mdx.enabled_if mdx) >>= function
      | false -> Memo.Build.return empty_none
      | true ->
        let+ () = Mdx.gen_rules ~sctx ~dir ~scope ~expander mdx in
        empty_none)
    | _ -> Memo.Build.return empty_none

  let of_stanzas stanzas ~cctxs ~sctx ~src_dir ~ctx_dir ~scope ~dir_contents
      ~expander ~files_to_install =
    let of_stanza =
      of_stanza ~sctx ~src_dir ~ctx_dir ~scope ~dir_contents ~expander
        ~files_to_install
    in
    let+ l = Memo.Build.parallel_map stanzas ~f:of_stanza in
    List.fold_left l ~init:{ empty_list with cctx = cctxs } ~f:(fun acc x ->
        cons acc x)
    |> rev
end

(* We need to instantiate Install_rules earlier to avoid issues whenever
 * Super_context is used too soon.
 * See: https://github.com/ocaml/dune/pull/1354#issuecomment-427922592 *)

let with_format sctx ~dir ~f =
  let* config = Super_context.format_config sctx ~dir in
  Memo.Build.when_ (not (Format_config.is_empty config)) (fun () -> f config)

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
  let deps =
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
    let only_generated_files =
      Dune_project.dune_version (Scope.project scope) >= (3, 0)
    in
    File_selector.create ~dir:(Path.build dir) ~only_generated_files pred
    |> Action_builder.paths_matching_unit ~loc:Loc.none
  in
  Rules.Produce.Alias.add_deps (Alias.all ~dir) deps

let gen_rules sctx dir_contents cctxs expander
    { Dir_with_dune.src_dir; ctx_dir; data = stanzas; scope; dune_version = _ }
    =
  let files_to_install
      { Install_conf.section = _; files; package = _; enabled_if = _ } =
    Memo.Build.List.map files ~f:(fun fb ->
        File_binding.Unexpanded.expand_src ~dir:ctx_dir fb
          ~f:(Expander.No_deps.expand_str expander)
        >>| Path.build)
    >>= fun files ->
    Rules.Produce.Alias.add_deps (Alias.all ~dir:ctx_dir)
      (Action_builder.paths files)
  in
  let* { For_stanza.merlin = merlins
       ; cctx = cctxs
       ; js = js_targets
       ; source_dirs
       } =
    For_stanza.of_stanzas stanzas ~cctxs ~sctx ~src_dir ~ctx_dir ~scope
      ~dir_contents ~expander ~files_to_install
  in
  let* () =
    Memo.Build.sequential_iter merlins ~f:(fun merlin ->
        let more_src_dirs =
          lib_src_dirs ~dir_contents |> List.rev_append (src_dir :: source_dirs)
        in
        Merlin.add_rules sctx ~dir:ctx_dir ~more_src_dirs ~expander merlin)
  in
  let* () =
    Memo.Build.parallel_iter stanzas ~f:(fun stanza ->
        match (stanza : Stanza.t) with
        | Menhir.T m -> (
          Expander.eval_blang expander m.enabled_if >>= function
          | false -> Memo.Build.return ()
          | true -> (
            let* ml_sources = Dir_contents.ocaml dir_contents in
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
                List.map (Menhir_rules.targets m)
                  ~f:(Path.Build.relative ctx_dir)
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
            | Some cctx -> Menhir_rules.gen_rules cctx m ~dir:ctx_dir))
        | Coq_stanza.Theory.T m -> (
          Expander.eval_blang expander m.enabled_if >>= function
          | false -> Memo.Build.return ()
          | true ->
            Coq_rules.setup_rules ~sctx ~dir:ctx_dir ~dir_contents m
            >>= Super_context.add_rules ~dir:ctx_dir sctx)
        | Coq_stanza.Extraction.T m ->
          Coq_rules.extraction_rules ~sctx ~dir:ctx_dir ~dir_contents m
          >>= Super_context.add_rules ~dir:ctx_dir sctx
        | Coq_stanza.Coqpp.T m ->
          Coq_rules.coqpp_rules ~sctx ~dir:ctx_dir m
          >>= Super_context.add_rules ~dir:ctx_dir sctx
        | _ -> Memo.Build.return ())
  in
  let+ () = define_all_alias ~dir:ctx_dir ~scope ~js_targets in
  cctxs

let gen_rules sctx dir_contents cctxs ~source_dir ~dir :
    (Loc.t * Compilation_context.t) list Memo.Build.t =
  let* () = with_format sctx ~dir ~f:(fun _ -> Format_rules.gen_rules ~dir) in
  let* expander =
    let+ expander = Super_context.expander sctx ~dir in
    Dir_contents.add_sources_to_expander sctx expander
  and* tests = Source_tree.Dir.cram_tests source_dir in
  let* () = Cram_rules.rules ~sctx ~expander ~dir tests in
  match Super_context.stanzas_in sctx ~dir with
  | Some d -> gen_rules sctx dir_contents cctxs expander d
  | None ->
    let+ () =
      define_all_alias ~dir ~js_targets:[]
        ~scope:(Super_context.find_scope_by_dir sctx dir)
    in
    []

let gen_rules ~sctx ~dir components =
  let module S = Build_system.Subdir_set in
  let* () = Opam_create.add_rules sctx ~dir in
  let* () = Install_rules.meta_and_dune_package_rules sctx ~dir in
  let+ subdirs_to_keep1 = Install_rules.gen_rules sctx ~dir
  and+ (subdirs_to_keep2 : Build_system.extra_sub_directories_to_keep) =
    match components with
    | [ ".dune"; "ccomp" ] ->
      (* Add rules for C compiler detection *)
      let+ () = Cxx_rules.rules ~sctx ~dir in
      S.These String.Set.empty
    | ".dune" :: _ -> Memo.Build.return (S.These String.Set.empty)
    | ".js" :: rest -> (
      let+ () = Jsoo_rules.setup_separate_compilation_rules sctx rest in
      match rest with
      | [] -> S.All
      | _ -> S.These String.Set.empty)
    | "_doc" :: rest -> (
      let+ () = Odoc.gen_rules sctx rest ~dir in
      match rest with
      | [] -> S.All
      | _ -> S.These String.Set.empty)
    | ".ppx" :: rest -> (
      let+ () = Preprocessing.gen_rules sctx rest in
      match rest with
      | [] -> S.All
      | _ -> S.These String.Set.empty)
    | comps ->
      let subdirs = [ ".formatted"; ".bin"; ".utop" ] in
      let+ () =
        match List.last comps with
        | Some ".formatted" ->
          let* expander = Super_context.expander sctx ~dir in
          gen_format_rules sctx ~expander ~output_dir:dir
        | Some ".bin" ->
          let src_dir = Path.Build.parent_exn dir in
          let* local_binaries =
            Super_context.local_binaries sctx ~dir:src_dir
          in
          Memo.Build.sequential_iter local_binaries ~f:(fun t ->
              let loc = File_binding.Expanded.src_loc t in
              let src = Path.build (File_binding.Expanded.src t) in
              let dst = File_binding.Expanded.dst_path t ~dir in
              Super_context.add_rule sctx ~loc ~dir
                (Action_builder.symlink ~src ~dst))
        | _ -> (
          Source_tree.find_dir (Path.Build.drop_build_context_exn dir)
          >>= function
          | None ->
            (* We get here when [dir] is a generated directory, such as [.utop]
               or [.foo.objs]. *)
            if Utop.is_utop_dir dir then
              Utop.setup sctx ~dir:(Path.Build.parent_exn dir)
            else if components <> [] then
              Build_system.load_dir ~dir:(Path.parent_exn (Path.build dir))
            else
              Memo.Build.return ()
          | Some source_dir -> (
            (* This interprets "rule" and "copy_files" stanzas. *)
            Dir_contents.gen_rules sctx ~dir
            >>= function
            | Group_part root -> Build_system.load_dir ~dir:(Path.build root)
            | Standalone_or_root (dir_contents, subs) ->
              let* cctxs = gen_rules sctx dir_contents [] ~source_dir ~dir in
              Memo.Build.parallel_iter subs ~f:(fun dc ->
                  gen_rules sctx dir_contents cctxs ~source_dir
                    ~dir:(Dir_contents.dir dc)
                  >>| ignore)))
      in
      S.These (String.Set.of_list subdirs)
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

let gen_rules ~sctx ~dir components =
  let module S = Build_system.Subdir_set in
  match components with
  | [ ".dune" ] ->
    (* [.dune] is treated specifically as generating the rules in all other
       directories forces the production of the configurator files for which the
       rules are setup in this branch. *)
    let+ () = Context.gen_configurator_rules (Super_context.context sctx) in
    S.These (String.Set.of_list [ "ccomp" ])
  | _ ->
    let* () = Memo.Lazy.force Context.force_configurator_files in
    gen_rules ~sctx ~dir components

let global_rules =
  Memo.lazy_ (fun () ->
      Rules.collect_unit (fun () ->
          let* sctxs = Memo.Lazy.force Super_context.all in
          Memo.Build.parallel_iter
            (Context_name.Map.values sctxs)
            ~f:Odoc.global_rules))

let gen_rules ctx_or_install ~dir components =
  match (ctx_or_install : Build_system.Context_or_install.t) with
  | Install ctx ->
    Super_context.find ctx
    >>= Memo.Build.Option.map ~f:(fun sctx ->
            Rules.collect (fun () -> Install_rules.gen_rules sctx ~dir))
  | Context ctx ->
    Super_context.find ctx
    >>= Memo.Build.Option.map ~f:(fun sctx ->
            Rules.collect (fun () -> gen_rules ~sctx ~dir components))
