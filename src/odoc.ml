open! Stdune
open Import
open Dune_file
open Build.O

module SC = Super_context

let (++) = Path.relative

let lib_unique_name lib =
  let name = Lib.name lib in
  match Lib.status lib with
  | Installed -> assert false
  | Public _  -> Lib_name.to_string name
  | Private scope_name ->
    SC.Scope_key.to_string (Lib_name.to_string name) scope_name

let pkg_or_lnu lib =
  match Lib.package lib with
  | Some p -> Package.Name.to_string p
  | None -> lib_unique_name lib

type target =
  | Lib of Lib.t
  | Pkg of Package.Name.t

type source = Module | Mld

type odoc =
  { odoc_input: Path.t
  ; html_dir: Path.t
  ; html_file: Path.t
  ; source: source
  }

let add_rule sctx =
  Super_context.add_rule sctx ~dir:(Super_context.build_dir sctx)

module Paths = struct
  let root (context : Context.t) =
    context.Context.build_dir ++ "_doc"

  let odocs ctx m =
    root ctx ++ (
      match m with
      | Lib lib -> sprintf "_odoc/lib/%s" (lib_unique_name lib)
      | Pkg pkg -> sprintf "_odoc/pkg/%s" (Package.Name.to_string pkg)
    )

  let html_root ctx = root ctx ++ "_html"

  let html ctx m =
    html_root ctx ++ (
      match m with
      | Pkg pkg -> Package.Name.to_string pkg
      | Lib lib -> pkg_or_lnu lib
    )

  let gen_mld_dir ctx pkg =
    root ctx ++ "_mlds" ++ (Package.Name.to_string pkg)

  let css_file ctx = html_root ctx ++ "odoc.css"
  let highlight_pack_js ctx = html_root ctx ++ "highlight.pack.js"

  let toplevel_index ctx = html_root ctx ++ "index.html"
end

module Deps = struct
  let html_alias ctx m =
    Alias.doc ~dir:(Paths.html ctx m)

  let alias = Alias.make ".odoc-all"

  let deps ctx requires =
    Build.of_result_map requires ~f:(fun libs ->
      Build.deps (
        List.fold_left libs ~init:Dep.Set.empty ~f:(fun acc (lib : Lib.t) ->
          if Lib.is_local lib then
            let dir = Paths.odocs ctx (Lib lib) in
            let alias = alias ~dir in
            Dep.Set.add acc (Dep.alias alias)
          else
            acc)))

  let alias ctx m = alias ~dir:(Paths.odocs ctx m)

  (* let static_deps t lib = Build_system.Alias.dep (alias t lib) *)

  let setup_deps ctx m deps = Build_system.Alias.add_deps (alias ctx m) deps
end

let odoc_ext = ".odoc"

module Mld : sig
  type t

  val create : Path.t -> t

  val odoc_file : doc_dir:Path.t -> t -> Path.t
  val odoc_input : t -> Path.t

end = struct
  type t = Path.t

  let create p = p

  let odoc_file ~doc_dir t =
    let t = Filename.chop_extension (Path.basename t) in
    Path.relative doc_dir (sprintf "page-%s%s" t odoc_ext)

  let odoc_input t = t
end

let odoc sctx =
  SC.resolve_program sctx ~dir:(Super_context.build_dir sctx) "odoc"
    ~loc:None ~hint:"try: opam install odoc"

let module_deps (m : Module.t) ~doc_dir ~(dep_graphs:Dep_graph.Ml_kind.t) =
  (if Module.has_intf m then
     Dep_graph.deps_of dep_graphs.intf m
   else
     (* When a module has no .mli, use the dependencies for the .ml *)
     Dep_graph.deps_of dep_graphs.impl m)
  >>^ List.map ~f:(Module.odoc_file ~doc_dir)
  |> Build.dyn_paths

let compile_module sctx (m : Module.t) ~includes:(file_deps, iflags)
      ~dep_graphs ~doc_dir ~pkg_or_lnu =
  let odoc_file = Module.odoc_file m ~doc_dir in
  add_rule sctx
    (file_deps
     >>>
     module_deps m ~doc_dir ~dep_graphs
     >>>
     Build.run ~dir:doc_dir (odoc sctx)
       [ A "compile"
       ; A "-I"; Path doc_dir
       ; iflags
       ; As ["--pkg"; pkg_or_lnu]
       ; A "-o"; Target odoc_file
       ; Dep (Module.cmti_file m)
       ]);
  (m, odoc_file)

let compile_mld sctx (m : Mld.t) ~includes ~doc_dir ~pkg =
  let odoc_file = Mld.odoc_file m ~doc_dir in
  add_rule sctx
    (includes
     >>>
     Build.run ~dir:doc_dir (odoc sctx)
       [ A "compile"
       ; Dyn Fn.id
       ; As ["--pkg"; Package.Name.to_string pkg]
       ; A "-o"; Target odoc_file
       ; Dep (Mld.odoc_input m)
       ]);
  odoc_file

let odoc_include_flags ctx requires =
  Arg_spec.of_result_map requires ~f:(fun libs ->
    let paths =
      libs |> List.fold_left ~f:(fun paths lib ->
        if Lib.is_local lib then (
          Path.Set.add paths (Paths.odocs ctx (Lib lib))
        ) else (
          paths
        )
      ) ~init:Path.Set.empty in
    Arg_spec.S (List.concat_map (Path.Set.to_list paths)
                  ~f:(fun dir -> [Arg_spec.A "-I"; Path dir])))

let setup_html sctx (odoc_file : odoc) ~requires =
  let ctx = Super_context.context sctx in
  let deps = Deps.deps ctx requires in
  let to_remove, dune_keep =
    match odoc_file.source with
    | Mld -> odoc_file.html_file, []
    | Module ->
      let dune_keep =
        Build.create_file (odoc_file.html_dir ++ Config.dune_keep_fname) in
      odoc_file.html_dir, [dune_keep]
  in
  add_rule sctx
    (deps
     >>>
     Build.progn (
       Build.remove_tree to_remove
       :: Build.mkdir odoc_file.html_dir
       :: Build.run ~dir:(Paths.html_root ctx)
            (odoc sctx)
            [ A "html"
            ; odoc_include_flags ctx requires
            ; A "-o"; Path (Paths.html_root ctx)
            ; Dep odoc_file.odoc_input
            ; Hidden_targets [odoc_file.html_file]
            ]
       :: dune_keep))

let setup_library_odoc_rules sctx (library : Library.t) ~scope ~modules
      ~requires ~(dep_graphs:Dep_graph.Ml_kind.t) =
  let lib =
    Option.value_exn (Lib.DB.find_even_when_hidden (Scope.libs scope)
                        (Library.best_name library)) in
  (* Using the proper package name doesn't actually work since odoc assumes
     that a package contains only 1 library *)
  let pkg_or_lnu = pkg_or_lnu lib in
  let ctx = Super_context.context sctx in
  let doc_dir = Paths.odocs ctx (Lib lib) in
  let odoc_include_flags = odoc_include_flags ctx requires in
  let includes = (Deps.deps ctx requires, odoc_include_flags) in
  let modules_and_odoc_files =
    List.map (Module.Name.Map.values modules) ~f:(
      compile_module sctx ~includes ~dep_graphs
        ~doc_dir ~pkg_or_lnu)
  in
  Deps.setup_deps ctx (Lib lib)
    (List.map modules_and_odoc_files ~f:snd
     |> Dep.Set.of_files)

let setup_css_rule sctx =
  let ctx = Super_context.context sctx in
  add_rule sctx
    (Build.run
       ~dir:ctx.build_dir
       (odoc sctx)
       [ A "support-files"; A "-o"; Path (Paths.html_root ctx)
       ; Hidden_targets [ Paths.css_file ctx
                        ; Paths.highlight_pack_js ctx
                        ]
       ])

let sp = Printf.sprintf

let setup_toplevel_index_rule sctx =
  let list_items =
    Super_context.packages sctx
    |> Package.Name.Map.to_list
    |> List.filter_map ~f:(fun (name, pkg) ->
      let name = Package.Name.to_string name in
      let link = sp {|<a href="%s/index.html">%s</a>|} name name in
      let version_suffix =
        match pkg.Package.version_from_opam_file with
        | None ->
          ""
        | Some v ->
          sp {| <span class="version">%s</span>|} v
      in
      Some (sp "<li>%s%s</li>" link version_suffix))
  in
  let list_items = String.concat ~sep:"\n      " list_items in
  let html = sp
               {|<!DOCTYPE html>
<html xmlns="http://www.w3.org/1999/xhtml">
  <head>
    <title>index</title>
    <link rel="stylesheet" href="./odoc.css"/>
    <meta charset="utf-8"/>
    <meta name="viewport" content="width=device-width,initial-scale=1.0"/>
  </head>
  <body>
    <main class="content">
      <div class="by-name">
      <h2>OCaml package documentation</h2>
      <ol>
      %s
      </ol>
      </div>
    </main>
  </body>
</html>|} list_items
  in
  let ctx = Super_context.context sctx in
  add_rule sctx (Build.write_file (Paths.toplevel_index ctx) html)

let libs_of_pkg sctx ~pkg =
  match Package.Name.Map.find (SC.libs_by_package sctx) pkg with
  | None -> Lib.Set.empty
  | Some (_, libs) -> libs

let load_all_odoc_rules_pkg sctx ~pkg =
  let pkg_libs = libs_of_pkg sctx ~pkg in
  let ctx = Super_context.context sctx in
  Build_system.load_dir ~dir:(Paths.odocs ctx (Pkg pkg));
  Lib.Set.iter pkg_libs ~f:(fun lib ->
    Build_system.load_dir ~dir:(Paths.odocs ctx (Lib lib)));
  pkg_libs

let create_odoc ctx ~target odoc_input =
  let html_base = Paths.html ctx target in
  match target with
  | Lib _ ->
    let html_dir =
      html_base ++ (
        Path.basename odoc_input
        |> Filename.chop_extension
        |> Stdune.String.capitalize
      ) in
    { odoc_input
    ; html_dir
    ; html_file = html_dir ++ "index.html"
    ; source = Module
    }
  | Pkg _ ->
    { odoc_input
    ; html_dir = html_base
    ; html_file = html_base ++ sprintf "%s.html" (
        Path.basename odoc_input
        |> Filename.chop_extension
        |> String.drop_prefix ~prefix:"page-"
        |> Option.value_exn
      )
    ; source = Mld
    }

let static_html ctx =
  let open Paths in
  [ css_file ctx
  ; highlight_pack_js ctx
  ; toplevel_index ctx
  ]

let odocs =
  let odoc_pred =
    Glob.of_string_exn (Loc.of_pos __POS__) "*.odoc"
    |> Glob.to_pred
  in
  fun ctx target ->
    let dir = Paths.odocs ctx target in
    File_selector.create ~dir odoc_pred
    |> Build_system.eval_pred
    |> Path.Set.fold ~init:[] ~f:(fun d acc ->
      create_odoc ctx d ~target :: acc)

let setup_lib_html_rules_def =
  let module Input = struct
    type t = Super_context.t * Lib.t * Lib.t list Or_exn.t

    let equal (sc1, l1, r1) (sc2, l2, r2) =
      Super_context.equal sc1 sc2
      && Lib.equal l1 l2
      && Or_exn.equal (List.equal Lib.equal) r1 r2

    let hash (sc, l, r) =
      Hashtbl.hash
        ( Super_context.hash sc
        , Lib.hash l
        , Or_exn.hash (List.hash Lib.hash) r
        )

    let to_sexp _ = Sexp.Encoder.string "<opaque>"
  end
  in
  let f (sctx, lib, requires) =
    let ctx = Super_context.context sctx in
    let odocs = odocs ctx (Lib lib) in
    List.iter odocs ~f:(setup_html sctx ~requires);
    let html_files = List.map ~f:(fun o -> o.html_file) odocs in
    let static_html = static_html ctx in
    Build_system.Alias.add_deps (Deps.html_alias ctx (Lib lib))
      (Dep.Set.of_files (List.rev_append static_html html_files))
  in
  Memo.With_implicit_output.create "setup-library-html-rules"
    ~doc:"setup html rules for library"
    ~implicit_output:Build_system.rule_collection_implicit_output
    ~input:(module Input)
    ~output:(module Unit)
    ~visibility:Hidden
    Sync
    f

let setup_lib_html_rules sctx lib ~requires =
  Memo.With_implicit_output.exec setup_lib_html_rules_def (sctx, lib, requires)

let setup_pkg_html_rules_def =
  let module Input = struct
    type t = Super_context.t * Package.Name.t * Lib.t list

    let equal (s1, p1, l1) (s2, p2, l2) =
      Package.Name.equal p1 p2
      && List.equal Lib.equal l1 l2
      && Super_context.equal s1 s2

    let hash (sctx, p, ls) =
      Hashtbl.hash
        ( Super_context.hash sctx
        , Package.Name.hash p
        , List.hash Lib.hash ls
        )

    let to_sexp (_, package, libs) =
      let open Dyn in
      Tuple
        [ Package.Name.to_dyn package
        ; List (List.map ~f:Lib.to_dyn libs)
        ]
      |> Dyn.to_sexp
  end
  in
  Memo.With_implicit_output.create "setup-package-html-rules"
    ~output:(module Unit)
    ~implicit_output:Build_system.rule_collection_implicit_output
    ~doc:"setup odoc package html rules"
    ~input:(module Input)
    ~visibility:Hidden
    Sync
    (fun (sctx, pkg, libs) ->
       let requires = Lib.closure libs ~linking:false in
       let ctx = Super_context.context sctx in
       List.iter libs ~f:(setup_lib_html_rules sctx ~requires);
       let pkg_odocs = odocs ctx (Pkg pkg) in
       List.iter pkg_odocs ~f:(setup_html sctx ~requires);
       let odocs =
         List.concat (
           pkg_odocs
           :: (List.map libs ~f:(fun lib -> odocs ctx (Lib lib)))
         ) in
       let html_files = List.map ~f:(fun o -> o.html_file) odocs in
       let static_html = static_html ctx in
       Build_system.Alias.add_deps (Deps.html_alias ctx (Pkg pkg))
         (Dep.Set.of_files (List.rev_append static_html html_files)))

let setup_pkg_html_rules sctx ~pkg ~libs =
  Memo.With_implicit_output.exec setup_pkg_html_rules_def (sctx, pkg, libs)

let setup_package_aliases sctx (pkg : Package.t) =
  let ctx = Super_context.context sctx in
  let alias =
    let dir = Path.append ctx.build_dir pkg.Package.path in
    Alias.doc ~dir
  in
  Build_system.Alias.add_deps alias (
    Deps.html_alias ctx (Pkg pkg.name)
    :: (libs_of_pkg sctx ~pkg:pkg.name
        |> Lib.Set.to_list
        |> List.map ~f:(fun lib -> Deps.html_alias ctx (Lib lib)))
    |> List.map ~f:Alias.stamp_file
    |> Dep.Set.of_files
  )

let entry_modules_by_lib sctx lib =
  Dir_contents.get_without_rules sctx ~dir:(Lib.src_dir lib)
  |> Dir_contents.modules_of_library ~name:(Lib.name lib)
  |> Lib_modules.entry_modules

let entry_modules sctx ~pkg =
  libs_of_pkg sctx ~pkg
  |> Lib.Set.to_list
  |> List.filter_map ~f:(fun l ->
    if Lib.is_local l then (
      Some (l, entry_modules_by_lib sctx l)
    ) else (
      None
    ))
  |> Lib.Map.of_list_exn

let default_index ~pkg entry_modules =
  let b = Buffer.create 512 in
  Printf.bprintf b "{0 %s index}\n"
    (Package.Name.to_string pkg);
  Lib.Map.to_list entry_modules
  |> List.sort ~compare:(fun (x, _) (y, _) ->
    Lib_name.compare (Lib.name x) (Lib.name y))
  |> List.iter ~f:(fun (lib, modules) ->
    Printf.bprintf b "{1 Library %s}\n" (Lib_name.to_string (Lib.name lib));
    Buffer.add_string b (
      match modules with
      | [ x ] ->
        sprintf
          "The entry point of this library is the module:\n{!module-%s}.\n"
          (Module.Name.to_string (Module.name x))
      | _ ->
        sprintf
          "This library exposes the following toplevel modules:\n\
           {!modules:%s}\n"
          (modules
           |> List.sort ~compare:(fun x y ->
             Module.Name.compare (Module.name x) (Module.name y))
           |> List.map ~f:(fun m -> Module.Name.to_string (Module.name m))
           |> String.concat ~sep:" ")
    );
  );
  Buffer.contents b

let check_mlds_no_dupes ~pkg ~mlds =
  match
    List.map mlds ~f:(fun mld ->
      (Filename.chop_extension (Path.basename mld), mld))
    |> String.Map.of_list
  with
  | Ok m -> m
  | Error (_, p1, p2) ->
    die "Package %s has two mld's with the same basename %s, %s"
      (Package.Name.to_string pkg)
      (Path.to_string_maybe_quoted p1)
      (Path.to_string_maybe_quoted p2)

let setup_package_odoc_rules_def =
  let module Input = struct
    type t = Super_context.t * Package.Name.t * Path.t list

    let hash (sctx, p, ps) =
      Hashtbl.hash
        ( Super_context.hash sctx
        , Package.Name.hash p
        , List.hash Path.hash ps
        )

    let equal (s1, x1, y1) (s2, x2, y2) =
      Super_context.equal s1 s2
      && Package.Name.equal x1 x2
      && List.equal Path.equal y1 y2

    let to_sexp (_, name, paths) =
      Dyn.Tuple
        [ Package.Name.to_dyn name
        ; Dyn.List (List.map ~f:Path.to_dyn paths)
        ]
      |> Dyn.to_sexp
  end
  in
  Memo.With_implicit_output.create "setup-package-odoc-rules"
    ~output:(module Unit)
    ~implicit_output:Build_system.rule_collection_implicit_output
    ~doc:"setup odoc package rules"
    ~input:(module Input)
    ~visibility:Hidden
    Sync
    (fun (sctx, pkg, mlds) ->
       let mlds = check_mlds_no_dupes ~pkg ~mlds in
       let ctx = Super_context.context sctx in
       let mlds =
         if String.Map.mem mlds "index" then
           mlds
         else
           let entry_modules = entry_modules ~pkg in
           let gen_mld = Paths.gen_mld_dir ctx pkg ++ "index.mld" in
           let entry_modules = entry_modules sctx in
           add_rule sctx
             (Build.write_file gen_mld (default_index ~pkg entry_modules));
           String.Map.add mlds "index" gen_mld in
       let odocs = List.map (String.Map.values mlds) ~f:(fun mld ->
         compile_mld sctx
           (Mld.create mld)
           ~pkg
           ~doc_dir:(Paths.odocs ctx (Pkg pkg))
           ~includes:(Build.arr (fun _ -> Arg_spec.As []))
       ) in
       Deps.setup_deps ctx (Pkg pkg) (Dep.Set.of_files odocs))

let setup_package_odoc_rules sctx ~pkg ~mlds =
  Memo.With_implicit_output.exec setup_package_odoc_rules_def (sctx, pkg, mlds)

let init sctx =
  let stanzas = SC.stanzas sctx in
  let ctx = Super_context.context sctx in
  SC.packages sctx
  |> Package.Name.Map.iter ~f:(fun (pkg : Package.t) ->
    (* setup @doc to build the correct html for the package *)
    setup_package_aliases sctx pkg;
  );
  Build_system.Alias.add_deps
    (Alias.private_doc ~dir:ctx.build_dir)
    (stanzas
     |> List.concat_map ~f:(fun (w : _ Dir_with_dune.t) ->
       List.filter_map w.data ~f:(function
         | Dune_file.Library (l : Dune_file.Library.t) ->
           begin match l.public with
           | Some _ -> None
           | None ->
             let scope = SC.find_scope_by_dir sctx w.ctx_dir in
             Some (Option.value_exn (
               Lib.DB.find_even_when_hidden (Scope.libs scope)
                 (Library.best_name l))
             )
           end
         | _ -> None
       ))
     |> List.map ~f:(fun (lib : Lib.t) -> Dep.alias (Deps.html_alias ctx (Lib lib)))
     |> Dep.Set.of_list
    )

let gen_rules sctx ~dir:_ rest =
  match rest with
  | ["_html"] ->
    setup_css_rule sctx;
    setup_toplevel_index_rule sctx
  | "_mlds" :: pkg :: _
  | "_odoc" :: "pkg" :: pkg :: _ ->
    let pkg = Package.Name.of_string pkg in
    let packages = Super_context.packages sctx in
    Package.Name.Map.find packages pkg
    |> Option.iter ~f:(fun _ ->
      let mlds = Packages.mlds sctx pkg in
      setup_package_odoc_rules sctx ~pkg ~mlds)
  | "_odoc" :: "lib" :: lib :: _ ->
    let lib, lib_db = SC.Scope_key.of_string sctx lib in
    let lib = Lib_name.of_string_exn ~loc:None lib in
    Lib.DB.find lib_db lib
    |> Result.iter ~f:(fun lib ->
      (* TODO instead of this hack, call memoized function that generates the
         rules for this library *)
      Build_system.load_dir ~dir:(Lib.src_dir lib))
  | "_html" :: lib_unique_name_or_pkg :: _ ->
    (* TODO we can be a better with the error handling in the case where
       lib_unique_name_or_pkg is neither a valid pkg or lnu *)
    let lib, lib_db = SC.Scope_key.of_string sctx lib_unique_name_or_pkg in
    let lib = Lib_name.of_string_exn ~loc:None lib in
    let setup_pkg_html_rules pkg =
      setup_pkg_html_rules sctx ~pkg ~libs:(
        Lib.Set.to_list (load_all_odoc_rules_pkg sctx ~pkg)) in
    Lib.DB.find lib_db lib
    |> Result.iter ~f:(fun lib ->
      match Lib.package lib with
      | None ->
        setup_lib_html_rules sctx
          lib ~requires:(Lib.closure ~linking:false [lib])
      | Some pkg ->
        setup_pkg_html_rules pkg);
    Option.iter
      (Package.Name.Map.find (SC.packages sctx)
         (Package.Name.of_string lib_unique_name_or_pkg))
      ~f:(fun pkg -> setup_pkg_html_rules pkg.name)
  | _ -> ()
