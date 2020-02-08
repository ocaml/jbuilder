open! Stdune
open Import
open Dune_file
open Build.O
module SC = Super_context

let ( ++ ) = Path.Build.relative

module Scope_key : sig
  val of_string : Super_context.t -> string -> Lib_name.t * Lib.DB.t

  val to_string : Lib_name.t -> Dune_project.t -> string
end = struct
  let of_string sctx s =
    match String.rsplit2 s ~on:'@' with
    | None ->
      (Lib_name.of_string_exn s ~loc:None, Super_context.public_libs sctx)
    | Some (lib, key) ->
      let scope =
        Dune_project.File_key.of_string key
        |> Super_context.find_project_by_key sctx
        |> Super_context.find_scope_by_project sctx
      in
      (Lib_name.of_string_exn lib ~loc:None, Scope.libs scope)

  let to_string lib project =
    let key = Dune_project.file_key project in
    sprintf "%s@%s" (Lib_name.to_string lib)
      (Dune_project.File_key.to_string key)
end

let lib_unique_name lib =
  let name = Lib.name lib in
  let info = Lib.info lib in
  let status = Lib_info.status info in
  match status with
  | Installed -> assert false
  | Public _ -> Lib_name.to_string name
  | Private project -> Scope_key.to_string name project

let pkg_or_lnu lib =
  match Lib.package lib with
  | Some p -> Package.Name.to_string p
  | None -> lib_unique_name lib

type target =
  | Lib of Lib.Local.t
  | Pkg of Package.Name.t

type source =
  | Module
  | Mld

type odoc =
  { odoc_input : Path.Build.t
  ; html_dir : Path.Build.t
  ; html_file : Path.Build.t
  ; source : source
  }

let add_rule sctx =
  Super_context.add_rule sctx ~dir:(Super_context.build_dir sctx)

module Paths = struct
  let root (context : Context.t) =
    Path.Build.relative context.Context.build_dir "_doc"

  let odocs ctx = function
    | Lib lib ->
      let obj_dir = Lib.Local.obj_dir lib in
      Obj_dir.odoc_dir obj_dir
    | Pkg pkg -> root ctx ++ sprintf "_odoc/pkg/%s" (Package.Name.to_string pkg)

  let html_root ctx = root ctx ++ "_html"

  let html ctx m =
    html_root ctx
    ++
    match m with
    | Pkg pkg -> Package.Name.to_string pkg
    | Lib lib -> pkg_or_lnu (Lib.Local.to_lib lib)

  let gen_mld_dir ctx pkg = root ctx ++ "_mlds" ++ Package.Name.to_string pkg

  let css_file ctx = html_root ctx ++ "odoc.css"

  let highlight_pack_js ctx = html_root ctx ++ "highlight.pack.js"

  let toplevel_index ctx = html_root ctx ++ "index.html"
end

module Dep = struct
  let html_alias ctx m = Alias.doc ~dir:(Paths.html ctx m)

  let alias = Alias.make (Alias.Name.of_string ".odoc-all")

  let deps ctx pkg requires =
    Build.of_result_map requires ~f:(fun libs ->
        Build.deps
          (let init =
             match pkg with
             | Some p ->
               Dep.Set.singleton
                 (Dep.alias (alias ~dir:(Paths.odocs ctx (Pkg p))))
             | None -> Dep.Set.empty
           in
           List.fold_left libs ~init ~f:(fun acc (lib : Lib.t) ->
               match Lib.Local.of_lib lib with
               | None -> acc
               | Some lib ->
                 let dir = Paths.odocs ctx (Lib lib) in
                 let alias = alias ~dir in
                 Dep.Set.add acc (Dep.alias alias))))

  let alias ctx m = alias ~dir:(Paths.odocs ctx m)

  (* let static_deps t lib = Build_system.Alias.dep (alias t lib) *)

  let setup_deps ctx m files = Rules.Produce.Alias.add_deps (alias ctx m) files
end

let odoc_ext = ".odoc"

module Mld : sig
  type t

  val create : Path.Build.t -> t

  val odoc_file : doc_dir:Path.Build.t -> t -> Path.Build.t

  val odoc_input : t -> Path.Build.t
end = struct
  type t = Path.Build.t

  let create p = p

  let odoc_file ~doc_dir t =
    let t = Filename.chop_extension (Path.Build.basename t) in
    Path.Build.relative doc_dir (sprintf "page-%s%s" t odoc_ext)

  let odoc_input t = t
end

let odoc sctx =
  SC.resolve_program sctx
    ~dir:(Super_context.build_dir sctx)
    "odoc" ~loc:None ~hint:"try: opam install odoc"

let module_deps (m : Module.t) ~obj_dir ~(dep_graphs : Dep_graph.Ml_kind.t) =
  Build.dyn_paths_unit
    (let+ deps =
       if Module.has m ~ml_kind:Intf then
         Dep_graph.deps_of dep_graphs.intf m
       else
         (* When a module has no .mli, use the dependencies for the .ml *)
         Dep_graph.deps_of dep_graphs.impl m
     in
     List.map deps ~f:(fun m -> Path.build (Obj_dir.Module.odoc obj_dir m)))

let compile_module sctx ~obj_dir (m : Module.t) ~includes:(file_deps, iflags)
    ~dep_graphs ~pkg_or_lnu =
  let odoc_file = Obj_dir.Module.odoc obj_dir m in
  let open Build.With_targets.O in
  add_rule sctx
    ( Build.with_no_targets file_deps
    >>> Build.with_no_targets (module_deps m ~obj_dir ~dep_graphs)
    >>>
    let doc_dir = Path.build (Obj_dir.odoc_dir obj_dir) in
    Command.run ~dir:doc_dir (odoc sctx)
      [ A "compile"
      ; A "-I"
      ; Path doc_dir
      ; iflags
      ; As [ "--pkg"; pkg_or_lnu ]
      ; A "-o"
      ; Target odoc_file
      ; Dep (Path.build (Obj_dir.Module.cmti_file obj_dir m))
      ] );
  (m, odoc_file)

let compile_mld sctx (m : Mld.t) ~includes ~doc_dir ~pkg =
  let odoc_file = Mld.odoc_file m ~doc_dir in
  add_rule sctx
    (Command.run ~dir:(Path.build doc_dir) (odoc sctx)
       [ A "compile"
       ; Command.Args.dyn includes
       ; As [ "--pkg"; Package.Name.to_string pkg ]
       ; A "-o"
       ; Target odoc_file
       ; Dep (Path.build (Mld.odoc_input m))
       ]);
  odoc_file

let odoc_include_flags ctx pkg requires =
  Command.of_result_map requires ~f:(fun libs ->
      let paths =
        libs
        |> List.fold_left
             ~f:(fun paths lib ->
               match Lib.Local.of_lib lib with
               | None -> paths
               | Some lib ->
                 Path.Set.add paths (Path.build (Paths.odocs ctx (Lib lib))))
             ~init:Path.Set.empty
      in
      let paths =
        match pkg with
        | Some p -> Path.Set.add paths (Path.build (Paths.odocs ctx (Pkg p)))
        | None -> paths
      in
      S
        (List.concat_map (Path.Set.to_list paths) ~f:(fun dir ->
             [ Command.Args.A "-I"; Path dir ])))

let setup_html sctx (odoc_file : odoc) ~pkg ~requires =
  let ctx = Super_context.context sctx in
  let deps = Dep.deps ctx pkg requires in
  let to_remove, dune_keep =
    match odoc_file.source with
    | Mld -> (odoc_file.html_file, [])
    | Module ->
      let dune_keep =
        Build.create_file (odoc_file.html_dir ++ Config.dune_keep_fname)
      in
      (odoc_file.html_dir, [ dune_keep ])
  in
  let open Build.With_targets.O in
  add_rule sctx
    ( Build.with_no_targets deps
    >>> Build.progn
          ( Build.with_no_targets
              (Build.return
                 (* Note that we declare no targets apart from [dune_keep]. This
                    means Dune doesn't know how to build specific documentation
                    files and that we can't run this rule in a sandbox. To
                    properly declare targets we would need to support some form
                    of "dynamic targets" or "target directories". *)
                 (Action.Progn
                    [ Action.Remove_tree to_remove
                    ; Action.Mkdir (Path.build odoc_file.html_dir)
                    ]))
          :: Command.run
               ~dir:(Path.build (Paths.html_root ctx))
               (odoc sctx)
               [ A "html"
               ; odoc_include_flags ctx pkg requires
               ; A "-o"
               ; Path (Path.build (Paths.html_root ctx))
               ; Dep (Path.build odoc_file.odoc_input)
               ; Hidden_targets [ odoc_file.html_file ]
               ]
          :: dune_keep ) )

let setup_library_odoc_rules cctx (library : Library.t) ~dep_graphs =
  let lib =
    let scope = Compilation_context.scope cctx in
    Library.best_name library
    |> Lib.DB.find_even_when_hidden (Scope.libs scope)
    |> Option.value_exn
  in
  let local_lib = Lib.Local.of_lib_exn lib in
  (* Using the proper package name doesn't actually work since odoc assumes that
     a package contains only 1 library *)
  let pkg_or_lnu = pkg_or_lnu lib in
  let sctx = Compilation_context.super_context cctx in
  let ctx = Super_context.context sctx in
  let requires = Compilation_context.requires_compile cctx in
  let odoc_include_flags =
    Command.Args.memo (odoc_include_flags ctx (Lib.package lib) requires)
  in
  let obj_dir = Compilation_context.obj_dir cctx in
  let modules = Compilation_context.modules cctx in
  let includes =
    (Dep.deps ctx (Lib.package lib) requires, odoc_include_flags)
  in
  let modules_and_odoc_files =
    Modules.fold_no_vlib modules ~init:[] ~f:(fun m acc ->
        let compiled =
          compile_module sctx ~includes ~dep_graphs ~obj_dir ~pkg_or_lnu m
        in
        compiled :: acc)
  in
  Dep.setup_deps ctx (Lib local_lib)
    ( List.map modules_and_odoc_files ~f:(fun (_, p) -> Path.build p)
    |> Path.Set.of_list )

let setup_css_rule sctx =
  let ctx = Super_context.context sctx in
  add_rule sctx
    (Command.run ~dir:(Path.build ctx.build_dir) (odoc sctx)
       [ A "support-files"
       ; A "-o"
       ; Path (Path.build (Paths.html_root ctx))
       ; Hidden_targets [ Paths.css_file ctx; Paths.highlight_pack_js ctx ]
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
             match pkg.Package.version with
             | None -> ""
             | Some v -> sp {| <span class="version">%s</span>|} v
           in
           Some (sp "<li>%s%s</li>" link version_suffix))
  in
  let list_items = String.concat ~sep:"\n      " list_items in
  let html =
    sp
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
</html>|}
      list_items
  in
  let ctx = Super_context.context sctx in
  add_rule sctx (Build.write_file (Paths.toplevel_index ctx) html)

let libs_of_pkg sctx ~pkg =
  SC.lib_entries_of_package sctx pkg
  |> (* Filter out all implementations of virtual libraries *)
  List.filter_map ~f:(function
    | Super_context.Lib_entry.Library lib ->
      let is_impl = Lib.Local.to_lib lib |> Lib.is_impl in
      Option.some_if (not is_impl) lib
    | Deprecated_library_name _ -> None)

let load_all_odoc_rules_pkg sctx ~pkg =
  let pkg_libs = libs_of_pkg sctx ~pkg in
  let ctx = Super_context.context sctx in
  let open Fiber.O in
  let* () =
    Build_system.load_dir ~dir:(Path.build (Paths.odocs ctx (Pkg pkg)))
  in
  let+ () =
    Fiber.parallel_iter pkg_libs ~f:(fun lib ->
        Build_system.load_dir ~dir:(Path.build (Paths.odocs ctx (Lib lib))))
  in
  pkg_libs

let create_odoc ctx ~target odoc_input =
  let html_base = Paths.html ctx target in
  match target with
  | Lib _ ->
    let html_dir =
      html_base
      ++ ( Path.Build.basename odoc_input
         |> Filename.chop_extension |> Stdune.String.capitalize )
    in
    { odoc_input
    ; html_dir
    ; html_file = html_dir ++ "index.html"
    ; source = Module
    }
  | Pkg _ ->
    { odoc_input
    ; html_dir = html_base
    ; html_file =
        html_base
        ++ sprintf "%s.html"
             ( Path.Build.basename odoc_input
             |> Filename.chop_extension
             |> String.drop_prefix ~prefix:"page-"
             |> Option.value_exn )
    ; source = Mld
    }

let static_html ctx =
  let open Paths in
  [ css_file ctx; highlight_pack_js ctx; toplevel_index ctx ]

let check_mlds_no_dupes ~pkg ~mlds =
  match
    List.map mlds ~f:(fun mld ->
        (Filename.chop_extension (Path.Build.basename mld), mld))
    |> String.Map.of_list
  with
  | Ok m -> m
  | Error (_, p1, p2) ->
    User_error.raise
      [ Pp.textf "Package %s has two mld's with the same basename %s, %s"
          (Package.Name.to_string pkg)
          (Path.to_string_maybe_quoted (Path.build p1))
          (Path.to_string_maybe_quoted (Path.build p2))
      ]

let odocs sctx target =
  let ctx = Super_context.context sctx in
  let dir = Paths.odocs ctx target in
  match target with
  | Pkg pkg ->
    let mlds =
      let mlds = Packages.mlds sctx pkg in
      let mlds = check_mlds_no_dupes ~pkg ~mlds in
      if String.Map.mem mlds "index" then
        mlds
      else
        let gen_mld = Paths.gen_mld_dir ctx pkg ++ "index.mld" in
        String.Map.add_exn mlds "index" gen_mld
    in
    String.Map.values mlds
    |> List.map ~f:(fun mld ->
           Mld.create mld |> Mld.odoc_file ~doc_dir:dir
           |> create_odoc ctx ~target)
  | Lib lib ->
    let info = Lib.Local.info lib in
    let dir = Lib_info.src_dir info in
    let modules =
      let name = Lib_info.name info in
      Dir_contents.get sctx ~dir |> Dir_contents.modules_of_library ~name
    in
    let obj_dir = Lib_info.obj_dir info in
    Modules.fold_no_vlib modules ~init:[] ~f:(fun m acc ->
        let odoc = Obj_dir.Module.odoc obj_dir m in
        create_odoc ctx ~target odoc :: acc)

let setup_lib_html_rules_def =
  let module Input = struct
    module Super_context = Super_context.As_memo_key

    type t = Super_context.t * Lib.Local.t * Lib.t list Or_exn.t

    let equal (sc1, l1, r1) (sc2, l2, r2) =
      Super_context.equal sc1 sc2
      && Lib.Local.equal l1 l2
      && Or_exn.equal (List.equal Lib.equal) r1 r2

    let hash (sc, l, r) =
      Hashtbl.hash
        ( Super_context.hash sc
        , Lib.Local.hash l
        , Or_exn.hash (List.hash Lib.hash) r )

    let to_dyn _ = Dyn.Opaque
  end in
  let f (sctx, lib, requires) =
    let ctx = Super_context.context sctx in
    let odocs = odocs sctx (Lib lib) in
    let pkg = Lib.package (Lib.Local.to_lib lib) in
    List.iter odocs ~f:(setup_html sctx ~pkg ~requires);
    let html_files = List.map ~f:(fun o -> Path.build o.html_file) odocs in
    let static_html = List.map ~f:Path.build (static_html ctx) in
    Rules.Produce.Alias.add_deps
      (Dep.html_alias ctx (Lib lib))
      (Path.Set.of_list (List.rev_append static_html html_files))
  in
  Memo.With_implicit_output.create "setup-library-html-rules"
    ~doc:"setup html rules for library" ~implicit_output:Rules.implicit_output
    ~input:(module Input)
    ~output:(module Unit)
    ~visibility:Hidden Sync f

let setup_lib_html_rules sctx lib ~requires =
  Memo.With_implicit_output.exec setup_lib_html_rules_def (sctx, lib, requires)

let setup_pkg_html_rules_def =
  let module Input = struct
    module Super_context = Super_context.As_memo_key

    type t = Super_context.t * Package.Name.t * Lib.Local.t list

    let equal (s1, p1, l1) (s2, p2, l2) =
      Package.Name.equal p1 p2
      && List.equal Lib.Local.equal l1 l2
      && Super_context.equal s1 s2

    let hash (sctx, p, ls) =
      Hashtbl.hash
        ( Super_context.hash sctx
        , Package.Name.hash p
        , List.hash Lib.Local.hash ls )

    let to_dyn (_, package, libs) =
      let open Dyn in
      Tuple
        [ Package.Name.to_dyn package
        ; List (List.map ~f:Lib.Local.to_dyn libs)
        ]
  end in
  Memo.With_implicit_output.create "setup-package-html-rules"
    ~output:(module Unit)
    ~implicit_output:Rules.implicit_output ~doc:"setup odoc package html rules"
    ~input:(module Input)
    ~visibility:Hidden Sync
    (fun (sctx, pkg, (libs : Lib.Local.t list)) ->
      let requires =
        let libs = (libs :> Lib.t list) in
        Lib.closure libs ~linking:false
      in
      let ctx = Super_context.context sctx in
      List.iter libs ~f:(setup_lib_html_rules sctx ~requires);
      let pkg_odocs = odocs sctx (Pkg pkg) in
      List.iter pkg_odocs ~f:(setup_html sctx ~pkg:(Some pkg) ~requires);
      let odocs =
        List.concat
          (pkg_odocs :: List.map libs ~f:(fun lib -> odocs sctx (Lib lib)))
      in
      let html_files = List.map ~f:(fun o -> Path.build o.html_file) odocs in
      let static_html = List.map ~f:Path.build (static_html ctx) in
      Rules.Produce.Alias.add_deps
        (Dep.html_alias ctx (Pkg pkg))
        (Path.Set.of_list (List.rev_append static_html html_files)))

let setup_pkg_html_rules sctx ~pkg ~libs =
  Memo.With_implicit_output.exec setup_pkg_html_rules_def (sctx, pkg, libs)

let setup_package_aliases sctx (pkg : Package.t) =
  let ctx = Super_context.context sctx in
  let alias =
    let dir = Path.Build.append_source ctx.build_dir pkg.Package.path in
    Alias.doc ~dir
  in
  Rules.Produce.Alias.add_deps alias
    ( Dep.html_alias ctx (Pkg pkg.name)
      :: ( libs_of_pkg sctx ~pkg:pkg.name
         |> List.map ~f:(fun lib -> Dep.html_alias ctx (Lib lib)) )
    |> List.map ~f:(fun f -> Path.build (Alias.stamp_file f))
    |> Path.Set.of_list )

let entry_modules_by_lib sctx lib =
  let info = Lib.Local.info lib in
  let dir = Lib_info.src_dir info in
  let name = Lib.name (Lib.Local.to_lib lib) in
  Dir_contents.get sctx ~dir
  |> Dir_contents.modules_of_library ~name
  |> Modules.entry_modules

let entry_modules sctx ~pkg =
  libs_of_pkg sctx ~pkg
  |> Lib.Local.Map.of_list_map_exn ~f:(fun l ->
         (l, entry_modules_by_lib sctx l))

let default_index ~pkg entry_modules =
  let b = Buffer.create 512 in
  Printf.bprintf b "{0 %s index}\n" (Package.Name.to_string pkg);
  Lib.Local.Map.to_list entry_modules
  |> List.sort ~compare:(fun (x, _) (y, _) ->
         let name lib = Lib.name (Lib.Local.to_lib lib) in
         Lib_name.compare (name x) (name y))
  |> List.iter ~f:(fun (lib, modules) ->
         let lib = Lib.Local.to_lib lib in
         Printf.bprintf b "{1 Library %s}\n" (Lib_name.to_string (Lib.name lib));
         Buffer.add_string b
           ( match modules with
           | [ x ] ->
             sprintf
               "The entry point of this library is the module:\n{!module-%s}.\n"
               (Module_name.to_string (Module.name x))
           | _ ->
             sprintf
               "This library exposes the following toplevel modules:\n\
                {!modules:%s}\n"
               ( modules
               |> List.filter ~f:(fun m ->
                      Module.visibility m = Visibility.Public)
               |> List.sort ~compare:(fun x y ->
                      Module_name.compare (Module.name x) (Module.name y))
               |> List.map ~f:(fun m -> Module_name.to_string (Module.name m))
               |> String.concat ~sep:" " ) ));
  Buffer.contents b

let setup_package_odoc_rules_def =
  let module Input = struct
    module Super_context = Super_context.As_memo_key

    type t = Super_context.t * Package.Name.t

    let hash (sctx, p) =
      Hashtbl.hash (Super_context.hash sctx, Package.Name.hash p)

    let equal (s1, x1) (s2, x2) =
      Super_context.equal s1 s2 && Package.Name.equal x1 x2

    let to_dyn (_, name) = Dyn.Tuple [ Package.Name.to_dyn name ]
  end in
  Memo.With_implicit_output.create "setup-package-odoc-rules"
    ~output:(module Unit)
    ~implicit_output:Rules.implicit_output ~doc:"setup odoc package rules"
    ~input:(module Input)
    ~visibility:Hidden Sync
    (fun (sctx, pkg) ->
      let mlds = Packages.mlds sctx pkg in
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
          String.Map.set mlds "index" gen_mld
      in
      let odocs =
        List.map (String.Map.values mlds) ~f:(fun mld ->
            compile_mld sctx (Mld.create mld) ~pkg
              ~doc_dir:(Paths.odocs ctx (Pkg pkg))
              ~includes:(Build.return []))
      in
      Dep.setup_deps ctx (Pkg pkg) (Path.set_of_build_paths_list odocs))

let setup_package_odoc_rules sctx ~pkg =
  Memo.With_implicit_output.exec setup_package_odoc_rules_def (sctx, pkg)

let init sctx =
  let stanzas = SC.stanzas sctx in
  let ctx = Super_context.context sctx in
  SC.packages sctx
  |> Package.Name.Map.iter ~f:(fun (pkg : Package.t) ->
         (* setup @doc to build the correct html for the package *)
         setup_package_aliases sctx pkg);
  Rules.Produce.Alias.add_deps
    (Alias.private_doc ~dir:ctx.build_dir)
    ( stanzas
    |> List.concat_map ~f:(fun (w : _ Dir_with_dune.t) ->
           List.filter_map w.data ~f:(function
             | Dune_file.Library (l : Dune_file.Library.t) -> (
               match l.public with
               | Some _ -> None
               | None ->
                 let scope = SC.find_scope_by_dir sctx w.ctx_dir in
                 Library.best_name l
                 |> Lib.DB.find_even_when_hidden (Scope.libs scope)
                 |> Option.value_exn |> Lib.Local.of_lib_exn |> Option.some )
             | _ -> None))
    |> List.map ~f:(fun (lib : Lib.Local.t) ->
           Lib lib |> Dep.html_alias ctx |> Alias.stamp_file |> Path.build)
    |> Path.Set.of_list )

let gen_rules sctx ~dir:_ rest : unit Fiber.t =
  match rest with
  | [ "_html" ] ->
    setup_css_rule sctx;
    Fiber.return (setup_toplevel_index_rule sctx)
  | "_mlds" :: pkg :: _
  | "_odoc" :: "pkg" :: pkg :: _ ->
    let pkg = Package.Name.of_string pkg in
    let packages = Super_context.packages sctx in
    Package.Name.Map.find packages pkg
    |> Option.iter ~f:(fun _ -> setup_package_odoc_rules sctx ~pkg)
    |> Fiber.return
  | "_odoc" :: "lib" :: lib :: _ -> (
    let lib, lib_db = Scope_key.of_string sctx lib in
    (* diml: why isn't [None] some kind of error here? *)
    match Lib.DB.find lib_db lib with
    | None -> Fiber.return ()
    | Some lib ->
      (* TODO instead of this hack, call memoized function that generates the
         rules for this library *)
      let info = Lib.info lib in
      let dir = Lib_info.src_dir info in
      Build_system.load_dir ~dir )
  | "_html" :: lib_unique_name_or_pkg :: _ -> (
    (* TODO we can be a better with the error handling in the case where
       lib_unique_name_or_pkg is neither a valid pkg or lnu *)
    let lib, lib_db = Scope_key.of_string sctx lib_unique_name_or_pkg in
    let setup_pkg_html_rules pkg =
      let open Fiber.O in
      let+ libs = load_all_odoc_rules_pkg sctx ~pkg in
      setup_pkg_html_rules sctx ~pkg ~libs
    in
    (* diml: why isn't [None] some kind of error here? *)
    let lib =
      let open Option.O in
      let* lib = Lib.DB.find lib_db lib in
      Lib.Local.of_lib lib
    in
    let open Fiber.O in
    let* () =
      match lib with
      | None -> Fiber.return ()
      | Some lib -> (
        match Lib.package (Lib.Local.to_lib lib) with
        | Some pkg -> setup_pkg_html_rules pkg
        | None ->
          Fiber.return
            (setup_lib_html_rules sctx lib
               ~requires:(Lib.closure ~linking:false [ Lib.Local.to_lib lib ]))
        )
    in
    match
      Package.Name.Map.find (SC.packages sctx)
        (Package.Name.of_string lib_unique_name_or_pkg)
    with
    | None -> Fiber.return ()
    | Some pkg -> setup_pkg_html_rules pkg.name )
  | _ -> Fiber.return ()
