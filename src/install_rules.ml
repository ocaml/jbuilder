open! Stdune
open Import
open Build.O
open! No_io

module Library = Dune_file.Library

let gen_dune_package sctx ~version ~(pkg : Local_package.t) =
  let ctx = Super_context.context sctx in
  let dune_package_file = Local_package.dune_package_file pkg in
  let meta_template = Local_package.meta_template pkg in
  let name = Local_package.name pkg in
  let dune_version = Syntax.greatest_supported_version Stanza.syntax in
  Build.if_file_exists meta_template
    ~then_:(Build.return Dune_package.Or_meta.Use_meta)
    ~else_:(
      version >>^ (fun version ->
        let dune_package =
          let pkg_root =
            Config.local_install_lib_dir ~context:ctx.name ~package:name
          in
          let lib_root lib =
            let (_, subdir) = Lib_name.split (Lib.name lib) in
            Path.L.relative pkg_root subdir
          in
          let libs =
            Local_package.libs pkg
            |> Lib.Set.to_list
            |> List.map ~f:(fun lib ->
              let name = Lib.name lib in
              let dir_contents =
                Dir_contents.get_without_rules sctx ~dir:(Lib.src_dir lib) in
              let lib_modules =
                Dir_contents.modules_of_library dir_contents ~name in
              let foreign_objects =
                let dir = Obj_dir.obj_dir (Lib.obj_dir lib) in
                Dir_contents.c_sources_of_library dir_contents ~name
                |> C.Sources.objects ~dir ~ext_obj:ctx.ext_obj
              in
              Lib.to_dune_lib lib ~dir:(lib_root lib) ~lib_modules
                ~foreign_objects)
          in
          Dune_package.Or_meta.Dune_package
            { Dune_package.
              version
            ; name
            ; libs
            ; dir = pkg_root
            }
        in
        dune_package))
  >>^ (fun pkg ->
    Dune_package.Or_meta.encode ~dune_version pkg
    |> Format.asprintf "%a@."
         (Fmt.list ~pp_sep:Fmt.nl
            (Dune_lang.pp (Stanza.File_kind.of_syntax dune_version))))
  >>>
  Build.write_file_dyn dune_package_file
  |> Super_context.add_rule sctx ~dir:ctx.build_dir

let version_from_dune_project sctx ~(pkg : Package.t) =
  let dir = Path.append (Super_context.build_dir sctx) pkg.path in
  let project = Scope.project (Super_context.find_scope_by_dir sctx dir) in
  Dune_project.version project

type version_method =
  | File of string
  | From_dune_project

let pkg_version sctx ~path ~(pkg : Package.t) =
  match pkg.version_from_opam_file with
  | Some s -> Build.return (Some s)
  | None ->
    let rec loop = function
      | [] -> Build.return None
      | candidate :: rest ->
        match candidate with
        | File fn ->
          let p = Path.relative path fn in
          Build.if_file_exists p
            ~then_:(Build.lines_of p
                    >>^ function
                    | ver :: _ -> Some ver
                    | _ -> Some "")
            ~else_:(loop rest)
        | From_dune_project ->
          match version_from_dune_project sctx ~pkg with
          | None -> loop rest
          | Some _ as x -> Build.return x
    in
    loop
      [ File (Package.Name.version_fn pkg.name)
      ; From_dune_project
      ; File "version"
      ; File "VERSION"
      ]

let init_meta sctx ~dir =
  Local_package.defined_in sctx ~dir
  |> List.iter ~f:(fun pkg ->
    let libs = Local_package.libs pkg in
    let path = Local_package.build_dir pkg in
    let pkg_name = Local_package.name pkg in
    let meta = Local_package.meta_file pkg in
    let meta_template = Local_package.meta_template pkg in
    let version =
      let pkg = Local_package.package pkg in
      let get = pkg_version sctx ~pkg ~path in
      Super_context.Pkg_version.set sctx pkg get
    in

    gen_dune_package sctx ~version ~pkg;

    let template =
      Build.if_file_exists meta_template
        ~then_:(
          match Local_package.virtual_lib pkg with
          | Some lib ->
            Build.fail { fail = fun () ->
              Errors.fail (Loc.in_file meta_template)
                "Package %a defines virtual library %a and has a META \
                 template. This is not allowed."
                Package.Name.pp (Local_package.name pkg)
                Lib_name.pp (Lib.name lib)
            }
          | None ->
            Build.lines_of meta_template)
        ~else_:(Build.return ["# DUNE_GEN"])
    in
    let meta_contents =
      version >>^ fun version ->
      Gen_meta.gen
        ~package:(Package.Name.to_string pkg_name)
        ~version
        (Lib.Set.to_list libs)
    in
    let ctx = Super_context.context sctx in
    Super_context.add_rule sctx ~dir:ctx.build_dir
      (Build.fanout meta_contents template
       >>^ (fun ((meta : Meta.t), template) ->
         let buf = Buffer.create 1024 in
         let ppf = Format.formatter_of_buffer buf in
         Format.pp_open_vbox ppf 0;
         List.iter template ~f:(fun s ->
           if String.is_prefix s ~prefix:"#" then
             match
               String.extract_blank_separated_words (String.drop s 1)
             with
             | ["JBUILDER_GEN" | "DUNE_GEN"] ->
               Format.fprintf ppf "%a@," Meta.pp meta.entries
             | _ -> Format.fprintf ppf "%s@," s
           else
             Format.fprintf ppf "%s@," s);
         Format.pp_close_box ppf ();
         Format.pp_print_flush ppf ();
         Buffer.contents buf)
       >>>
       Build.write_file_dyn meta))

let lib_ppxs sctx ~(lib : Dune_file.Library.t) ~scope ~dir_kind =
  match lib.kind with
  | Normal | Ppx_deriver -> []
  | Ppx_rewriter ->
    let name = Dune_file.Library.best_name lib in
    match (dir_kind : Dune_lang.Syntax.t) with
    | Dune ->
      [Preprocessing.get_compat_ppx_exe sctx ~name ~kind:Dune]
    | Jbuild ->
      let driver =
        let deps =
          List.concat_map lib.buildable.libraries
            ~f:Dune_file.Lib_dep.to_lib_names
        in
        match
          List.filter deps ~f:(fun lib_name ->
            match Lib_name.to_string lib_name with
            | "ppx_driver" | "ppxlib" | "ppx_type_conv" -> true
            | _ -> false)
        with
        | [] -> None
        | l ->
          match Scope.name scope
              , List.mem ~set:l (Lib_name.of_string_exn ~loc:None "ppxlib")
          with
          | Named "ppxlib", _ | _, true ->
            Some "ppxlib.runner"
          | _ ->
            Some "ppx_driver.runner"
      in
      [Preprocessing.get_compat_ppx_exe sctx ~name ~kind:(Jbuild driver)]

let lib_install_files sctx ~dir_contents ~dir ~sub_dir ~scope ~dir_kind
      (lib : Library.t) =
  let loc = lib.buildable.loc in
  let make_entry section ?dst fn =
    ( Some loc
    , Install.Entry.make section fn
        ~dst:(
          let dst =
            match dst with
            | Some s -> s
            | None   -> Path.basename fn
          in
          match sub_dir with
          | None -> dst
          | Some dir -> sprintf "%s/%s" dir dst))
  in
  let installable_modules =
    Dir_contents.modules_of_library dir_contents
      ~name:(Library.best_name lib)
    |> Lib_modules.installable_modules
  in
  let sources =
    List.concat_map installable_modules ~f:(fun m ->
      List.map (Module.sources m) ~f:(fun source ->
        (* We add the -gen suffix to a few files generated by dune,
           such as the alias module. *)
        let dst = Path.basename source |> String.drop_suffix ~suffix:"-gen" in
        make_entry Lib source ?dst))
  in
  let ctx = Super_context.context sctx in
  let module_files =
    let if_ cond l = if cond then l else [] in
    let { Mode.Dict.byte ; native } =
      Dune_file.Mode_conf.Set.eval lib.modes
        ~has_native:(Option.is_some ctx.ocamlopt)
    in
    let virtual_library = Library.is_virtual lib in
    List.concat_map installable_modules ~f:(fun m ->
      List.concat
        [ if_ (Module.is_public m)
            [ Module.cm_public_file_unsafe m Cmi ]
        ; if_ (native && Module.has_impl m)
            [ Module.cm_file_unsafe m Cmx ]
        ; if_ (byte && Module.has_impl m && virtual_library)
            [ Module.cm_file_unsafe m Cmo ]
        ; if_ (native && Module.has_impl m && virtual_library)
            [ Module.obj_file m ~mode:Native ~ext:ctx.ext_obj ]
        ; List.filter_map Ml_kind.all ~f:(Module.cmt_file m)
        ])
  in
  let archives = Lib_archives.make ~ctx ~dir_contents ~dir lib in
  let execs = lib_ppxs sctx ~lib ~scope ~dir_kind in
  List.concat
    [ sources
    ; List.map module_files ~f:(make_entry Lib)
    ; List.map (Lib_archives.files archives) ~f:(make_entry Lib)
    ; List.map execs ~f:(make_entry Libexec)
    ; List.map (Lib_archives.dlls archives) ~f:(fun a ->
        (Some loc, Install.Entry.make Stublibs a))
    ]

let local_install_rules sctx (entries : (Loc.t option * Install.Entry.t) list)
      ~install_paths ~package =
  let ctx = Super_context.context sctx in
  let install_dir = Config.local_install_dir ~context:ctx.name in
  List.map entries ~f:(fun (loc, entry) ->
    let dst =
      Path.append install_dir
        (Install.Entry.relative_installed_path entry ~paths:install_paths)
    in
    let loc =
      match loc with
      | Some l -> l
      | None -> Loc.in_file entry.src
    in
    Build_system.set_package entry.src package;
    Super_context.add_rule sctx ~loc ~dir:ctx.build_dir
      (Build.symlink ~src:entry.src ~dst);
    Install.Entry.set_src entry dst)

let promote_install_file (ctx : Context.t) =
  not ctx.implicit &&
  match ctx.kind with
  | Default -> true
  | Opam _  -> false

let install_file sctx (package : Local_package.t) entries =
  let ctx = Super_context.context sctx in
  let opam = Local_package.opam_file package in
  let meta = Local_package.meta_file package in
  let dune_package = Local_package.dune_package_file package in
  let package_name = Local_package.name package in
  let pkg_build_dir = Local_package.build_dir package in
  let install_paths = Local_package.install_paths package in
  let entries =
    let docs =
      Local_package.odig_files package
      |> List.map ~f:(fun doc -> (None, Install.Entry.make Doc doc))
    in
    local_install_rules sctx ~package:package_name ~install_paths (
      (None, Install.Entry.make Lib opam ~dst:"opam")
      :: (None, Install.Entry.make Lib meta ~dst:"META")
      :: (None, Install.Entry.make Lib dune_package ~dst:"dune-package")
      :: docs)
    |> List.rev_append entries
  in
  let fn =
    Path.relative pkg_build_dir
      (Utils.install_file ~package:package_name
         ~findlib_toolchain:ctx.findlib_toolchain)
  in
  let files = Install.files entries in
  Build_system.Alias.add_deps
    (Build_system.Alias.package_install ~context:ctx ~pkg:package_name)
    (Dep.Set.of_files_set files)
    ~dyn_deps:
      (Build_system.package_deps package_name files
       >>^ fun packages ->
       Package.Name.Set.to_list packages
       |> List.map ~f:(fun pkg ->
         Build_system.Alias.package_install ~context:ctx ~pkg
         |> Dep.alias)
       |> Dep.Set.of_list);
  Super_context.add_rule sctx ~dir:pkg_build_dir
    ~mode:(if promote_install_file ctx then
             Promote (Until_clean, None)
           else
             (* We must ignore the source file since it might be
                copied to the source tree by another context. *)
             Ignore_source_files)
    (Build.path_set files
     >>^ (fun () ->
       let entries =
         match ctx.findlib_toolchain with
         | None -> entries
         | Some toolchain ->
           let prefix = Path.of_string (toolchain ^ "-sysroot") in
           List.map entries
             ~f:(Install.Entry.add_install_prefix
                   ~paths:install_paths ~prefix)
       in
       Install.gen_install_file entries)
     >>>
     Build.write_file_dyn fn)

let init_binary_artifacts sctx package =
  let installs =
    Local_package.installs package
    |> List.concat_map
         ~f:(fun ({ Dir_with_dune.
                    data =
                      { Dune_file.Install_conf. section; files; package = _ }
                  ; dune_version = _
                  ; ctx_dir = dir
                  ; src_dir = _
                  ; scope = _
                  ; kind = _ }) ->
              List.map files ~f:(fun { File_bindings. src ; dst } ->
                let (loc, src) = src in
                let dst = Option.map ~f:snd dst in
                ( Some loc
                , Install.Entry.make section (Path.relative dir src) ?dst
                )))
  in
  let install_paths = Local_package.install_paths package in
  let package = Local_package.name package in
  local_install_rules sctx ~package ~install_paths installs

let init_install sctx (package : Local_package.t) entries =
  let docs =
    Local_package.mlds package
    |> List.map ~f:(fun mld ->
      (None,
       Install.Entry.make
         ~dst:(sprintf "odoc-pages/%s" (Path.basename mld))
         Install.Section.Doc mld))
  in
  let lib_install_files =
    Local_package.lib_stanzas package
    |> List.concat_map
         ~f:(fun { Dir_with_dune.
                   data = (lib : Dune_file.Library.t)
                 ; scope
                 ; ctx_dir = dir
                 ; src_dir = _
                 ; kind = dir_kind
                 ; dune_version = _
                 } ->
              let sub_dir = (Option.value_exn lib.public).sub_dir in
              let dir_contents = Dir_contents.get_without_rules sctx ~dir in
              lib_install_files sctx ~dir ~sub_dir lib ~scope
                ~dir_kind ~dir_contents)
  in
  let entries =
    let install_paths = Local_package.install_paths package in
    let package = Local_package.name package in
    List.rev_append docs lib_install_files
    |> local_install_rules sctx ~package ~install_paths
    |> List.rev_append entries
  in
  install_file sctx package entries

let init_install_files (ctx : Context.t) (package : Local_package.t) =
  if not ctx.implicit then
    let install_fn =
      Utils.install_file ~package:(Local_package.name package)
        ~findlib_toolchain:ctx.findlib_toolchain
    in
    let path = Local_package.build_dir package in
    let install_alias = Alias.install ~dir:path in
    let install_file = Path.relative path install_fn in
    Build_system.Alias.add_deps install_alias (Dep.Set.of_files [install_file])

let init sctx =
  let packages = Local_package.of_sctx sctx in
  let ctx = Super_context.context sctx in
  let artifacts_per_package =
    Build_system.handle_add_rule_effects (fun () ->
      Package.Name.Map.map packages ~f:(init_binary_artifacts sctx))
  in
  Build_system.handle_add_rule_effects (fun () ->
    Package.Name.Map.iter packages ~f:(fun pkg ->
      Local_package.name pkg
      |> Package.Name.Map.find artifacts_per_package
      |> Option.value_exn
      |> init_install sctx pkg;
      init_install_files ctx pkg))
