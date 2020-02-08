open Import
open! No_io
open Build.O
module SC = Super_context
module Executables = Dune_file.Executables

let executables_rules ~sctx ~dir ~expander ~dir_contents ~scope ~compile_info
    (exes : Dune_file.Executables.t) =
  (* Use "eobjs" rather than "objs" to avoid a potential conflict with a library
     of the same name *)
  let obj_dir = Dune_file.Executables.obj_dir exes ~dir in
  Check_rules.add_obj_dir sctx ~obj_dir;
  let first_exe = snd (List.hd exes.names) in
  let modules =
    Dir_contents.modules_of_executables dir_contents ~first_exe ~obj_dir
  in
  let pp =
    Preprocessing.make sctx ~dir ~dep_kind:Required ~scope ~expander
      ~preprocess:exes.buildable.preprocess
      ~preprocessor_deps:exes.buildable.preprocessor_deps
      ~lint:exes.buildable.lint ~lib_name:None
  in
  let modules =
    Modules.map_user_written modules ~f:(fun m ->
        let name = Module.name m in
        Preprocessing.pp_module_as pp name m)
  in
  let programs =
    List.map exes.names ~f:(fun (loc, name) ->
        let mod_name = Module_name.of_string name in
        match Modules.find modules mod_name with
        | Some m ->
          if not (Module.has m ~ml_kind:Impl) then
            User_error.raise ~loc
              [ Pp.textf "Module %S has no implementation."
                  (Module_name.to_string mod_name)
              ]
          else
            { Exe.Program.name; main_module_name = mod_name; loc }
        | None ->
          User_error.raise ~loc
            [ Pp.textf "Module %S doesn't exist."
                (Module_name.to_string mod_name)
            ])
  in
  let ctx = SC.context sctx in
  let explicit_js_mode = Dune_project.explicit_js_mode (Scope.project scope) in
  let linkages =
    let module L = Dune_file.Executables.Link_mode in
    let l =
      let has_native = Option.is_some ctx.ocamlopt in
      let modes =
        let add_if_not_already_present modes mode loc =
          match L.Map.add exes.modes mode loc with
          | Ok modes -> modes
          | Error _ -> modes
        in
        match L.Map.find exes.modes L.js with
        | Some loc -> add_if_not_already_present exes.modes L.byte loc
        | None -> (
          if explicit_js_mode then
            exes.modes
          else
            match L.Map.find exes.modes L.byte with
            | Some loc -> add_if_not_already_present exes.modes L.js loc
            | None -> exes.modes )
      in
      List.filter_map (L.Map.to_list modes) ~f:(fun ((mode : L.t), loc) ->
          match (has_native, mode) with
          | false, Other { mode = Native; _ } -> None
          | _ -> Some (Exe.Linkage.of_user_config ctx ~loc mode))
    in
    (* If bytecode was requested but not native or best version, add custom
       linking *)
    if
      L.Map.mem exes.modes L.byte
      && (not (L.Map.mem exes.modes L.native))
      && not (L.Map.mem exes.modes L.exe)
    then
      Exe.Linkage.custom ctx :: l
    else
      l
  in
  let flags = SC.ocaml_flags sctx ~dir exes.buildable in
  let link_deps = SC.Deps.interpret sctx ~expander exes.link_deps in
  let foreign_archives = exes.buildable.foreign_archives |> List.map ~f:snd in
  let link_flags =
    link_deps
    >>> Expander.expand_and_eval_set expander exes.link_flags
          ~standard:(Build.return [])
  in
  (* TODO: Currently [exe_rules] differ from [lib_rules] in some aspects and the
     reason is unclear. For example, instead of building an archive for foreign
     stubs, we link the corresponding object files directly. It would be nice to
     make the code more uniform. *)
  let ext_lib = ctx.lib_config.ext_lib in
  let link_args =
    let+ flags = link_flags in
    Command.Args.S
      [ Command.Args.As flags
      ; Command.Args.S
          (List.map foreign_archives ~f:(fun archive ->
               let lib = Foreign.Archive.lib_file ~archive ~dir ~ext_lib in
               Command.Args.S [ A "-cclib"; Dep (Path.build lib) ]))
      ]
  in
  let requires_compile = Lib.Compile.direct_requires compile_info in
  let cctx =
    let requires_link = Lib.Compile.requires_link compile_info in
    let js_of_ocaml =
      let js_of_ocaml = exes.buildable.js_of_ocaml in
      if explicit_js_mode then
        Option.some_if (List.mem ~set:linkages Exe.Linkage.js) js_of_ocaml
      else
        Some js_of_ocaml
    in
    let dynlink =
      (* See https://github.com/ocaml/dune/issues/2527 *)
      true
      || Dune_file.Executables.Link_mode.Map.existsi exes.modes
           ~f:(fun mode _loc ->
             match mode with
             | Other { kind = Shared_object; _ } -> true
             | _ -> false)
    in
    Compilation_context.create () ~super_context:sctx ~expander ~scope ~obj_dir
      ~modules ~flags ~requires_link ~requires_compile ~preprocessing:pp
      ~js_of_ocaml ~opaque:(SC.opaque sctx) ~dynlink ~package:exes.package
  in
  let o_files =
    if not (Executables.has_foreign exes) then
      []
    else
      let what =
        if List.is_empty exes.buildable.Dune_file.Buildable.foreign_stubs then
          "archives"
        else
          "stubs"
      in
      if List.mem Exe.Linkage.byte ~set:linkages then
        User_error.raise ~loc:exes.buildable.loc
          [ Pp.textf "Pure bytecode executables cannot contain foreign %s." what
          ]
          ~hints:
            [ Pp.text
                "If you only need to build a native executable use \"(modes \
                 exe)\"."
            ];
      let foreign_sources =
        Dir_contents.foreign_sources_of_executables dir_contents ~first_exe
      in
      let o_files =
        Foreign_rules.build_o_files ~sctx ~dir ~expander
          ~requires:requires_compile ~dir_contents ~foreign_sources
        |> List.map ~f:Path.build
      in
      Check_rules.add_files sctx ~dir o_files;
      o_files
  in
  let requires_compile = Compilation_context.requires_compile cctx in
  Exe.build_and_link_many cctx ~programs ~linkages ~link_args ~o_files
    ~promote:exes.promote;
  ( cctx
  , Merlin.make () ~requires:requires_compile ~flags ~modules
      ~preprocess:(Dune_file.Buildable.single_preprocess exes.buildable)
      ~obj_dir )

let rules ~sctx ~dir ~dir_contents ~scope ~expander
    (exes : Dune_file.Executables.t) =
  let dune_version = Scope.project scope |> Dune_project.dune_version in
  let compile_info =
    Lib.DB.resolve_user_written_deps_for_exes (Scope.libs scope) exes.names
      exes.buildable.libraries
      ~pps:(Dune_file.Preprocess_map.pps exes.buildable.preprocess)
      ~dune_version
      ~allow_overlaps:exes.buildable.allow_overlapping_dependencies
      ~variants:exes.variants ~optional:exes.optional
      ~forbidden_libraries:exes.forbidden_libraries
  in
  let f () =
    executables_rules exes ~sctx ~dir ~dir_contents ~scope ~expander
      ~compile_info
  in
  SC.Libs.gen_select_rules sctx compile_info ~dir;
  Bootstrap_info.gen_rules sctx exes ~dir compile_info;
  SC.Libs.with_lib_deps sctx compile_info ~dir ~f
