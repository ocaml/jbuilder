open! Stdune
open Import
open Build.O
open! No_io

module Buildable = Dune_file.Buildable
module Library = Dune_file.Library
module Mode_conf = Dune_file.Mode_conf

module SC = Super_context

module Gen (P : Install_rules.Params) = struct
  module Odoc = Odoc.Gen(P)
  module C_rules = C_rules.Gen(P)

  let sctx = P.sctx
  let ctx = SC.context sctx

  let opaque = SC.opaque sctx

  let msvc_hack_cclibs =
    List.map ~f:(fun lib ->
      let lib =
        match String.drop_prefix lib ~prefix:"-l" with
        | None -> lib
        | Some l -> l ^ ".lib"
      in
      Option.value ~default:lib (String.drop_prefix ~prefix:"-l" lib))

  let build_lib (lib : Library.t) ~expander ~flags ~dir ~mode
        ~top_sorted_modules ~modules =
    let kind = Mode.cm_kind mode in
    Option.iter (Context.compiler ctx mode) ~f:(fun compiler ->
      let target = Library.archive lib ~dir ~ext:(Mode.compiled_lib_ext mode) in
      let stubs_flags =
        if not (Library.has_stubs lib) then
          []
        else
          let stubs_name = Library.stubs_name lib in
          match mode with
          | Byte -> ["-dllib"; "-l" ^ stubs_name; "-cclib"; "-l" ^ stubs_name]
          | Native -> ["-cclib"; "-l" ^ stubs_name]
      in
      let map_cclibs =
        (* https://github.com/ocaml/dune/issues/119 *)
        if ctx.ccomp_type = "msvc" then
          msvc_hack_cclibs
        else
          Fn.id
      in
      let artifacts ~ext modules =
        List.map modules ~f:(Module.obj_file ~mode ~ext)
      in
      let obj_deps =
        Build.paths (artifacts modules ~ext:(Cm_kind.ext kind))
      in
      let obj_deps =
        match mode with
        | Byte   -> obj_deps
        | Native ->
          obj_deps >>>
          Build.paths (artifacts modules ~ext:ctx.ext_obj)
      in
      SC.add_rule ~dir sctx ~loc:lib.buildable.loc
        (obj_deps
         >>>
         Build.fanout4
           (top_sorted_modules >>^artifacts ~ext:(Cm_kind.ext kind))
           (Expander.expand_and_eval_set expander lib.c_library_flags
              ~standard:(Build.return []))
           (Ocaml_flags.get flags mode)
           (Expander.expand_and_eval_set expander lib.library_flags
              ~standard:(Build.return []))
         >>>
         Build.run (Ok compiler) ~dir:ctx.build_dir
           [ Dyn (fun (_, _, flags, _) -> As flags)
           ; A "-a"; A "-o"; Target target
           ; As stubs_flags
           ; Dyn (fun (_, cclibs, _, _) -> Arg_spec.quote_args "-cclib" (map_cclibs cclibs))
           ; Dyn (fun (_, _, _, library_flags) -> As library_flags)
           ; As (match lib.kind with
               | Normal -> []
               | Ppx_deriver | Ppx_rewriter -> ["-linkall"])
           ; Dyn (fun (cm_files, _, _, _) -> Deps cm_files)
           ; Hidden_targets
               (match mode with
                | Byte -> []
                | Native -> [Library.archive lib ~dir ~ext:ctx.ext_lib])
           ]))

  (* If the compiler reads the cmi for module alias even with
     [-w -49 -no-alias-deps], we must sandbox the build of the
     alias module since the modules it references are built after. *)
  let alias_module_build_sandbox =
    Ocaml_version.always_reads_alias_cmi ctx.version

  let build_alias_module ~lib_modules ~dir ~cctx ~dynlink ~js_of_ocaml =
    let vimpl = Compilation_context.vimpl cctx in
    let alias_module =
      Option.value_exn (Lib_modules.alias_module lib_modules) in
    let file =
      match Module.impl alias_module with
      | Some f -> f
      | None -> Option.value_exn (Module.intf alias_module)
    in
    let alias_file () =
      let main_module_name =
        Option.value_exn (Lib_modules.main_module_name lib_modules)
      in
      Vimpl.aliased_modules vimpl lib_modules
      |> Module.Name.Map.values
      |> List.map ~f:(fun (m : Module.t) ->
        let name = Module.Name.to_string (Module.name m) in
        sprintf "(** @canonical %s.%s *)\n\
                module %s = %s\n"
          (Module.Name.to_string main_module_name)
          name
          name
          (Module.Name.to_string (Module.real_unit_name m)))
      |> String.concat ~sep:"\n"
    in
    SC.add_rule sctx ~dir (
      Build.arr alias_file >>> Build.write_file_dyn file.path
    );
    let cctx = Compilation_context.for_alias_module cctx in
    Module_compilation.build_module cctx alias_module
      ~js_of_ocaml
      ~dynlink
      ~sandbox:alias_module_build_sandbox
      ~dep_graphs:(Dep_graph.Ml_kind.dummy alias_module)

  let build_wrapped_compat_modules (lib : Library.t) cctx ~js_of_ocaml
        ~dynlink ~lib_modules =
    let wrapped_compat = Lib_modules.wrapped_compat lib_modules in
    let modules = Lib_modules.modules lib_modules in
    let wrapped = Lib_modules.wrapped lib_modules in
    let transition_message = lazy (
        match (wrapped : Wrapped.t) with
        | Simple _ -> assert false
        | Yes_with_transition r -> r)
    in
    Module.Name.Map.iteri wrapped_compat ~f:(fun name m ->
      let main_module_name =
        match Library.main_module_name lib with
        | This (Some mmn) -> Module.Name.to_string mmn
        | _ -> assert false
      in
      let contents =
        let name = Module.Name.to_string name in
        let hidden_name = sprintf "%s__%s" main_module_name name in
        let real_name = sprintf "%s.%s" main_module_name name in
        sprintf {|[@@@deprecated "%s. Use %s instead."] include %s|}
          (Lazy.force transition_message) real_name hidden_name
      in
      let source_path = Option.value_exn (Module.file m Impl) in
      Build.return contents
      >>> Build.write_file_dyn source_path
      |> SC.add_rule sctx ~dir:(Compilation_context.dir cctx)
    );
    let dep_graphs =
      Dep_graph.Ml_kind.wrapped_compat ~modules ~wrapped_compat
    in
    let cctx = Compilation_context.for_wrapped_compat cctx wrapped_compat in
    Module_compilation.build_modules cctx ~js_of_ocaml ~dynlink ~dep_graphs

  let ocamlmklib (lib : Library.t) ~dir ~expander ~o_files ~sandbox ~custom
        ~targets =
    SC.add_rule sctx ~sandbox ~dir
      ~loc:lib.buildable.loc
      (Expander.expand_and_eval_set expander
         lib.c_library_flags ~standard:(Build.return [])
       >>>
       Build.run ~dir:ctx.build_dir
         (Ok ctx.ocamlmklib)
         [ A "-g"
         ; if custom then A "-custom" else As []
         ; A "-o"
         ; Path (Library.stubs lib ~dir)
         ; Deps o_files
         ; Dyn (fun cclibs ->
             (* https://github.com/ocaml/dune/issues/119 *)
             if ctx.ccomp_type = "msvc" then
               let cclibs = msvc_hack_cclibs cclibs in
               Arg_spec.quote_args "-ldopt" cclibs
             else
               As cclibs
           )
         ; Hidden_targets targets
         ])

  let build_self_stubs lib ~expander ~dir ~o_files =
    let static = Library.stubs_archive lib ~dir ~ext_lib:ctx.ext_lib in
    let dynamic = Library.dll lib ~dir ~ext_dll:ctx.ext_dll in
    let modes =
      Mode_conf.Set.eval lib.modes
        ~has_native:(Option.is_some ctx.ocamlopt) in
    let ocamlmklib = ocamlmklib lib ~expander ~dir ~o_files in
    if modes.native &&
       modes.byte   &&
       Dynlink_supported.get lib.dynlink ctx.supports_shared_libraries
    then begin
      (* If we build for both modes and support dynlink, use a
         single invocation to build both the static and dynamic
         libraries *)
      ocamlmklib ~sandbox:false ~custom:false ~targets:[static; dynamic]
    end else begin
      ocamlmklib ~sandbox:false ~custom:true ~targets:[static];
      (* We can't tell ocamlmklib to build only the dll, so we
         sandbox the action to avoid overriding the static archive *)
      ocamlmklib ~sandbox:true ~custom:false ~targets:[dynamic]
    end

  let build_stubs lib ~dir ~expander ~requires ~dir_contents
        ~vlib_stubs_o_files =
    let lib_o_files =
      if Library.has_stubs lib then
        let c_flags =
          { C.Kind.Dict.
            c = lib.c_flags
          ; cxx = lib.cxx_flags
          }
        in
        let c_sources = Dir_contents.c_sources_of_library
                          dir_contents ~name:(Library.best_name lib) in
        C_rules.build_o_files
          ~c_sources
          ~c_flags
          ~dir ~expander ~requires ~dir_contents
      else
        []
    in
    match vlib_stubs_o_files @ lib_o_files with
    | [] -> ()
    | o_files -> build_self_stubs lib ~dir ~expander ~o_files

  let build_shared lib ~dir ~flags ~(ctx : Context.t) =
    Option.iter ctx.ocamlopt ~f:(fun ocamlopt ->
      let src = Library.archive lib ~dir ~ext:(Mode.compiled_lib_ext Native) in
      let dst = Library.archive lib ~dir ~ext:".cmxs" in
      let build =
        Build.dyn_paths (Build.arr (fun () ->
          [Library.archive lib ~dir ~ext:ctx.ext_lib]))
        >>>
        Ocaml_flags.get flags Native
        >>>
        Build.run ~dir:ctx.build_dir
          (Ok ocamlopt)
          [ Dyn (fun flags -> As flags)
          ; A "-shared"; A "-linkall"
          ; A "-I"; Path dir
          ; A "-o"; Target dst
          ; Dep src
          ]
      in
      let build =
        if Library.has_stubs lib then
          Build.path (Library.stubs_archive ~dir lib ~ext_lib:ctx.ext_lib)
          >>>
          build
        else
          build
      in
      SC.add_rule sctx build ~dir)

  let setup_build_archives (lib : Dune_file.Library.t)
        ~wrapped_compat ~cctx ~(dep_graphs : Dep_graph.Ml_kind.t)
        ~expander
        ~vlib_dep_graphs =
    let dir = Compilation_context.dir cctx in
    let obj_dir = Compilation_context.obj_dir cctx in
    let flags = Compilation_context.flags cctx in
    let modules = Compilation_context.modules cctx in
    let js_of_ocaml = lib.buildable.js_of_ocaml in
    let vimpl = Compilation_context.vimpl cctx in
    let modules =
      match lib.stdlib with
      | Some { exit_module = Some name; _ } -> begin
          match Module.Name.Map.find modules name with
          | None -> modules
          | Some m ->
            (* These files needs to be alongside stdlib.cma as the
               compiler implicitly adds this module. *)
            [ Mode.Native, ".cmx"
            ; Byte, ".cmo"
            ; Native, ctx.ext_obj ]
            |> List.iter ~f:(fun (mode, ext) ->
              let src = Module.obj_file m ~mode ~ext in
              let dst = Path.relative dir ((Module.obj_name m) ^ ext) in
              SC.add_rule sctx ~dir (Build.copy ~src ~dst));
            Module.Name.Map.remove modules name
        end
      | _ ->
        modules
    in
    let modules = List.rev_append
                    (Module.Name_map.impl_only modules)
                    (Vimpl.impl_only vimpl) in
    let wrapped_compat = Module.Name.Map.values wrapped_compat in
    (* Compatibility modules have implementations so we can just append them.
       We append the modules at the end as no library modules depend on
       them. *)
    let top_sorted_modules =
      match vlib_dep_graphs with
      | None ->
        Dep_graph.top_closed_implementations dep_graphs.impl modules
        >>^ fun modules -> modules @ wrapped_compat
      | Some (vlib_dep_graphs : Dep_graph.Ml_kind.t) ->
        Dep_graph.top_closed_multi_implementations
          [ vlib_dep_graphs.impl
          ; dep_graphs.impl
          ]
          modules
    in

    let modes =
      Mode_conf.Set.eval lib.modes
        ~has_native:(Option.is_some ctx.ocamlopt) in
    (let modules = modules @ wrapped_compat in
     Mode.Dict.Set.to_list modes
     |> List.iter ~f:(fun mode ->
       build_lib lib ~expander ~flags ~dir ~mode ~top_sorted_modules
         ~modules));
    (* Build *.cma.js *)
    if modes.byte then
      SC.add_rules sctx ~dir (
        let src =
          Library.archive lib ~dir
            ~ext:(Mode.compiled_lib_ext Mode.Byte) in
        let target =
          Path.relative (Obj_dir.obj_dir obj_dir) (Path.basename src)
          |> Path.extend_basename ~suffix:".js" in
        Js_of_ocaml_rules.build_cm cctx ~js_of_ocaml ~src ~target);
    if Dynlink_supported.By_the_os.get ctx.natdynlink_supported
    && modes.native then
        build_shared lib ~dir ~flags ~ctx

  let library_rules (lib : Library.t) ~dir_contents ~dir ~expander ~scope
        ~compile_info ~dir_kind =
    let dep_kind =
      if lib.optional then Lib_deps_info.Kind.Optional else Required
    in
    let flags = SC.ocaml_flags sctx ~dir lib.buildable in
    let lib_modules =
      Dir_contents.modules_of_library dir_contents ~name:(Library.best_name lib)
    in
    let obj_dir = Library.obj_dir ~dir lib in
    Check_rules.add_obj_dir sctx ~obj_dir;
    let source_modules = Lib_modules.modules lib_modules in
    let vimpl =
      Virtual_rules.impl sctx ~lib ~dir ~scope ~modules:source_modules in
    Option.iter vimpl ~f:(Virtual_rules.setup_copy_rules_for_impl ~sctx ~dir);
    (* Preprocess before adding the alias module as it doesn't need
       preprocessing *)
    let pp =
      Preprocessing.make sctx ~dir ~dep_kind ~scope
        ~preprocess:lib.buildable.preprocess
        ~expander
        ~preprocessor_deps:
          (SC.Deps.interpret sctx ~expander lib.buildable.preprocessor_deps)
        ~lint:lib.buildable.lint
        ~lib_name:(Some (snd lib.name))
        ~dir_kind
    in

    let lib_modules =
      Preprocessing.pp_modules pp source_modules
      |> Lib_modules.set_modules lib_modules
    in

    let alias_module = Lib_modules.alias_module lib_modules in
    let modules = Lib_modules.for_compilation lib_modules in

    let cctx =
      let requires_compile = Lib.Compile.direct_requires compile_info in
      let requires_link    = Lib.Compile.requires_link compile_info in
      Compilation_context.create ()
        ~super_context:sctx
        ~expander
        ?vimpl
        ~scope
        ~dir_kind
        ~obj_dir
        ~modules
        ?alias_module
        ?lib_interface_module:(Lib_modules.lib_interface_module lib_modules)
        ~flags
        ~requires_compile
        ~requires_link
        ~preprocessing:pp
        ~no_keep_locs:lib.no_keep_locs
        ~opaque
        ?stdlib:lib.stdlib
    in

    let requires_compile = Compilation_context.requires_compile cctx in

    let dynlink =
      Dynlink_supported.get lib.dynlink ctx.supports_shared_libraries
    in
    let js_of_ocaml = lib.buildable.js_of_ocaml in

    build_wrapped_compat_modules lib cctx ~dynlink ~js_of_ocaml ~lib_modules;

    let (vlib_dep_graphs, dep_graphs) =
      let dep_graphs = Ocamldep.rules cctx in
      match vimpl with
      | None ->
        (None, dep_graphs)
      | Some impl ->
        let vlib = Vimpl.vlib_dep_graph impl in
        ( Some vlib
        , Dep_graph.Ml_kind.merge_for_impl ~vlib ~impl:dep_graphs
        )
    in

    Module_compilation.build_modules cctx ~js_of_ocaml ~dynlink ~dep_graphs;

    if Option.is_none lib.stdlib
    && Lib_modules.needs_alias_module lib_modules then
      build_alias_module ~dir ~lib_modules ~cctx ~dynlink ~js_of_ocaml;

    let expander = Super_context.expander sctx ~dir in

    let vlib_stubs_o_files = Vimpl.vlib_stubs_o_files vimpl in
    if Library.has_stubs lib || not (List.is_empty vlib_stubs_o_files) then
      build_stubs lib ~dir ~expander ~requires:requires_compile
        ~dir_contents ~vlib_stubs_o_files;

    Lib_file_deps.setup_file_deps ~lib ~dir
      ~modules:(Lib_modules.have_artifacts lib_modules
                |> Module.Name.Map.values
                |> Vimpl.for_file_deps vimpl);

    if not (Library.is_virtual lib) then (
      let wrapped_compat = Lib_modules.wrapped_compat lib_modules in
      setup_build_archives lib ~wrapped_compat ~cctx ~dep_graphs
        ~vlib_dep_graphs ~expander
    );

    Odoc.setup_library_odoc_rules lib ~requires:requires_compile
      ~modules ~dep_graphs ~scope;

    let flags =
      match alias_module with
      | None -> Ocaml_flags.common flags
      | Some m ->
        Ocaml_flags.prepend_common
          ["-open"; Module.Name.to_string (Module.name m)] flags
        |> Ocaml_flags.common
    in

    Sub_system.gen_rules
      { super_context = sctx
      ; dir
      ; stanza = lib
      ; scope
      ; source_modules
      ; compile_info
      };

    let objs_dirs = Path.Set.singleton (Obj_dir.byte_dir obj_dir) in

    (cctx,
     Merlin.make ()
       ~requires:requires_compile
       ~flags
       ~preprocess:(Buildable.single_preprocess lib.buildable)
       ~libname:(snd lib.name)
       ~objs_dirs
    )

  let rules (lib : Library.t) ~dir_contents ~dir ~expander ~scope
        ~dir_kind : Compilation_context.t * Merlin.t =
    let compile_info =
      Lib.DB.get_compile_info (Scope.libs scope) (Library.best_name lib)
        ~allow_overlaps:lib.buildable.allow_overlapping_dependencies
    in
    SC.Libs.gen_select_rules sctx compile_info ~dir;
    SC.Libs.with_lib_deps sctx compile_info ~dir
      ~f:(fun () ->
        library_rules lib ~dir_contents ~dir ~scope ~expander ~compile_info
          ~dir_kind)

end
