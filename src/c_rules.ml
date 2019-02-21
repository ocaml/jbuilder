open! Stdune
open Import
open Build.O
open! No_io

module Library = Dune_file.Library
module SC = Super_context

module Gen (P : Install_rules.Params) = struct

  let sctx = P.sctx
  let ctx = SC.context sctx

  let build_c_file ~flags ~expander ~dir ~includes (loc, src, dst) =
    let src = C.Source.path src in
    SC.add_rule sctx ~loc ~dir
      (Expander.expand_and_eval_set expander flags
         ~standard:(Build.return (Context.cc_g ctx))
       >>>
       Build.run
         (* We have to execute the rule in the library directory as
            the .o is produced in the current directory *)
         ~dir
         (Ok ctx.ocamlc)
         [ A "-g"
         ; includes
         ; Dyn (fun c_flags -> Arg_spec.quote_args "-ccopt" c_flags)
         ; A "-o"; Target dst
         ; Dep src
         ]);
    dst

  let build_cxx_file ~flags ~expander ~dir ~includes (loc, src, dst) =
    let src = C.Source.path src in
    let open Arg_spec in
    let output_param =
      if ctx.ccomp_type = "msvc" then
        [Concat ("", [A "/Fo"; Target dst])]
      else
        [A "-o"; Target dst]
    in
    SC.add_rule sctx ~loc ~dir
      (Expander.expand_and_eval_set expander flags
         ~standard:(Build.return (Context.cc_g ctx))
       >>>
       Build.run
         (* We have to execute the rule in the library directory as
            the .o is produced in the current directory *)
         ~dir
         (SC.resolve_program ~loc:None ~dir sctx ctx.c_compiler)
         ([ S [A "-I"; Path ctx.stdlib_dir]
          ; As (SC.cxx_flags sctx)
          ; includes
          ; Dyn (fun cxx_flags -> As cxx_flags)
          ] @ output_param @
          [ A "-c"; Dep src
          ]));
    dst

  let build_o_files ~(c_sources : C.Sources.t)
        ~(c_flags : Ordered_set_lang.Unexpanded.t C.Kind.Dict.t)
        ~dir ~expander ~requires ~dir_contents =
    let all_dirs = Dir_contents.dirs dir_contents in
    let h_files =
      List.fold_left all_dirs ~init:[] ~f:(fun acc dc ->
        String.Set.fold (Dir_contents.text_files dc) ~init:acc
          ~f:(fun fn acc ->
            if String.is_suffix fn ~suffix:".h" then
              Path.relative (Dir_contents.dir dc) fn :: acc
            else
              acc))
    in
    let includes =
      Arg_spec.S
        [ Hidden_deps h_files
        ; Arg_spec.of_result_map requires ~f:(fun libs ->
            S [ Lib.L.c_include_flags libs ~stdlib_dir:ctx.stdlib_dir
              ; Hidden_deps (Lib_file_deps.file_deps libs
                               ~groups:[Lib_file_deps.Group.Header])
              ])
        ]
    in

    let build_x_files (kind : C.Kind.t) files =
      let flags = C.Kind.Dict.get c_flags kind in
      let build =
        match kind with
        | C -> build_c_file
        | Cxx -> build_cxx_file
      in
      String.Map.to_list files
      |> List.map ~f:(fun (obj, (loc, src)) ->
        let dst = Path.relative dir (obj ^ ctx.ext_obj) in
        build ~flags ~expander ~dir ~includes (loc, src, dst)
      )
    in
    let { C.Kind.Dict. c ; cxx } =
      C.Sources.split_by_kind c_sources
      |> C.Kind.Dict.mapi ~f:build_x_files
    in
    c @ cxx

  let executables_rules ~dir ~expander ~dir_contents ~compile_info
        (exes : Dune_file.C_executables.t) =
    let c_sources =
      Dir_contents.c_sources_of_executables dir_contents
        ~first_exe:(snd (List.hd exes.names))
    in
    let o_files =
      let requires_compile = Lib.Compile.direct_requires compile_info in
      let c_flags =
        { C.Kind.Dict.
          c = exes.c_flags
        ; cxx = exes.cxx_flags
        } in
      build_o_files ~c_flags ~c_sources ~dir ~expander
        ~requires:requires_compile ~dir_contents
    in
    let c_compiler = Ok (Context.c_compiler ctx) in
    List.iter exes.names ~f:(fun (loc, exe) ->
      let o_files = List.filter o_files ~f:(fun obj ->
        let obj = Path.basename obj in
        List.for_all exes.names ~f:(fun (_, exe') ->
          exe = exe' || exe' <> obj))
      in
      let target = Path.relative dir (exe ^ ".exe") in
      Build.run ~dir
        c_compiler
        [ A "-o"; Target target
        ; Deps o_files
        ]
      |> Super_context.add_rule sctx ~loc ~dir
    )

  let exe_rules ~dir ~dir_contents ~scope ~expander
        (exes : Dune_file.C_executables.t) =
    let compile_info =
      Lib.DB.resolve_user_written_deps_for_exes
        (Scope.libs scope)
        exes.names
        exes.libraries
        ~pps:[]
        ~allow_overlaps:false
    in
    SC.Libs.gen_select_rules sctx compile_info ~dir;
    SC.Libs.with_lib_deps sctx compile_info ~dir
      ~f:(fun () ->
        executables_rules exes ~dir
          ~dir_contents ~expander ~compile_info)
end
