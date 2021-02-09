open! Stdune
open! Dune_engine

let header_file_content =
  {|
#if defined( __clang__ )
  #define CCOMP clang
#elif defined( _MSC_VER )
  #define CCOMP msvc
#elif defined( __GNUC__ )
  #define CCOMP gcc
#else
  #define CCOMP other
#endif

CCOMP
|}

let rules ~sctx ~dir =
  let file = Path.Build.relative dir Cxx_flags.preprocessed_filename in
  let ocfg = (Super_context.context sctx).ocaml_config in
  let prog =
    Super_context.resolve_program sctx ~dir ~loc:None
      (Ocaml_config.c_compiler ocfg)
  in
  (* let tmp = Path.External.of_string (Filename.get_temp_dir_name ()) in *)
  let header_file = Path.Build.relative dir "header_check.h" in
  let write_test_file = Action.write_file header_file header_file_content in
  let args =
    let open Command.Args in
    [ (match Ocaml_config.ccomp_type ocfg with
      | Msvc -> A "/EP"
      | Other _ -> As [ "-E"; "-P" ])
    ; A Path.(to_absolute_filename (build header_file))
    ]
  in
  let action =
    let open Action_builder.With_targets.O in
    let+ run_preprocessor =
      Command.run ~dir:(Path.build dir) ~stdout_to:file prog args
    in
    Action.progn [ write_test_file; run_preprocessor ]
  in
  Super_context.add_rule sctx ~dir action
