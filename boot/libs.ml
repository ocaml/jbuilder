let executables = [ "main" ]

let external_libraries = [ "unix"; "threads.posix" ]

let local_libraries =
  [ ("boot/third-party/result", Some "Result", false, None)
  ; ("boot/third-party/pp/src", Some "Pp", false, None)
  ; ("boot/third-party/csexp/src", Some "Csexp", false, None)
  ; ("src/dune_filesystem_stubs", Some "Dune_filesystem_stubs", false, None)
  ; ("src/stdune", Some "Stdune", false, None)
  ; ("src/dune_lang", Some "Dune_lang", false, None)
  ; ("vendor/incremental-cycles/src", Some "Incremental_cycles", false, None)
  ; ("src/dag", Some "Dag", false, None)
  ; ("src/fiber", Some "Fiber", false, None)
  ; ("src/memo", Some "Memo", false, None)
  ; ("src/dune_util", Some "Dune_util", false, None)
  ; ("src/xdg", Some "Xdg", false, None)
  ; ("src/cache", Some "Cache", false, None)
  ; ("src/cache_daemon", Some "Cache_daemon", false, None)
  ; ("boot/third-party/re/lib", Some "Re", false, None)
  ; ("boot/third-party/opam-file-format/src", None, false, None)
  ; ("otherlibs/dune-glob/src", Some "Dune_glob", false, None)
  ; ("src/ocaml-config", Some "Ocaml_config", false, None)
  ; ("src/catapult", Some "Catapult", false, None)
  ; ("src/jbuild_support", Some "Jbuild_support", false, None)
  ; ("otherlibs/action-plugin/src", Some "Dune_action_plugin", false, None)
  ; ("src/meta_parser", Some "Dune_meta_parser", false, None)
  ; ("src/section", Some "Dune_section", false, None)
  ; ("boot/third-party/build_path_prefix_map", Some "Build_path_prefix_map",
    false, None)
  ; ("src/dune_engine", Some "Dune_engine", false, None)
  ; ("src/dune_rules", Some "Dune_rules", true, None)
  ; ("boot/third-party/cmdliner/src", None, false, None)
  ; ("otherlibs/build-info/src", Some "Build_info", false,
    Some "Build_info_data")
  ]
