(** To add a new third-party library, you must first create a new update script
    that will put in place the sources into the [boot/third-party]
    folder.contents. This script must contain an exact source, such as a release
    tarball or a URL and revision. The copied files must be comitted in the
    repository.

    The script should copy only the ml/mli files and not the [dune] files other
    files since these are not used during bootstrap. To let the bootstrap know
    about this new library, you should then add it to the bellow list.

    Note that there is no entry or script for the "result" library. This is
    because this library is very simple and we only need compatiblilty with
    recent versions of OCaml, so we don't need the full complexity of this
    library.

    In the bellow list, each library is a tuple of:

    - the library name
    - directory relative to the third-party directory
    - toplevel module name of the library, or [None] if the library is not
      wrapped
    - whether to scan sub-directories or not *)

let libs =
  [ ("csexp", "csexp/src", Some "Csexp", false)
  ; ("result", "result", Some "Result", false)
  ; ("pp", "pp/src", Some "Pp", false)
  ; ( "build_path_prefix_map"
    , "build_path_prefix_map"
    , Some "Build_path_prefix_map"
    , false )
  ; ("opam-file-format", "opam-file-format/src", None, false)
  ; ("cmdliner", "cmdliner/src", None, false)
  ; ("re", "re/lib", Some "Re", false)
  ]
