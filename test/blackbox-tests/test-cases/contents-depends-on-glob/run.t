There are some install rules, and there is also some globbing.

This can create a dependency cycle if "install rules" try to
produce targets in the directory being globbed.

(install rules depend on library artifacts, which in turn depend
on the directory listing)

  $ dune build @install
  $ cat _build/default/bar/bar.install
  lib: [
    "_build/install/default/lib/bar/META"
    "_build/install/default/lib/bar/bar.cmi"
    "_build/install/default/lib/bar/bar.cmt"
    "_build/install/default/lib/bar/bar.cmx"
    "_build/install/default/lib/bar/bar.ml"
    "_build/install/default/lib/bar/bar__.cmi"
    "_build/install/default/lib/bar/bar__.cmt"
    "_build/install/default/lib/bar/bar__.cmx"
    "_build/install/default/lib/bar/bar__.ml"
    "_build/install/default/lib/bar/bar__Baz.cmi"
    "_build/install/default/lib/bar/bar__Baz.cmt"
    "_build/install/default/lib/bar/bar__Baz.cmx"
    "_build/install/default/lib/bar/baz.ml"
    "_build/install/default/lib/bar/dune-package"
    "_build/install/default/lib/bar/lib$ext_lib"
    "_build/install/default/lib/bar/lib.cma"
    "_build/install/default/lib/bar/lib.cmxa"
    "_build/install/default/lib/bar/lib.cmxs"
    "_build/install/default/lib/bar/opam"
  ]
