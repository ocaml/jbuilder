  $ dune runtest --force --display short
  description = "contains \"quotes\""
  requires = "bytes"
  archive(byte) = "lib.cma"
  archive(native) = "lib.cmxa"
  plugin(byte) = "lib.cma"
  plugin(native) = "lib.cmxs"
  package "baz" (
    directory = "baz"
    description = "sub library with modes set to byte"
    requires = "bytes"
    archive(byte) = "lib.cma"
    archive(native) = "lib.cmxa"
    plugin(byte) = "lib.cma"
    plugin(native) = "lib.cmxs"
  )
  package "ppd" (
    directory = "ppd"
    description = "pp'd with a rewriter"
    requires = "foobar foobar.baz foobar.runtime-lib2"
    archive(byte) = "lib.cma"
    archive(native) = "lib.cmxa"
    plugin(byte) = "lib.cma"
    plugin(native) = "lib.cmxs"
  )
  package "rewriter" (
    directory = "rewriter"
    description = "ppx rewriter"
    requires(ppx_driver) = "foobar foobar.rewriter2"
    archive(ppx_driver,byte) = "lib.cma"
    archive(ppx_driver,native) = "lib.cmxa"
    plugin(ppx_driver,byte) = "lib.cma"
    plugin(ppx_driver,native) = "lib.cmxs"
    # This is what dune uses to find out the runtime dependencies of
    # a preprocessor
    ppx_runtime_deps = "foobar.baz"
    # This line makes things transparent for people mixing preprocessors
    # and normal dependencies
    requires(-ppx_driver) = "foobar.baz foobar.runtime-lib2"
    ppx(-ppx_driver,-custom_ppx) = "./ppx.exe --as-ppx"
    library_kind = "ppx_rewriter"
  )
  package "rewriter2" (
    directory = "rewriter2"
    description = "ppx rewriter expander"
    requires = "foobar"
    archive(byte) = "lib.cma"
    archive(native) = "lib.cmxa"
    plugin(byte) = "lib.cma"
    plugin(native) = "lib.cmxs"
    # This is what dune uses to find out the runtime dependencies of
    # a preprocessor
    ppx_runtime_deps = "foobar.runtime-lib2"
  )
  package "runtime-lib2" (
    directory = "runtime-lib2"
    description = "runtime library for foobar.rewriter2"
    requires = ""
    archive(byte) = "lib.cma"
    archive(native) = "lib.cmxa"
    plugin(byte) = "lib.cma"
    plugin(native) = "lib.cmxs"
    linkopts(javascript) = "+foobar/foobar_runtime.js
                            +foobar/foobar_runtime2.js"
    jsoo_runtime = "foobar_runtime.js foobar_runtime2.js"
  )
  package "sub" (
    directory = "sub"
    description = "sub library in a sub dir"
    requires = "bytes"
    archive(byte) = "lib.cma"
    archive(native) = "lib.cmxa"
    plugin(byte) = "lib.cma"
    plugin(native) = "lib.cmxs"
  )
