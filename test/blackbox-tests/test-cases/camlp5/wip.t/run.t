In the directory rewriter1 we build external rewriter by direct invokation of camlp5
Magic below to disable machine-dependent paths
See https://stackoverflow.com/a/3618308/1065436
  $ dune build rewriter1/pp5+dump.byte rewriter1/pp5+o.byte 3>&1 1>&2 2>&3 3>&- | sed '/Interface topdirs.cmi occurs in several directories/d'	#| sed '/ocamlfind: \[WARNING\] Package/d'
      mkcamlp5 rewriter1/pp5+dump.byte
  ocamlfind: [WARNING] Package `camlp5.extend': camlp5.extend SHOULD NOT be used with camlp5.pa_o
      mkcamlp5 rewriter1/pp5+o.byte
  ocamlfind: [WARNING] Package `camlp5.extend': camlp5.extend SHOULD NOT be used with camlp5.pa_o

In the directory rewriter2 we build a rewriter using dune's stanzas
and a libnrary that is being preprocessed by it

  $ dune build rewriter2/librewriter2.cmxa 3>&1 1>&2 2>&3 3>&- | sed '/Interface topdirs.cmi occurs in several directories/d'	| sed '/ocamlfind: \[WARNING\] Package/d'
      mkcamlp5 .camlp5/2c0d6cec43b8a3e2c47ab007fa0ebd5c/rewriter.exe
  $ dune build rewriter2/lib2.cmxa

There we check that generated META file is adequate
  $ dune build @install
  $ cat _build/default/META.proj1
  package "lib2" (
    directory = "lib2"
    description = "A library which uses proj1.pa1 to compile itself"
    requires = ""
    archive(byte) = "lib2.cma"
    archive(native) = "lib2.cmxa"
    plugin(byte) = "lib2.cma"
    plugin(native) = "lib2.cmxs"
  )
  package "pa1" (
    directory = "pa1"
    description = "Syntax extension number 1"
    requires(syntax,preprocessor) = "camlp5"
    archive(syntax,preprocessor,byte) = "librewriter2.cma"
    archive(syntax,preprocessor,native) = "librewriter2.cmxa"
    plugin(syntax,preprocessor,byte) = "librewriter2.cma"
    plugin(syntax,preprocessor,native) = "librewriter2.cmxs"
  )
