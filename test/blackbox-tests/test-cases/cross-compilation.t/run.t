  $ env OCAMLFIND_CONF=$PWD/etc/findlib.conf dune build --display short -x foo file @install --promote-install-files
        ocamlc lib/.p.objs/byte/p.{cmi,cmo,cmt} [default.foo]
      ocamldep bin/.blah.eobjs/blah.ml.d [default.foo]
        ocamlc lib/.p.objs/byte/p.{cmi,cmo,cmt}
      ocamldep bin/.blah.eobjs/blah.ml.d
      ocamlopt lib/.p.objs/native/p.{cmt,cmx,o} [default.foo]
        ocamlc lib/p.cma [default.foo]
      ocamlopt lib/.p.objs/native/p.{cmt,cmx,o}
      ocamlopt lib/p.{a,cmxa} [default.foo]
      ocamlopt lib/p.{a,cmxa}
        ocamlc bin/.blah.eobjs/byte/blah.{cmi,cmo,cmt} [default.foo]
      ocamlopt lib/p.cmxs [default.foo]
        ocamlc bin/.blah.eobjs/byte/blah.{cmi,cmo,cmt}
      ocamlopt bin/.blah.eobjs/native/blah.{cmt,cmx,o} [default.foo]
      ocamlopt bin/.blah.eobjs/native/blah.{cmt,cmx,o}
      ocamlopt bin/blah.exe [default.foo]
      ocamlopt bin/blah.exe
          blah file [default.foo]
          blah file
  $ cat _build/default.foo/file
  42
  $ ls *.install
  p-foo.install
  $ dune_cmd cat p-foo.install | dune_cmd sanitize
  lib: [
    "_build/install/default.foo/lib/p/META" {"../../foo-sysroot/lib/p/META"}
    "_build/install/default.foo/lib/p/dune-package" {"../../foo-sysroot/lib/p/dune-package"}
    "_build/install/default.foo/lib/p/opam" {"../../foo-sysroot/lib/p/opam"}
    "_build/install/default.foo/lib/p/p$ext_lib" {"../../foo-sysroot/lib/p/p$ext_lib"}
    "_build/install/default.foo/lib/p/p.cma" {"../../foo-sysroot/lib/p/p.cma"}
    "_build/install/default.foo/lib/p/p.cmi" {"../../foo-sysroot/lib/p/p.cmi"}
    "_build/install/default.foo/lib/p/p.cmt" {"../../foo-sysroot/lib/p/p.cmt"}
    "_build/install/default.foo/lib/p/p.cmx" {"../../foo-sysroot/lib/p/p.cmx"}
    "_build/install/default.foo/lib/p/p.cmxa" {"../../foo-sysroot/lib/p/p.cmxa"}
    "_build/install/default.foo/lib/p/p.cmxs" {"../../foo-sysroot/lib/p/p.cmxs"}
    "_build/install/default.foo/lib/p/p.ml" {"../../foo-sysroot/lib/p/p.ml"}
  ]
  bin: [
    "_build/install/default.foo/bin/blah" {"../foo-sysroot/bin/blah"}
  ]
