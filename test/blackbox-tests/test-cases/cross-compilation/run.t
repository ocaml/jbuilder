  $ env OCAMLFIND_CONF=$PWD/etc/findlib.conf dune build --display short -x foo file @install --promote-install-files
      ocamldep lib/.p.objs/p.ml.d [default.foo]
        ocamlc lib/.p.objs/byte/p.{cmi,cmo,cmt} [default.foo]
        ocamlc lib/.p.objs/lib.cma [default.foo]
      ocamldep bin/.blah.eobjs/blah.ml.d [default.foo]
      ocamldep lib/.p.objs/p.ml.d
        ocamlc lib/.p.objs/byte/p.{cmi,cmo,cmt}
      ocamlopt lib/.p.objs/native/p.{cmx,o}
      ocamlopt lib/.p.objs/lib.{a,cmxa}
      ocamldep bin/.blah.eobjs/blah.ml.d
        ocamlc bin/.blah.eobjs/byte/blah.{cmi,cmo,cmt}
      ocamlopt bin/.blah.eobjs/native/blah.{cmx,o}
      ocamlopt bin/blah.exe
          blah file
      ocamlopt lib/.p.objs/native/p.{cmx,o} [default.foo]
      ocamlopt lib/.p.objs/lib.{a,cmxa} [default.foo]
      ocamlopt lib/.p.objs/lib.cmxs [default.foo]
          blah file [default.foo]
        ocamlc bin/.blah.eobjs/byte/blah.{cmi,cmo,cmt} [default.foo]
      ocamlopt bin/.blah.eobjs/native/blah.{cmx,o} [default.foo]
      ocamlopt bin/blah.exe [default.foo]
  $ cat _build/default.foo/file
  42
  $ ls *.install
  p-foo.install
  $ cat p-foo.install
  lib: [
    "_build/install/default.foo/lib/p/META" {"../../foo-sysroot/lib/p/META"}
    "_build/install/default.foo/lib/p/dune-package" {"../../foo-sysroot/lib/p/dune-package"}
    "_build/install/default.foo/lib/p/lib$ext_lib" {"../../foo-sysroot/lib/p/lib$ext_lib"}
    "_build/install/default.foo/lib/p/lib.cma" {"../../foo-sysroot/lib/p/lib.cma"}
    "_build/install/default.foo/lib/p/lib.cmxa" {"../../foo-sysroot/lib/p/lib.cmxa"}
    "_build/install/default.foo/lib/p/lib.cmxs" {"../../foo-sysroot/lib/p/lib.cmxs"}
    "_build/install/default.foo/lib/p/opam" {"../../foo-sysroot/lib/p/opam"}
    "_build/install/default.foo/lib/p/p.cmi" {"../../foo-sysroot/lib/p/p.cmi"}
    "_build/install/default.foo/lib/p/p.cmt" {"../../foo-sysroot/lib/p/p.cmt"}
    "_build/install/default.foo/lib/p/p.cmx" {"../../foo-sysroot/lib/p/p.cmx"}
    "_build/install/default.foo/lib/p/p.ml" {"../../foo-sysroot/lib/p/p.ml"}
  ]
  bin: [
    "_build/install/default.foo/bin/blah" {"../foo-sysroot/bin/blah"}
  ]
