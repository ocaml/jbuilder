  $ env OCAMLFIND_CONF=$PWD/etc/findlib.conf dune build --display short -x foo file @install --promote-install-files
      ocamldep lib/.p.objs/p.ml.d [default.foo]
        ocamlc lib/.p.objs/byte/p.{cmi,cmo,cmt} [default.foo]
        ocamlc lib/p.cma [default.foo]
      ocamldep bin/.blah.eobjs/blah.ml.d [default.foo]
      ocamldep lib/.p.objs/p.ml.d
        ocamlc lib/.p.objs/byte/p.{cmi,cmo,cmt}
      ocamlopt lib/.p.objs/native/p.{cmx,o}
      ocamlopt lib/p.{a,cmxa}
      ocamldep bin/.blah.eobjs/blah.ml.d
        ocamlc bin/.blah.eobjs/byte/blah.{cmi,cmo,cmt}
      ocamlopt bin/.blah.eobjs/native/blah.{cmx,o}
      ocamlopt bin/blah.exe
          blah file
      ocamlopt lib/.p.objs/native/p.{cmx,o} [default.foo]
      ocamlopt lib/p.{a,cmxa} [default.foo]
      ocamlopt lib/p.cmxs [default.foo]
          blah file [default.foo]
        ocamlc bin/.blah.eobjs/byte/blah.{cmi,cmo,cmt} [default.foo]
      ocamlopt bin/.blah.eobjs/native/blah.{cmx,o} [default.foo]
      ocamlopt bin/blah.exe [default.foo]
  $ cat _build/default.foo/file
  42
  $ ls *.install
  p-foo.install
  $ dune_cmd cat p-foo.install
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
  $ env OCAMLFIND_CONF=$PWD/etc/findlib.conf dune install --prefix=_prefix --dry-run -x foo
  Installing _prefix/foo-sysroot/lib/p/META
  Installing _prefix/foo-sysroot/lib/p/dune-package
  Installing _prefix/foo-sysroot/lib/p/opam
  Installing _prefix/foo-sysroot/lib/p/p$ext_lib
  Installing _prefix/foo-sysroot/lib/p/p.cma
  Installing _prefix/foo-sysroot/lib/p/p.cmi
  Installing _prefix/foo-sysroot/lib/p/p.cmt
  Installing _prefix/foo-sysroot/lib/p/p.cmx
  Installing _prefix/foo-sysroot/lib/p/p.cmxa
  Installing _prefix/foo-sysroot/lib/p/p.cmxs
  Installing _prefix/foo-sysroot/lib/p/p.ml
  Installing _prefix/foo-sysroot/bin/blah
  Removing (if it exists) _prefix/foo-sysroot/lib/p/META
  Creating directory _prefix/foo-sysroot/lib/p
  Copying _build/install/default.foo/lib/p/META to _prefix/foo-sysroot/lib/p/META (executable: false)
  Removing (if it exists) _prefix/foo-sysroot/lib/p/dune-package
  Creating directory _prefix/foo-sysroot/lib/p
  Copying _build/install/default.foo/lib/p/dune-package to _prefix/foo-sysroot/lib/p/dune-package (executable: false)
  Removing (if it exists) _prefix/foo-sysroot/lib/p/opam
  Creating directory _prefix/foo-sysroot/lib/p
  Copying _build/install/default.foo/lib/p/opam to _prefix/foo-sysroot/lib/p/opam (executable: false)
  Removing (if it exists) _prefix/foo-sysroot/lib/p/p$ext_lib
  Creating directory _prefix/foo-sysroot/lib/p
  Copying _build/install/default.foo/lib/p/p$ext_lib to _prefix/foo-sysroot/lib/p/p$ext_lib (executable: false)
  Removing (if it exists) _prefix/foo-sysroot/lib/p/p.cma
  Creating directory _prefix/foo-sysroot/lib/p
  Copying _build/install/default.foo/lib/p/p.cma to _prefix/foo-sysroot/lib/p/p.cma (executable: false)
  Removing (if it exists) _prefix/foo-sysroot/lib/p/p.cmi
  Creating directory _prefix/foo-sysroot/lib/p
  Copying _build/install/default.foo/lib/p/p.cmi to _prefix/foo-sysroot/lib/p/p.cmi (executable: false)
  Removing (if it exists) _prefix/foo-sysroot/lib/p/p.cmt
  Creating directory _prefix/foo-sysroot/lib/p
  Copying _build/install/default.foo/lib/p/p.cmt to _prefix/foo-sysroot/lib/p/p.cmt (executable: false)
  Removing (if it exists) _prefix/foo-sysroot/lib/p/p.cmx
  Creating directory _prefix/foo-sysroot/lib/p
  Copying _build/install/default.foo/lib/p/p.cmx to _prefix/foo-sysroot/lib/p/p.cmx (executable: false)
  Removing (if it exists) _prefix/foo-sysroot/lib/p/p.cmxa
  Creating directory _prefix/foo-sysroot/lib/p
  Copying _build/install/default.foo/lib/p/p.cmxa to _prefix/foo-sysroot/lib/p/p.cmxa (executable: false)
  Removing (if it exists) _prefix/foo-sysroot/lib/p/p.cmxs
  Creating directory _prefix/foo-sysroot/lib/p
  Copying _build/install/default.foo/lib/p/p.cmxs to _prefix/foo-sysroot/lib/p/p.cmxs (executable: false)
  Removing (if it exists) _prefix/foo-sysroot/lib/p/p.ml
  Creating directory _prefix/foo-sysroot/lib/p
  Copying _build/install/default.foo/lib/p/p.ml to _prefix/foo-sysroot/lib/p/p.ml (executable: false)
  Removing (if it exists) _prefix/foo-sysroot/bin/blah
  Creating directory _prefix/foo-sysroot/bin
  Copying _build/install/default.foo/bin/blah to _prefix/foo-sysroot/bin/blah (executable: true)
