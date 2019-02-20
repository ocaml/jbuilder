  $ env ORIG_PATH="$PATH" PATH="$PWD/ocaml-bin:$PATH" dune build --display short
      ocamldep bin/.toto.eobjs/toto.ml.d
        ocamlc bin/.toto.eobjs/byte/toto.{cmi,cmo,cmt}
      ocamlopt bin/.toto.eobjs/native/toto.{cmx,o}
      ocamlopt bin/toto.exe
      ocamldep src/.foo.objs/foo.ml.d
        ocamlc src/.foo.objs/byte/foo.{cmi,cmo,cmt}
        ocamlc src/foo.cma
      ocamlopt src/.foo.objs/native/foo.{cmx,o}
      ocamlopt src/foo.{a,cmxa}
      ocamlopt src/foo.cmxs

Check that building a native only executable fails
  $ env ORIG_PATH="$PATH" PATH="$PWD/ocaml-bin:$PATH" dune build --display short native-only/foo.exe
      ocamldep native-only/.foo.eobjs/foo.ml.d
        ocamlc native-only/.foo.eobjs/byte/foo.{cmi,cmo,cmt}
      ocamlopt native-only/.foo.eobjs/native/foo.{cmx,o}
      ocamlopt native-only/foo.exe
