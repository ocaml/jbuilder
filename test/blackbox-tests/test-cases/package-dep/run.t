  $ dune runtest --display short
      ocamldep .foo.objs/foo.ml.d
        ocamlc .foo.objs/byte/foo.{cmi,cmo,cmt}
      ocamldep .bar.objs/bar.ml.d
        ocamlc .bar.objs/byte/bar.{cmi,cmo,cmt}
        ocamlc .bar.objs/lib.cma
      ocamlopt .bar.objs/native/bar.{cmx,o}
      ocamlopt .bar.objs/lib.{a,cmxa}
      ocamlopt .bar.objs/lib.cmxs
      ocamlopt .foo.objs/native/foo.{cmx,o}
      ocamlopt .foo.objs/lib.{a,cmxa}
      ocamlopt .foo.objs/lib.cmxs
        ocamlc .foo.objs/lib.cma
     ocamlfind test.exe
          test alias runtest
  42 42
