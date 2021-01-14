  $ dune build --display short --debug-dependency-path @all
        ocamlc lib/.ocaml_lib.objs/byte/ocaml_lib.{cmi,cmo,cmt}
      ocamldep lib/.ocaml_lib.objs/a.mli.d
      ocamldep lib/.ocaml_lib.objs/a.ml.d
      ocamlopt lib/.ocaml_lib.objs/native/ocaml_lib.{cmx,o}
        ocamlc lib/.ocaml_lib.objs/byte/ocaml_lib__A.{cmi,cmti}
      ocamlopt lib/.ocaml_lib.objs/native/ocaml_lib__A.{cmx,o}
        ocamlc lib/.ocaml_lib.objs/byte/ocaml_lib__A.{cmo,cmt}
        coqffi theories/a.v
      ocamlopt lib/ocaml_lib.{a,cmxa}
        ocamlc lib/ocaml_lib.cma
        coqdep theories/a.v.d
        coqdep theories/b.v.d
      ocamlopt lib/ocaml_lib.cmxs
          coqc theories/.a.aux,theories/a.{glob,vo}
          coqc theories/.b.aux,theories/b.{glob,vo}
