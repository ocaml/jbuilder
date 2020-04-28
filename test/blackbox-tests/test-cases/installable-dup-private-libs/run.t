  $ dune build @install --display short
      ocamldep a1/.a.objs/a.ml.d
        ocamlc a1/.a.objs/byte/a.{cmi,cmo,cmt}
        ocamlc a1/.a.objs/lib.cma
      ocamldep a2/.a.objs/a.ml.d
        ocamlc a2/.a.objs/byte/a.{cmi,cmo,cmt}
        ocamlc a2/.a.objs/lib.cma
      ocamlopt a1/.a.objs/native/a.{cmx,o}
      ocamlopt a1/.a.objs/lib.{a,cmxa}
      ocamlopt a1/.a.objs/lib.cmxs
      ocamlopt a2/.a.objs/native/a.{cmx,o}
      ocamlopt a2/.a.objs/lib.{a,cmxa}
      ocamlopt a2/.a.objs/lib.cmxs
