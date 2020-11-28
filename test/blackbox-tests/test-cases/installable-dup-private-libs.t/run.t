  $ dune build @install --display short
        ocamlc a1/.a.objs/byte/a.{cmi,cmo,cmt}
        ocamlc a2/.a.objs/byte/a.{cmi,cmo,cmt}
      ocamlopt a1/.a.objs/native/a.{cmt,cmx,o}
        ocamlc a1/a.cma
      ocamlopt a2/.a.objs/native/a.{cmt,cmx,o}
        ocamlc a2/a.cma
      ocamlopt a1/a.{a,cmxa}
      ocamlopt a2/a.{a,cmxa}
      ocamlopt a1/a.cmxs
      ocamlopt a2/a.cmxs
