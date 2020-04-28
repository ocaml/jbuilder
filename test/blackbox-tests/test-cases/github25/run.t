This test define an installed "plop" with a "plop.ca-marche-pas"
sub-package which depend on a library that doesn't exist.

The build itself uses only "plop.ca-marche", which doesn't have this
problem. So dune shouldn't crash because of "plop.ca-marche-pas"

We need ocamlfind to run this test

  $ dune build @install --display short --only hello
        ocamlc root/.hello.objs/byte/hello.{cmi,cmo,cmt}
        ocamlc root/.hello.objs/lib.cma
      ocamlopt root/.hello.objs/native/hello.{cmx,o}
      ocamlopt root/.hello.objs/lib.{a,cmxa}
      ocamlopt root/.hello.objs/lib.cmxs

  $ dune build @install --display short --only pas-de-bol 2>&1 | sed 's/[^ "]*findlib-packages/.../'
        ocamlc root/.pas_de_bol.objs/byte/pas_de_bol.{cmi,cmo,cmt}
      ocamlopt root/.pas_de_bol.objs/native/pas_de_bol.{cmx,o}
      ocamldep root/.pas_de_bol.objs/a.ml.d
  File ".../plop/META", line 1, characters 0-0:
  Error: Library "une-lib-qui-nexiste-pas" not found.
  -> required by library "plop.ca-marche-pas" in
     .../plop
  Hint: try:
  dune external-lib-deps --missing --only pas-de-bol --display short @install
      ocamldep root/.pas_de_bol.objs/b.ml.d
