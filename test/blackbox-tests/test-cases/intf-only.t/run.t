Successes:

  $ dune build --display short --root foo --debug-dep
  Entering directory 'foo'
      ocamldep .foo.objs/foo.ml.d
        ocamlc .foo.objs/byte/foo__.{cmi,cmo,cmt}
      ocamldep .foo.objs/intf.mli.d
      ocamlopt .foo.objs/native/foo__.{cmt,cmx,o}
        ocamlc .foo.objs/byte/foo__Intf.{cmi,cmti}
        ocamlc .foo.objs/byte/foo.{cmi,cmo,cmt}
        ocamlc test/.bar.objs/byte/bar.{cmi,cmo,cmt}
      ocamlopt .foo.objs/native/foo.{cmt,cmx,o}
        ocamlc foo.cma
      ocamlopt test/.bar.objs/native/bar.{cmt,cmx,o}
        ocamlc test/bar.cma
      ocamlopt foo.{a,cmxa}
      ocamlopt test/bar.{a,cmxa}
      ocamlopt foo.cmxs
      ocamlopt test/bar.cmxs

Errors:

  $ dune build --root a foo.cma
  Entering directory 'a'
  File "dune", line 1, characters 0-21:
  1 | (library
  2 |  (name foo))
  Error: Some modules don't have an implementation.
  You need to add the following field to this stanza:
  
    (modules_without_implementation x y)
  [1]
  $ dune build --root b foo.cma
  Entering directory 'b'
  File "dune", line 3, characters 33-34:
  3 |  (modules_without_implementation x))
                                       ^
  Error: The following modules must be listed here as they don't have an
  implementation:
  - Y
  [1]
  $ dune build --root c foo.cma
  Entering directory 'c'
  File "dune", line 3, characters 33-34:
  3 |  (modules_without_implementation x))
                                       ^
  Error: Module X doesn't exist.
  [1]
  $ dune build --root d foo.cma
  Entering directory 'd'
  File "dune", line 3, characters 33-34:
  3 |  (modules_without_implementation x))
                                       ^
  Error: The following modules have an implementation, they cannot be listed as
  modules_without_implementation:
  - X
  [1]
