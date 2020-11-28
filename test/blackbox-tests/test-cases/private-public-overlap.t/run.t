public libraries may not have private dependencies

  $ dune build --root private-dep
  Entering directory 'private-dep'
  File "dune", line 8, characters 12-22:
  8 |  (libraries privatelib)
                  ^^^^^^^^^^
  Error: Library "privatelib" is private, it cannot be a dependency of a public
  library. You need to give "privatelib" a public name.
  [1]

On the other hand, public libraries may have private preprocessors
  $ dune build --display short --root private-rewriter
  Entering directory 'private-rewriter'
        ocamlc .ppx_internal.objs/byte/ppx_internal.{cmi,cmo,cmt}
      ocamlopt .ppx_internal.objs/native/ppx_internal.{cmt,cmx,o}
      ocamlopt ppx_internal.{a,cmxa}
        ocamlc .ppx/be26d3600214af2fa78c2c9ef25e9069/dune__exe___ppx.{cmi,cmo}
      ocamlopt .ppx/be26d3600214af2fa78c2c9ef25e9069/dune__exe___ppx.{cmx,o}
      ocamlopt .ppx/be26d3600214af2fa78c2c9ef25e9069/ppx.exe
           ppx mylib.pp.ml
        ocamlc .mylib.objs/byte/mylib.{cmi,cmo,cmt}
      ocamlopt .mylib.objs/native/mylib.{cmt,cmx,o}
        ocamlc mylib.cma
      ocamlopt mylib.{a,cmxa}
      ocamlopt mylib.cmxs

Unless they introduce private runtime dependencies:
  $ dune build --root private-runtime-deps
  Entering directory 'private-runtime-deps'
  File "dune", line 16, characters 7-18:
  16 |   (pps private_ppx))
              ^^^^^^^^^^^
  Error: Library "private_runtime_dep" is private, it cannot be a dependency of
  a public library. You need to give "private_runtime_dep" a public name.
  [1]

However, public binaries may accept private dependencies
  $ dune exec --root exes ./publicbin.exe
  Entering directory 'exes'
  Entering directory 'exes'

Private dependencies shouldn't make the library optional
  $ dune build @install --display short --root optional
  Entering directory 'optional'
