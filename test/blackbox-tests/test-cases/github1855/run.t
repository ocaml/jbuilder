Reproduction of https://github.com/ocaml/dune/issues/1895

An implementation and a virtual library both define the same (non virtual
) module. However, the module is private in the public library hence it should
just quietly shadow the module in the virtual library.

  $ cat > dune-project <<EOF
  > (lang dune 2.0)
  > EOF

  $ mkdir vlib impl
  $ touch impl/dom.ml vlib/dom.mli vlib/overlap.ml impl/overlap.ml
  $ cat > vlib/dune <<EOF
  > (library
  >  (name vlib1855)
  >  (virtual_modules dom))
  > EOF
  $ cat > impl/dune <<EOF
  > (library
  >  (name impl1855)
  >  (implements vlib1855)
  >  (private_modules overlap))
  > EOF

  $ dune build @all --display short
