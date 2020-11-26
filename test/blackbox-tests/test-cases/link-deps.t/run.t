It is possible to add link-time dependencies.

In particular, these can depend on the result of the compilation (like a .cmo
file) and be created just before linking.

  $ dune build link_deps.exe
  File "dune", line 1, characters 0-105:
  1 | (alias
  2 |   (name message)
  3 |   (deps .link_deps.eobjs/.byte_objs/link_deps.cmo)
  4 |   (action (echo "link\n"))
  5 |   )
  Error: No rule found for .link_deps.eobjs/.byte_objs/link_deps.cmo
  In fact, there are no rules defined in this directory
  [1]
