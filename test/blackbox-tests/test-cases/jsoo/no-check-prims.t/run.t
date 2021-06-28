Check that -no-check-prims is passed when `js` is the only linking mode
listed in the dune file

  $ dune build foo.bc

Otherwise, it is *not* passed, and we get the usual error.

  $ dune build bar.bc
  File "_none_", line 1:
  Error: Error while linking .bar.eobjs/byte/dune__exe__Bar.cmo:
         The external function `does_not_exist' is not available
  [1]
