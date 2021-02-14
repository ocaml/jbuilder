Tests for promoting with non existent reference
-----------------------------------------------
# Diffing should do as if it is an empty file

  $ cat x-non-existent
  cat: x-non-existent: No such file or directory
  [1]

  $ dune build @blah-non-existent
  File "_build/default/x-non-existent", line 1, characters 0-0:
  Error:
  --- _build/default/x-non-existent
  +++ _build/default/x.gen
  +toto
  [1]

  $ dune promote
  Promoting _build/default/x.gen to x-non-existent.

  $ cat x-non-existent
  toto

  $ dune build @blah-non-existent-in-sub-dir
  File "_build/default/subdir/x-non-existent-in-sub-dir", line 1, characters 0-0:
  Error:
  --- _build/default/subdir/x-non-existent-in-sub-dir
  +++ _build/default/x.gen
  +toto
  [1]

  $ dune promote
  Promoting _build/default/x.gen to subdir/x-non-existent-in-sub-dir.

  $ cat subdir/x-non-existent-in-sub-dir
  toto
