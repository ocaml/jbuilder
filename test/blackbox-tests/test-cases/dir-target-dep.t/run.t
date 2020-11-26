  $ dune build --root target && cat target/_build/default/dir/*
  Entering directory 'target'
  bar contents
  foo contents

  $ dune build --root dep
  Entering directory 'dep'
  File "dune", line 1, characters 0-68:
  1 | (alias
  2 |  (name default)
  3 |  (deps dir)
  4 |  (action (bash "cat %{deps}/*")))
  Error: No rule found for dir
  May I interest you in one of the following targets instead?
  - default/dune
  - default/dune-project
  [1]

We should not be able to produce a directory in a rule that already exists
  $ dune build --display=short --root no-overlapping-rules
  Entering directory 'no-overlapping-rules'
  File "dune", line 5, characters 0-51:
  5 | (rule
  6 |  (targets dir)
  7 |  (action (run ./foo.exe dir)))
  Error: Rule has a target default/dir
  This conflicts with a source directory in the same directory
  [1]

Dune crashes if there's a file named after the directory target
  $ dune build --root file-overlap-dir @all
  Entering directory 'file-overlap-dir'
  Error: Multiple rules generated for _build/default/dir:
  - file present in source tree
  - dune:1
  Hint: rm -f dir
  [1]

directory target and (mode promote) results in a crash
  $ dune build --root mode-promote @all 2>&1 | head -n2
  Entering directory 'mode-promote'
  Error: Is a directory
