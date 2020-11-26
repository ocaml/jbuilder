This test checks that executable that uses 'dynamic-run'
and requires dependency that can not be build fails.

  $ cat > dune-project << EOF
  > (lang dune 2.0)
  > (using action-plugin 0.1)
  > EOF

  $ cat > dune << EOF
  > (rule
  >  (alias runtest)
  >  (action (dynamic-run ./foo.exe)))
  > EOF

  $ cp ./bin/foo.exe ./

  $ dune runtest
  File "dune", line 1, characters 0-57:
  1 | (rule
  2 |  (alias runtest)
  3 |  (action (dynamic-run ./foo.exe)))
  Error: No rule found for some_absent_dependency
  May I interest you in one of the following targets instead?
  - default/dune
  - default/dune-project
  - default/foo.exe
  - default/run.t
  [1]
