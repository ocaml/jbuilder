This test checks that dune can gracefully handle situation when user provides
ordinary executable instead of one supporting dynamic dependencies.

  $ cp ../bin/foo.exe ./
  $ dune runtest --display short
           foo alias runtest
  Hello from foo!
  File "dune", line 1, characters 0-57:
  1 | (alias
  2 |  (name runtest)
  3 |  (action (dynamic-run ./foo.exe)))
  Error: Executable 'foo.exe' that was declared to support dynamic dependency
  discovery (declared by using 'dynamic-run' tag) failed to respond to dune.
  
  If you don't use dynamic dependency discovery in your executable you may
  consider changing 'dynamic-run' to 'run' in your rule definition.
  [1]

