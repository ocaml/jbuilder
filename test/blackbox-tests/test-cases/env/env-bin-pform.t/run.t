%{exe:foo.exe} should not be veisible if foo.exe is added to PATH via the
binaries stanza. %{bin:foo} is visible on the other hand.
  $ dune build
  File "dune", line 5, characters 0-54:
  5 | (alias
  6 |  (name default)
  7 |  (action (run %{exe:foo.exe})))
  Error: No rule found for foo.exe
  May I interest you in one of the following targets instead?
  - default/dune
  - default/dune-project
  - default/run.t
           foo alias default
  this is foo.exe
  [1]
