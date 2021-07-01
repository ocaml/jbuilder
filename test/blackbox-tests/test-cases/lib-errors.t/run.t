Test various errors related to resolving libraries

Cycle detection
---------------

  $ dune build cycle.exe

Select with no solution
-----------------------

  $ dune build select_error.exe
  File "dune", line 25, characters 12-27:
  25 |  (libraries (select x from))
                   ^^^^^^^^^^^^^^^
  Error: No solution found for this select form.
  [1]

