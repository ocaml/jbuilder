If a generated directory is "overlaid" by a source dir, then things break.

  $ mkdir .cinaps

  $ cat > dune-project <<EOF
  > (lang dune 2.2)
  > (using cinaps 1.0)
  > EOF

  $ cat > dune <<EOF
  > (cinaps (files *.ml))
  > (dirs .cinaps)
  > EOF

  $ dune build @cinaps
  File "dune", line 1, characters 0-21:
  1 | (cinaps (files *.ml))
      ^^^^^^^^^^^^^^^^^^^^^
  Error: No rule found for .cinaps/cinaps.exe
  In fact, there are no rules defined in this directory
  [1]
