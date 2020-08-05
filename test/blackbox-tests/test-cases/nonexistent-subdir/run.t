Check that promoting from a nonexistent subdir doesn't cause crashes.
  $ dune build x/y
  $ cat x/y
  z
