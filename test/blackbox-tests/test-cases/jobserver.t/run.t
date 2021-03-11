  $ echo "(lang dune 2.8)" > dune-project
  $ cat >dune <<EOF
  > (rule (with-stdout-to foo (echo bar)))
  > EOF
  $ make -j2
  MAKEFLAGS= --jobserver-fds=3,4 -j
  dune build ./foo
  $ cat _build/default/foo
  bar
