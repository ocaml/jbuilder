  $ cat >dune-project <<EOF
  > (lang dune 2.9)
  > EOF

  $ cat >dune <<EOF
  > (rule
  >  (alias runtest)
  >  (action (diff hello.expected hello.output)))
  > EOF

  $ echo 'foo' > hello.expected 
  $ echo 'foo' > hello.output 

  $ dune runtest

  $ cat >dune-project <<EOF
  > (lang dune 3.0)
  > EOF

FIXME, result should be the same as with lang 2.8
  $ dune runtest
  File "run.t", line 1, characters 0-0:
  Error: Files _build/default/run.t and _build/default/run.t.corrected differ.
  [1]

FIXME enabling the following test causes infinite hang on my machine (Ulysse)
$ dune runtest
