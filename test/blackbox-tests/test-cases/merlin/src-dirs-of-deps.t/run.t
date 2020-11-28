We create two libraries where one depends on the the other. The dependency
library also has more than one src dir.

  $ echo "(lang dune 2.0)" > dune-project
  $ mkdir -p lib1/sub
  $ cat >lib1/dune <<EOF
  > (include_subdirs unqualified)
  > (library (name lib1))
  > EOF
  $ touch lib1/sub/foo.ml
  $ touch lib1/bar.ml
  $ mkdir lib2
  $ cat >lib2/dune <<EOF
  > (library (name lib2) (libraries lib1) (modules ()))
  > EOF
  $ dune build lib2/.merlin
  $ cat lib2/.merlin
  EXCLUDE_QUERY_DIR
  B ../_build/default/lib1/.lib1.objs/byte
  B ../_build/default/lib1/.lib1.objs/native
  B ../_build/default/lib2/.lib2.objs/byte
  B ../_build/default/lib2/.lib2.objs/native
  S ../lib1
  S ../lib1/sub
  S .
  FLG -open Lib2 -w @1..3@5..28@30..39@43@46..47@49..57@61..62-40 -strict-sequence -strict-formats -short-paths -keep-locs
