  $ dune build .foo.objs/lib.cma --profile release
  $ cat .merlin
  EXCLUDE_QUERY_DIR
  B _build/default/.foo.objs/byte
  S .
  FLG -open Foo -w -40
  $ rm -f .merlin
  $ dune build .foo.objs/lib.cma --profile release
  $ cat .merlin
  EXCLUDE_QUERY_DIR
  B _build/default/.foo.objs/byte
  S .
  FLG -open Foo -w -40
  $ echo toto > .merlin
  $ dune build .foo.objs/lib.cma --profile release
  $ cat .merlin
  EXCLUDE_QUERY_DIR
  B _build/default/.foo.objs/byte
  S .
  FLG -open Foo -w -40
