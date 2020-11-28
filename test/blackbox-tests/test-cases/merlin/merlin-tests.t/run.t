  $ dune build @print-merlins --profile release
  sanitize_dot_merlin alias print-merlins
  # Processing exe/.merlin
  B $LIB_PREFIX/lib/bytes
  B $LIB_PREFIX/lib/findlib
  B $LIB_PREFIX/lib/ocaml
  B ../_build/default/exe/.x.eobjs/byte
  B ../_build/default/exe/.x.eobjs/native
  B ../_build/default/lib/.foo.objs/byte
  B ../_build/default/lib/.foo.objs/native
  B ../_build/default/lib/.foo.objs/public_cmi
  EXCLUDE_QUERY_DIR
  FLG -pp '$TESTCASE_ROOT/_build/default/pp/pp.exe'
  FLG -w -40
  S $LIB_PREFIX/lib/bytes
  S $LIB_PREFIX/lib/findlib
  S $LIB_PREFIX/lib/ocaml
  S .
  S ../lib
  # Processing lib/.merlin
  B $LIB_PREFIX/lib/bytes
  B $LIB_PREFIX/lib/findlib
  B $LIB_PREFIX/lib/ocaml
  B ../_build/default/lib/.bar.objs/byte
  B ../_build/default/lib/.bar.objs/native
  B ../_build/default/lib/.foo.objs/byte
  B ../_build/default/lib/.foo.objs/native
  EXCLUDE_QUERY_DIR
  FLG -open Foo -w -40 -open Bar -w -40
  FLG -ppx '$PPX/4128e43a9cfb141a37f547484cc9bf46/ppx.exe --as-ppx --cookie '\''library-name="foo"'\'''
  S $LIB_PREFIX/lib/bytes
  S $LIB_PREFIX/lib/findlib
  S $LIB_PREFIX/lib/ocaml
  S .
  S subdir
Make sure a ppx directive is generated
  $ grep -q ppx lib/.merlin
Make sure pp flag is correct and variables are expanded
  $ dune build @print-merlins-pp
  sanitize_dot_merlin alias print-merlins-pp
  # Processing pp-with-expand/.merlin
  B ../_build/default/pp-with-expand/.foobar.eobjs/byte
  B ../_build/default/pp-with-expand/.foobar.eobjs/native
  EXCLUDE_QUERY_DIR
  FLG -pp '$TESTCASE_ROOT/_build/default/pp/pp.exe -nothing'
  FLG -w @1..3@5..28@30..39@43@46..47@49..57@61..62-40 -strict-sequence -strict-formats -short-paths -keep-locs
  S .
We want future-syntax to either be applied, or not, depending on OCaml version.
Adding the `echo` with expected output to the set of lines is a way of achieving that.
  $ (echo "FLG -pp '\$BIN/ocaml-syntax-shims'"; dune build @print-merlins-future-syntax 2>&1) | sort | uniq
  # Processing future-syntax/.merlin
  B ../_build/default/future-syntax/.pp_future_syntax.eobjs/byte
  B ../_build/default/future-syntax/.pp_future_syntax.eobjs/native
  EXCLUDE_QUERY_DIR
  FLG -pp '$BIN/ocaml-syntax-shims'
  FLG -w @1..3@5..28@30..39@43@46..47@49..57@61..62-40 -strict-sequence -strict-formats -short-paths -keep-locs
  S .
  sanitize_dot_merlin alias print-merlins-future-syntax
