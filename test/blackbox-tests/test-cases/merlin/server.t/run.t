  $ FILE=$PWD/main.ml
  $ dune ocaml-merlin  <<EOF
  > (4:File${#FILE}:$FILE)
  > EOF
  ()

  $ dune build @check
  $ dune ocaml-merlin  <<EOF
  > (4:File${#FILE}:$FILE)
  > EOF
  ((17:EXCLUDE_QUERY_DIR)(1:B31:_build/default/.main.eobjs/byte)(1:B31:_build/default/.mylib.objs/byte)(1:S1:.)(3:FLG223:-open Mylib -w @1..3@5..28@30..39@43@46..47@49..57@61..62-40 -strict-sequence -strict-formats -short-paths -keep-locs -w @1..3@5..28@30..39@43@46..47@49..57@61..62-40 -strict-sequence -strict-formats -short-paths -keep-locs))
