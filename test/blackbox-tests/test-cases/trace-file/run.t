  $ dune build prog.exe --trace-file trace.json

This captures the commands that are being run:

  $ <trace.json grep '"[be]"' | cut -c 2- | sed -E 's/ [0-9]+/ .../g'
  {"cat": "process", "name": "ocamlc.opt", "id": ..., "pid": ..., "ph": "b", "ts": ..., "args": ["-config"]}
  {"cat": "process", "name": "ocamlc.opt", "id": ..., "pid": ..., "ph": "e", "ts": ...}
  {"cat": "process", "name": "ocamldep.opt", "id": ..., "pid": ..., "ph": "b", "ts": ..., "args": ["-modules","-impl","prog.ml"]}
  {"cat": "process", "name": "ocamldep.opt", "id": ..., "pid": ..., "ph": "e", "ts": ...}
  {"cat": "process", "name": "ocamlc.opt", "id": ..., "pid": ..., "ph": "b", "ts": ..., "args": ["-w","@1..3@5..28@30..39@43@46..47@49..57@61..62-40","-strict-sequence","-strict-formats","-short-paths","-keep-locs","-g","-bin-annot","-I",".prog.eobjs/byte","-no-alias-deps","-opaque","-o",".prog.eobjs/byte/prog.cmo","-c","-impl","prog.ml"]}
  {"cat": "process", "name": "ocamlc.opt", "id": ..., "pid": ..., "ph": "e", "ts": ...}
  {"cat": "process", "name": "ocamlopt.opt", "id": ..., "pid": ..., "ph": "b", "ts": ..., "args": ["-w","@1..3@5..28@30..39@43@46..47@49..57@61..62-40","-strict-sequence","-strict-formats","-short-paths","-keep-locs","-g","-I",".prog.eobjs/byte","-I",".prog.eobjs/native","-intf-suffix",".ml","-no-alias-deps","-opaque","-o",".prog.eobjs/native/prog.cmx","-c","-impl","prog.ml"]}
  {"cat": "process", "name": "ocamlopt.opt", "id": ..., "pid": ..., "ph": "e", "ts": ...}
  {"cat": "process", "name": "ocamlopt.opt", "id": ..., "pid": ..., "ph": "b", "ts": ..., "args": ["-w","@1..3@5..28@30..39@43@46..47@49..57@61..62-40","-strict-sequence","-strict-formats","-short-paths","-keep-locs","-g","-noautoliblink","-o","prog.exe",".prog.eobjs/native/prog.cmx"]}
  {"cat": "process", "name": "ocamlopt.opt", "id": ..., "pid": ..., "ph": "e", "ts": ...}

As well as data about the garbage collector:

  $ <trace.json grep '"C"' | cut -c 2- | sed -E 's/ [0-9]+/ .../g' | sort -u
  {"name": "compactions", "pid": ..., "tid": ..., "ph": "C", "ts": ..., "args": {"value": ...}}
  {"name": "evaluated-rules", "pid": ..., "tid": ..., "ph": "C", "ts": ..., "args": {"value": ...}}
  {"name": "fds", "pid": ..., "tid": ..., "ph": "C", "ts": ..., "args": {"value": ...}}
  {"name": "free_words", "pid": ..., "tid": ..., "ph": "C", "ts": ..., "args": {"value": ...}}
  {"name": "heap_words", "pid": ..., "tid": ..., "ph": "C", "ts": ..., "args": {"value": ...}}
  {"name": "live_words", "pid": ..., "tid": ..., "ph": "C", "ts": ..., "args": {"value": ...}}
  {"name": "major_collections", "pid": ..., "tid": ..., "ph": "C", "ts": ..., "args": {"value": ...}}
  {"name": "major_words", "pid": ..., "tid": ..., "ph": "C", "ts": ..., "args": {"value": ....00}}
  {"name": "minor_collections", "pid": ..., "tid": ..., "ph": "C", "ts": ..., "args": {"value": ...}}
  {"name": "minor_words", "pid": ..., "tid": ..., "ph": "C", "ts": ..., "args": {"value": ....00}}
  {"name": "promoted_words", "pid": ..., "tid": ..., "ph": "C", "ts": ..., "args": {"value": ....00}}
  {"name": "stack_size", "pid": ..., "tid": ..., "ph": "C", "ts": ..., "args": {"value": ...}}
  {"name": "top_heap_words", "pid": ..., "tid": ..., "ph": "C", "ts": ..., "args": {"value": ...}}
