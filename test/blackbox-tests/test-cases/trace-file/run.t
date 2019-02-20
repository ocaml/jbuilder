  $ dune build prog.exe --trace-file trace.json

This captures the commands that are being run:

  $ <trace.json grep '"[be]"' | cut -c 2- | sed -E 's/ [0-9]+/ .../g'
  {"cat": "process", "name": "ocamlfind", "id": ..., "pid": ..., "ph": "b", "ts": ..., "args": ["printconf","conf"]}
  {"cat": "process", "name": "ocamlfind", "id": ..., "pid": ..., "ph": "e", "ts": ...}
  {"cat": "process", "name": "ocamlc.opt", "id": ..., "pid": ..., "ph": "b", "ts": ..., "args": ["-config"]}
  {"cat": "process", "name": "ocamlc.opt", "id": ..., "pid": ..., "ph": "e", "ts": ...}
  {"cat": "process", "name": "ocamldep.opt", "id": ..., "pid": ..., "ph": "b", "ts": ..., "args": ["-modules","-impl","prog.ml"]}
  {"cat": "process", "name": "ocamldep.opt", "id": ..., "pid": ..., "ph": "e", "ts": ...}
  {"cat": "process", "name": "ocamlc.opt", "id": ..., "pid": ..., "ph": "b", "ts": ..., "args": ["-w","@a-4-29-40-41-42-44-45-48-58-59-60-40","-strict-sequence","-strict-formats","-short-paths","-keep-locs","-g","-bin-annot","-I",".prog.eobjs/byte","-no-alias-deps","-opaque","-o",".prog.eobjs/byte/prog.cmo","-c","-impl","prog.ml"]}
  {"cat": "process", "name": "ocamlc.opt", "id": ..., "pid": ..., "ph": "e", "ts": ...}
  {"cat": "process", "name": "ocamlopt.opt", "id": ..., "pid": ..., "ph": "b", "ts": ..., "args": ["-w","@a-4-29-40-41-42-44-45-48-58-59-60-40","-strict-sequence","-strict-formats","-short-paths","-keep-locs","-g","-I",".prog.eobjs/byte","-I",".prog.eobjs/native","-intf-suffix",".ml","-no-alias-deps","-opaque","-o",".prog.eobjs/native/prog.cmx","-c","-impl","prog.ml"]}
  {"cat": "process", "name": "ocamlopt.opt", "id": ..., "pid": ..., "ph": "e", "ts": ...}
  {"cat": "process", "name": "ocamlopt.opt", "id": ..., "pid": ..., "ph": "b", "ts": ..., "args": ["-w","@a-4-29-40-41-42-44-45-48-58-59-60-40","-strict-sequence","-strict-formats","-short-paths","-keep-locs","-g","-o","prog.exe",".prog.eobjs/native/prog.cmx"]}
  {"cat": "process", "name": "ocamlopt.opt", "id": ..., "pid": ..., "ph": "e", "ts": ...}

As well as data about the garbage collector:

  $ <trace.json grep '"C"' | cut -c 2- | sed -E 's/ [0-9]+/ .../g' | sort -u
  {"name": "fds", "pid": ..., "tid": ..., "ph": "C", "ts": ..., "args": {"value": ...}}
  {"name": "free_words", "pid": ..., "tid": ..., "ph": "C", "ts": ..., "args": {"value": ...}}
  {"name": "live_words", "pid": ..., "tid": ..., "ph": "C", "ts": ..., "args": {"value": ...}}
  {"name": "stack_size", "pid": ..., "tid": ..., "ph": "C", "ts": ..., "args": {"value": ...}}
