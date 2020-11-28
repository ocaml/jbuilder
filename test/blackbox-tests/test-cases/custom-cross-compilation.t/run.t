  $ dune build --root ./normal --display short file @install
  Entering directory 'normal'
      ocamldep .p.eobjs/p.ml.d [cross]
      ocamldep .p.eobjs/p.ml.d
        ocamlc .p.eobjs/byte/p.{cmi,cmo,cmt} [cross]
        ocamlc .p.eobjs/byte/p.{cmi,cmo,cmt}
      ocamlopt .p.eobjs/native/p.{cmt,cmx,o} [cross]
      ocamlopt .p.eobjs/native/p.{cmt,cmx,o}
      ocamlopt p.exe [cross]
      ocamlopt p.exe
             p file [cross]
             p file

  $ cat normal/_build/cross/file
  137

  $ dune build --root ./bad-configuration --display short file @install
  Entering directory 'bad-configuration'
  File "dune-workspace", line 5, characters 9-50:
  5 | (context (default
  6 |  (name cross-1)
  7 |  (host default)))
  Error: Context 'cross-1' is both a host (for 'cross-2') and a target (for
  'default').
  [1]

  $ dune build --root ./topological-loop --display short file @install
  Entering directory 'topological-loop'
  File "dune-workspace", line 13, characters 9-50:
  13 | (context (default
  14 |  (name cross-3)
  15 |  (host cross-2)))
  Error: Context 'cross-3' is both a host (for 'cross-1') and a target (for
  'cross-2').
  [1]

  $ env OCAMLFIND_CONF=$PWD/target-and-host/etc/findlib.conf dune build --root ./target-and-host --display short file @install
  Entering directory 'target-and-host'
  File "dune-workspace", line 5, characters 9-65:
  5 | (context (default
  6 |  (name cross-1)
  7 |  (targets foo)
  8 |  (host default)))
  Error: `targets` and `host` options cannot be used in the same context.
  [1]


  $ dune build --root ./context-not-found --display short file @install
  Entering directory 'context-not-found'
  File "dune-workspace", line 5, characters 9-47:
  5 | (context (default
  6 |  (name cross-1)
  7 |  (host oups)))
  Error: Undefined host context 'oups' for 'cross-1'.
  [1]
