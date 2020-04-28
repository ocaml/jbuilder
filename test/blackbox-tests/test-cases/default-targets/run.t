Generates targets when modes is set for binaries:
  $ dune build --root bins --display short @all 2>&1 | grep '\.bc\|\.exe'
        ocamlc byteandnative.bc
        ocamlc bytecodeonly.bc
      ocamlopt nativeonly.exe
      ocamlopt byteandnative.exe
        ocamlc bytecodeonly.exe

Generate targets when modes are set for libraries

  $ dune build --root libs --display short @all 2>&1 | grep 'cma\|cmxa\|cmxs'
  [1]
