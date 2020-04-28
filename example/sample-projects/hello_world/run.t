  $ dune build @install @runtest --display short
      ocamldep lib/.hello_world.objs/hello_world.ml.d
        ocamlc lib/.hello_world.objs/byte/hello_world.{cmi,cmo,cmt}
        ocamlc lib/.hello_world.objs/lib.cma
      ocamldep bin/.main.eobjs/main.ml.d
      ocamldep test/.test.eobjs/test.ml.d
      ocamlopt lib/.hello_world.objs/native/hello_world.{cmx,o}
      ocamlopt lib/.hello_world.objs/lib.{a,cmxa}
      ocamlopt lib/.hello_world.objs/lib.cmxs
        ocamlc test/.test.eobjs/byte/test.{cmi,cmo,cmt}
      ocamlopt test/.test.eobjs/native/test.{cmx,o}
      ocamlopt test/test.exe
          test test/test.output
        ocamlc bin/.main.eobjs/byte/main.{cmi,cmo,cmt}
      ocamlopt bin/.main.eobjs/native/main.{cmx,o}
      ocamlopt bin/main.exe
