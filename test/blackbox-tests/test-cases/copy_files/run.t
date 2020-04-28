  $ dune build --root test1 test.exe .merlin --display short --debug-dependency-path
  Entering directory 'test1'
      ocamldep .foo.objs/dummy.ml.d
        ocamlc .foo.objs/byte/dummy.{cmi,cmo,cmt}
      ocamlopt .foo.objs/native/dummy.{cmx,o}
      ocamlopt .foo.objs/lib.{a,cmxa}
        ocamlc bar$ext_obj
    ocamlmklib dllfoo_stubs$ext_dll,libfoo_stubs$ext_lib
      ocamllex lexers/lexer1.ml
      ocamldep .test.eobjs/lexer1.ml.d
      ocamldep .test.eobjs/test.ml.d
        ocamlc .test.eobjs/byte/lexer1.{cmi,cmo,cmt}
        ocamlc .test.eobjs/byte/test.{cmi,cmo,cmt}
      ocamlopt .test.eobjs/native/test.{cmx,o}
      ocamlopt .test.eobjs/native/lexer1.{cmx,o}
      ocamlopt test.exe
  $ dune build --root test1 @bar-source --display short
  Entering directory 'test1'
  #line 1 "include/bar.h"
  int foo () {return 42;}
  $ dune build --root test2 @foo/cat
  Entering directory 'test2'
  # 1 "dummy.txt"
  hello
