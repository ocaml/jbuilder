  $ dune exec ./qnativerun/run.exe --display short
      ocamldep q/.q.objs/q.mli.d
        ocamlc q/.q.objs/byte/q.{cmi,cmti}
      ocamldep q/.q.objs/q.ml.d
      ocamlopt q/.q.objs/native/q.{cmx,o}
      ocamlopt q/.q.objs/lib.{a,cmxa}
        ocamlc q/q_stub$ext_obj
    ocamlmklib q/dllq_stubs$ext_dll,q/libq_stubs$ext_lib
      ocamldep qnativerun/.run.eobjs/run.ml.d
        ocamlc qnativerun/.run.eobjs/byte/run.{cmi,cmo,cmt}
      ocamlopt qnativerun/.run.eobjs/native/run.{cmx,o}
      ocamlopt qnativerun/run.exe
  42
#  $ dune exec ./qbyterun/run.bc --display short
