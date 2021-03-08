  $ dune build --display short --debug-dependency-path @all
        coqdep thy1/a.v.d
        ocamlc src_b/.ml_plugin_b.objs/byte/ml_plugin_b.{cmi,cmo,cmt}
      ocamldep src_b/.ml_plugin_b.objs/simple_b.ml.d
        ocamlc src_a/.ml_plugin_a.objs/byte/ml_plugin_a.{cmi,cmo,cmt}
      ocamldep src_a/.ml_plugin_a.objs/gram.mli.d
      ocamldep src_a/.ml_plugin_a.objs/simple.ml.d
        coqdep thy2/a.v.d
         coqpp src_a/gram.ml
      ocamlopt src_b/.ml_plugin_b.objs/native/ml_plugin_b.{cmx,o}
      ocamlopt src_a/.ml_plugin_a.objs/native/ml_plugin_a.{cmx,o}
        ocamlc src_a/.ml_plugin_a.objs/byte/ml_plugin_a__Gram.{cmi,cmti}
        ocamlc src_a/.ml_plugin_a.objs/byte/ml_plugin_a__Simple.{cmi,cmo,cmt}
      ocamldep src_a/.ml_plugin_a.objs/gram.ml.d
        ocamlc src_b/.ml_plugin_b.objs/byte/ml_plugin_b__Simple_b.{cmi,cmo,cmt}
      ocamlopt src_a/.ml_plugin_a.objs/native/ml_plugin_a__Simple.{cmx,o}
      ocamlopt src_a/.ml_plugin_a.objs/native/ml_plugin_a__Gram.{cmx,o}
        ocamlc src_a/.ml_plugin_a.objs/byte/ml_plugin_a__Gram.{cmo,cmt}
      ocamlopt src_b/.ml_plugin_b.objs/native/ml_plugin_b__Simple_b.{cmx,o}
        ocamlc src_b/ml_plugin_b.cma
      ocamlopt src_a/ml_plugin_a.{a,cmxa}
        ocamlc src_a/ml_plugin_a.cma
      ocamlopt src_b/ml_plugin_b.{a,cmxa}
      ocamlopt src_a/ml_plugin_a.cmxs
      ocamlopt src_b/ml_plugin_b.cmxs
          coqc thy1/.a.aux,thy1/a.{glob,vo}
          coqc thy1/a.vos (exit 1)
  (cd _build/default && /Users/rgrinberg/github/ocaml/dune/_opam/bin/coqc -q -w -native-compiler-disabled -native-compiler ondemand -I /Users/rgrinberg/github/ocaml/dune/_opam/lib/coq/clib -I /Users/rgrinberg/github/ocaml/dune/_opam/lib/coq/config -I /Users/rgrinberg/github/ocaml/dune/_opam/lib/coq/engine -I /Users/rgrinberg/github/ocaml/dune/_opam/lib/coq/gramlib/.pack -I /Users/rgrinberg/github/ocaml/dune/_opam/lib/coq/interp -I /Users/rgrinberg/github/ocaml/dune/_opam/lib/coq/kernel -I /Users/rgrinberg/github/ocaml/dune/_opam/lib/coq/kernel/byterun -I /Users/rgrinberg/github/ocaml/dune/_opam/lib/coq/lib -I /Users/rgrinberg/github/ocaml/dune/_opam/lib/coq/library -I /Users/rgrinberg/github/ocaml/dune/_opam/lib/coq/parsing -I /Users/rgrinberg/github/ocaml/dune/_opam/lib/coq/plugins/ltac -I /Users/rgrinberg/github/ocaml/dune/_opam/lib/coq/pretyping -I /Users/rgrinberg/github/ocaml/dune/_opam/lib/coq/printing -I /Users/rgrinberg/github/ocaml/dune/_opam/lib/coq/proofs -I /Users/rgrinberg/github/ocaml/dune/_opam/lib/coq/stm -I /Users/rgrinberg/github/ocaml/dune/_opam/lib/coq/tactics -I /Users/rgrinberg/github/ocaml/dune/_opam/lib/coq/vernac -I /Users/rgrinberg/github/ocaml/dune/_opam/lib/ocaml -I /Users/rgrinberg/github/ocaml/dune/_opam/lib/ocaml/threads -I /Users/rgrinberg/github/ocaml/dune/_opam/lib/zarith -I src_a -I src_b -R thy1 thy1 thy1/a.v)
  Error: System error: "./thy1/.a.aux: Permission denied"
  
  -> required by thy1/a.vos
  -> required by alias thy1/all
          coqc thy2/.a.aux,thy2/a.{glob,vo}
          coqc thy2/a.vos (exit 1)
  (cd _build/default && /Users/rgrinberg/github/ocaml/dune/_opam/bin/coqc -q -w -native-compiler-disabled -native-compiler ondemand -I /Users/rgrinberg/github/ocaml/dune/_opam/lib/coq/clib -I /Users/rgrinberg/github/ocaml/dune/_opam/lib/coq/config -I /Users/rgrinberg/github/ocaml/dune/_opam/lib/coq/engine -I /Users/rgrinberg/github/ocaml/dune/_opam/lib/coq/gramlib/.pack -I /Users/rgrinberg/github/ocaml/dune/_opam/lib/coq/interp -I /Users/rgrinberg/github/ocaml/dune/_opam/lib/coq/kernel -I /Users/rgrinberg/github/ocaml/dune/_opam/lib/coq/kernel/byterun -I /Users/rgrinberg/github/ocaml/dune/_opam/lib/coq/lib -I /Users/rgrinberg/github/ocaml/dune/_opam/lib/coq/library -I /Users/rgrinberg/github/ocaml/dune/_opam/lib/coq/parsing -I /Users/rgrinberg/github/ocaml/dune/_opam/lib/coq/plugins/ltac -I /Users/rgrinberg/github/ocaml/dune/_opam/lib/coq/pretyping -I /Users/rgrinberg/github/ocaml/dune/_opam/lib/coq/printing -I /Users/rgrinberg/github/ocaml/dune/_opam/lib/coq/proofs -I /Users/rgrinberg/github/ocaml/dune/_opam/lib/coq/stm -I /Users/rgrinberg/github/ocaml/dune/_opam/lib/coq/tactics -I /Users/rgrinberg/github/ocaml/dune/_opam/lib/coq/vernac -I /Users/rgrinberg/github/ocaml/dune/_opam/lib/ocaml -I /Users/rgrinberg/github/ocaml/dune/_opam/lib/ocaml/threads -I /Users/rgrinberg/github/ocaml/dune/_opam/lib/zarith -I src_a -I src_b -Q thy1 thy1 -R thy2 thy2 thy2/a.v)
  Error: System error: "./thy2/.a.aux: Permission denied"
  
  -> required by thy2/a.vos
  -> required by install lib/coq/user-contrib/thy2/a.vos
  -> required by cplugin.install
  -> required by alias all
  [1]
