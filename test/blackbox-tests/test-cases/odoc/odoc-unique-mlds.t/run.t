Duplicate mld's in the same scope
  $ dune build @doc --display short --root ./same-scope
  Entering directory 'same-scope'
          odoc _doc/_html/highlight.pack.js,_doc/_html/odoc.css
          odoc _doc/_odoc/pkg/root/page-index.odoc
        ocamlc lib1/.root_lib1.objs/byte/root_lib1.{cmi,cmo,cmt}
        ocamlc lib2/.root_lib2.objs/byte/root_lib2.{cmi,cmo,cmt}
          odoc lib1/.root_lib1.objs/byte/root_lib1.odoc
          odoc lib2/.root_lib2.objs/byte/root_lib2.odoc
          odoc lib1/.root_lib1.objs/byte/root_lib1.odocl
          odoc lib2/.root_lib2.objs/byte/root_lib2.odocl
          odoc _doc/_odoc/pkg/root/page-index.odocl
          odoc _doc/_html/root/Root_lib1/.dune-keep,_doc/_html/root/Root_lib1/index.html
          odoc _doc/_html/root/Root_lib2/.dune-keep,_doc/_html/root/Root_lib2/index.html
          odoc _doc/_html/root/index.html

Duplicate mld's in different scope
  $ rm -rf diff-scope/_build
  $ dune build @doc --display short --root ./diff-scope
  Entering directory 'diff-scope'
          odoc _doc/_html/highlight.pack.js,_doc/_html/odoc.css
          odoc _doc/_odoc/pkg/scope1/page-index.odoc
        ocamlc scope1/.scope1.objs/byte/scope1.{cmi,cmo,cmt}
          odoc _doc/_odoc/pkg/scope2/page-index.odoc
        ocamlc scope2/.scope2.objs/byte/scope2.{cmi,cmo,cmt}
          odoc scope1/.scope1.objs/byte/scope1.odoc
          odoc scope2/.scope2.objs/byte/scope2.odoc
          odoc scope1/.scope1.objs/byte/scope1.odocl
          odoc _doc/_odoc/pkg/scope1/page-index.odocl
          odoc scope2/.scope2.objs/byte/scope2.odocl
          odoc _doc/_odoc/pkg/scope2/page-index.odocl
          odoc _doc/_html/scope1/Scope1/.dune-keep,_doc/_html/scope1/Scope1/index.html
          odoc _doc/_html/scope1/index.html
          odoc _doc/_html/scope2/Scope2/.dune-keep,_doc/_html/scope2/Scope2/index.html
          odoc _doc/_html/scope2/index.html
