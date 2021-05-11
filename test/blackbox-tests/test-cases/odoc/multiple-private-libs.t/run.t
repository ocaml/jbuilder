This test checks that there is no clash when two private libraries have the same name

  $ dune build --display short @doc-private
          odoc _doc/_html/highlight.pack.js,_doc/_html/odoc.css
        ocamlc a/.test.objs/byte/test.{cmi,cmo,cmt}
        ocamlc b/.test.objs/byte/test.{cmi,cmo,cmt}
          odoc a/.test.objs/byte/test.odoc
          odoc b/.test.objs/byte/test.odoc
          odoc a/.test.objs/byte/test.odocl
          odoc b/.test.objs/byte/test.odocl
          odoc _doc/_html/test@6aabb9861046/Test/.dune-keep,_doc/_html/test@6aabb9861046/Test/index.html
          odoc _doc/_html/test@ea8c79305c05/Test/.dune-keep,_doc/_html/test@ea8c79305c05/Test/index.html
