This test generates documentation using odoc for a library:

  $ dune build @doc

  $ find _build/default/_doc/_odoc -name '*.odoc'
  _build/default/_doc/_odoc/pkg/foo/page-index.odoc
  _build/default/_doc/_odoc/pkg/bar/page-index.odoc

.odocl files should be generated in the same path as the original .odoc file

  $ find _build/default/_doc/_odoc -name '*.odocl'
  _build/default/_doc/_odoc/pkg/foo/page-index.odocl
  _build/default/_doc/_odoc/pkg/bar/page-index.odocl

  $ dune runtest
  <!DOCTYPE html>
  <html xmlns="http://www.w3.org/1999/xhtml">
    <head>
      <title>index</title>
      <link rel="stylesheet" href="./odoc.css"/>
      <meta charset="utf-8"/>
      <meta name="viewport" content="width=device-width,initial-scale=1.0"/>
    </head>
    <body>
      <main class="content">
        <div class="by-name">
        <h2>OCaml package documentation</h2>
        <ol>
        <li><a href="bar/index.html">bar</a></li>
        <li><a href="foo/index.html">foo</a></li>
        </ol>
        </div>
      </main>
    </body>
  </html>

  $ dune build @foo-mld
  {0 foo index}
  {1 Library foo}
  This library exposes the following toplevel modules:
  {!modules:Foo Foo2}
  {1 Library foo.byte}
  The entry point of this library is the module:
  {!module-Foo_byte}.

  $ dune build @bar-mld
  {0 bar index}
  {1 Library bar}
  The entry point of this library is the module:
  {!module-Bar}.
