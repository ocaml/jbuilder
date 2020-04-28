Simple test with a multi dir exe
--------------------------------

  $ dune build --root test1
  Entering directory 'test1'
           foo alias default
  Hello, world!

Test that include_subdirs stop the recursion
--------------------------------------------

  $ dune build --root test2
  Entering directory 'test2'
          main alias default
  Hello, world!

Test with C stubs in sub-directories
------------------------------------

  $ dune runtest --root test3
  Entering directory 'test3'
  File "dune", line 9, characters 16-25:
  9 |  (c_names stub1 sub/stub2))
                      ^^^^^^^^^
  Error: Relative part of stub is not necessary and should be removed. To
  include sources in subdirectories, use the (include_subdirs ...) stanza.
  [1]

Test some error cases
---------------------

  $ dune build --root error1
  Entering directory 'error1'
  File "dune", line 1, characters 0-0:
  Error: Module "X" appears in several directories:
  - _build/default/b
  - _build/default/a
  This is not allowed, please rename one of them.
  [1]

  $ dune build --root error2
  Entering directory 'error2'
  File "dune", line 2, characters 0-29:
  2 | (include_subdirs unqualified)
      ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
  Error: The 'include_subdirs' stanza cannot appear more than once
  [1]

  $ dune build --root error3
  Entering directory 'error3'
  File "src/gen/dune", line 1, characters 0-23:
  1 | (executable (name gen))
      ^^^^^^^^^^^^^^^^^^^^^^^
  Error: This stanza is not allowed in a sub-directory of directory with
  (include_subdirs unqualified).
  Hint: add (include_subdirs no) to this file.
  [1]

Test for (include_subdir unqualified) with (preprocess (action ...))
--------------------------------------------------------------------

  $ dune build --display short --root test4 @all
  Entering directory 'test4'
      ocamldep .main.eobjs/main.ml.d
        ocamlc .main.eobjs/byte/main.{cmi,cmo,cmt}
        ocamlc main.bc
      ocamlopt .main.eobjs/native/main.{cmx,o}
      ocamlopt main.exe
          main sub/foo.pp.ml
