  $ env -u OCAMLRUNPARAM dune runtest simple
           run alias simple/runtest (exit 2)
  (cd _build/default/simple && .foo_simple.inline-tests/run.exe)
  Fatal error: exception File "simple/.foo_simple.inline-tests/run.ml-gen", line 1, characters 40-46: Assertion failed
  [1]

The expected behavior for the following three tests is to output nothing: the tests are disabled or ignored. 
  $ env -u OCAMLRUNPARAM dune runtest simple --profile release

  $ env -u OCAMLRUNPARAM dune runtest simple --profile disable-inline-tests

  $ env -u OCAMLRUNPARAM dune runtest simple --profile ignore-inline-tests

  $ env -u OCAMLRUNPARAM dune runtest simple --profile enable-inline-tests
           run alias simple/runtest (exit 2)
  (cd _build/default/simple && .foo_simple.inline-tests/run.exe)
  Fatal error: exception File "simple/.foo_simple.inline-tests/run.ml-gen", line 1, characters 40-46: Assertion failed
  [1]

  $ dune runtest missing-backend
  File "missing-backend/dune", line 3, characters 1-15:
  3 |  (inline_tests))
       ^^^^^^^^^^^^^^
  Error: No inline tests backend found.
  [1]

  $ dune runtest too-many-backends
  File "too-many-backends/dune", line 17, characters 1-15:
  17 |  (inline_tests)
        ^^^^^^^^^^^^^^
  Error: Too many independent inline tests backends found:
  - "backend_tmb1" in _build/default/too-many-backends
  - "backend_tmb2" in _build/default/too-many-backends
  [1]

  $ dune runtest many-backends-choose
           run alias many-backends-choose/runtest
  backend_mbc1

  $ dune runtest dune-file | sed "s/(lang dune .*)/(lang dune <version>)/"
           run alias dune-file/runtest
  414243
  (lang dune <version>)
  (name foo)
  (library
   (name foo)
   (kind normal)
   (archives (byte lib.cma) (native lib.cmxa))
   (plugins (byte lib.cma) (native lib.cmxs))
   (native_archives lib$ext_lib)
   (main_module_name Foo)
   (modes byte native)
   (modules
    (wrapped
     (main_module_name Foo)
     (alias_module
      (name Foo)
      (obj_name foo)
      (visibility public)
      (kind alias)
      (impl))
     (wrapped true)))
   (inline_tests.backend
    (runner_libraries str)
    (flags
     inline-test-runner
     %{library-name}
     -source-tree-root
     %{workspace_root}
     -diff-cmd
     -)
    (generate_runner
     (progn
      (echo "let () = print_int 41")
      (echo "\n")
      (echo "let () = print_int 42")
      (echo "\n")
      (echo "let () = print_int 43;;")))))

  $ dune build dune-file/foo.install && dune install foo --prefix install
  Installing install/lib/foo/META
  Installing install/lib/foo/dune-package
  Installing install/lib/foo/foo.cmi
  Installing install/lib/foo/foo.cmt
  Installing install/lib/foo/foo.cmx
  Installing install/lib/foo/foo.ml
  Installing install/lib/foo/lib$ext_lib
  Installing install/lib/foo/lib.cma
  Installing install/lib/foo/lib.cmxa
  Installing install/lib/foo/lib.cmxs
  Installing install/lib/foo/opam

Make sure we can read generated dune-package files:

  $ export OCAMLPATH=$PWD/install/lib; dune runtest --root dune-file-user
  Entering directory 'dune-file-user'
           run alias runtest
  414243
