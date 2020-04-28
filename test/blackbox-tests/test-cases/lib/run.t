----------------------------------------------------------------------------------
Testsuite for the %{lib...} and %{lib-private...} variable.
TODO: Fix %{libexec} and %{libexec-private} variables and test them.

  $ cat >sdune <<'EOF'
  > #!/usr/bin/env bash
  > DUNE_SANDBOX=symlink dune "$@"
  > EOF
  $ chmod +x sdune

----------------------------------------------------------------------------------
* Find a public library using the %{lib:...} variable

  $ echo "(lang dune 2.0)" > dune-project
  $ mkdir -p src
  $ cat >src/dune <<EOF
  > (library
  >  (name private_lib)
  >  (public_name public_lib)
  >  (modules a))
  > EOF

  $ cat >dune <<EOF
  > (rule
  >  (alias find-a)
  >  (action (echo "%{lib:public_lib:a.ml}")))
  > EOF

  $ cat >src/a.ml <<EOF
  > let a = "A"
  > EOF
  $ cat >src/a.mli <<EOF
  > val a : string
  > EOF

  $ touch public_lib.opam
  $ ./sdune build @find-a
  ../install/default/lib/public_lib/a.ml

----------------------------------------------------------------------------------
* Error when finding a public library by its private name using the %{lib:...} variable

  $ cat >src/dune <<EOF
  > (library
  >  (name private_lib)
  >  (public_name public_lib)
  >  (modules a))
  > EOF

  $ cat >dune <<EOF
  > (rule
  >  (alias find-a)
  >  (action (echo "%{lib:private_lib:a.ml}")))
  > EOF

  $ ./sdune build @find-a
  File "dune", line 3, characters 18-39:
  3 |  (action (echo "%{lib:private_lib:a.ml}")))
                        ^^^^^^^^^^^^^^^^^^^^^
  Error: The library "private_lib" is not public. The variable "lib" expands to
  the file's installation path which is not defined for private libraries.
  [1]

----------------------------------------------------------------------------------
* Find a public library by its private name using the %{lib-private:...} variable
* Error when using Dune language < 2.1

  $ cat >src/dune <<EOF
  > (library
  >  (name private_lib)
  >  (public_name public_lib)
  >  (modules a))
  > EOF

  $ cat >dune <<EOF
  > (rule
  >  (alias find-a)
  >  (action (echo "%{lib-private:private_lib:a.ml}")))
  > EOF

  $ ./sdune build @find-a
  File "dune", line 3, characters 18-47:
  3 |  (action (echo "%{lib-private:private_lib:a.ml}")))
                        ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
  Error: %{lib-private:..} is only available since version 2.1 of the dune
  language. Please update your dune-project file to have (lang dune 2.1).
  [1]

----------------------------------------------------------------------------------
* Find a public library by its private name using the %{lib-private:...} variable
* Success when using Dune language 2.1

  $ echo "(lang dune 2.1)" > dune-project

  $ ./sdune build @find-a
  src/a.ml

----------------------------------------------------------------------------------
* Find a private library using the %{lib-private:...} variable

  $ cat >src/dune <<EOF
  > (library
  >  (name private_lib)
  >  (modules a))
  > EOF

  $ ./sdune clean
  $ ./sdune build @find-a
  src/a.ml

----------------------------------------------------------------------------------
* The %{lib-private:...} variable works with public library names too

  $ cat >src/dune <<EOF
  > (library
  >  (name private_lib)
  >  (public_name public_lib)
  >  (modules a))
  > EOF

  $ cat >dune <<EOF
  > (rule
  >  (alias find-a)
  >  (action (echo "%{lib-private:public_lib:a.ml}")))
  > EOF

  $ ./sdune clean
  $ ./sdune build @find-a
  src/a.ml

----------------------------------------------------------------------------------
* The %{lib-private:...} variable does not work with external libraries

  $ mkdir -p external
  $ cat >external/dune-project <<EOF
  > (lang dune 2.1)
  > (name external_library)
  > EOF
  $ cat >external/dune <<EOF
  > (library
  >  (name extlib)
  >  (public_name external_library))
  > EOF

  $ touch external/external_library.opam
  $ ( cd external && ../sdune build @install && ../sdune install --prefix install)
  Installing install/lib/external_library/META
  Installing install/lib/external_library/dune-package
  Installing install/lib/external_library/extlib.cmi
  Installing install/lib/external_library/extlib.cmt
  Installing install/lib/external_library/extlib.cmx
  Installing install/lib/external_library/extlib.ml
  Installing install/lib/external_library/lib$ext_lib
  Installing install/lib/external_library/lib.cma
  Installing install/lib/external_library/lib.cmxa
  Installing install/lib/external_library/lib.cmxs
  Installing install/lib/external_library/opam

  $ cat >src/dune <<EOF
  > (library
  >  (name private_lib)
  >  (public_name public_lib)
  >  (modules a))
  > (rule
  >  (alias find-a)
  >  (action (echo "%{lib-private:external_library:opam}")))
  > EOF

  $ touch src/public_lib.opam
  $ echo "(lang dune 2.1)" > src/dune-project
  $ echo "(name test-lib)" >> src/dune-project

  $ export OCAMLPATH=$PWD/external/install/lib; ./sdune build @find-a --root=src
  Entering directory 'src'
  File "dune", line 7, characters 18-52:
  7 |  (action (echo "%{lib-private:external_library:opam}")))
                        ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
  Error: The variable "lib-private" can only refer to libraries within the same
  project. The current project's name is "test-lib", but the reference is to an
  external library.
  [1]

----------------------------------------------------------------------------------
* The %{lib-private:...} is only allowed within the same project

  $ mkdir -p another
  $ cat >another/dune <<EOF
  > (library
  >  (name anotherlib)
  >  (public_name another_library))
  > EOF

  $ cat >src/dune <<EOF
  > (library
  >  (name private_lib)
  >  (public_name public_lib)
  >  (modules a))
  > (rule
  >  (alias find-a)
  >  (action (echo "%{lib-private:another_library:file}")))
  > EOF

  $ touch another/another_library.opam
  $ rm public_lib.opam
  $ rm dune
  $ echo "(lang dune 2.1)" > another/dune-project
  $ echo "(name another-lib)" >> another/dune-project

  $ ./sdune build @find-a
  File "src/dune", line 7, characters 18-51:
  7 |  (action (echo "%{lib-private:another_library:file}")))
                        ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
  Error: The variable "lib-private" can only refer to libraries within the same
  project. The current project's name is "test-lib", but the reference is to
  "another-lib".
  [1]
