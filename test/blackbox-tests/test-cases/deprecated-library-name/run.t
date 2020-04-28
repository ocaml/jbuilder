Test the `deprecated_library_name` stanza
=========================================

Test that deprecated library names are supported in a given workspace
---------------------------------------------------------------------

  $ for i in a b c; do
  >   mkdir $i
  >   cat >$i/dune-project <<EOF
  > (lang dune 2.0)
  > (package (name $i))
  > EOF
  > done

  $ cat >a/dune <<EOF
  > (deprecated_library_name (old_public_name a) (new_public_name b))
  > EOF

  $ cat >b/dune <<EOF
  > (library (public_name b))
  > EOF

  $ cat >b/b.ml <<EOF
  > let x = "Hello, world!"
  > EOF

  $ cat >c/dune <<EOF
  > (executable (name prog) (libraries a))
  > EOF

  $ cat >c/prog.ml <<EOF
  > print_endline B.x
  > EOF

  $ dune exec c/prog.exe
  Hello, world!

Test that deprecated library names can be installed
---------------------------------------------------

  $ dune clean

Note that at this point the library "b" does not exist, so this also
tests that the "old_public_name" field is evaluated lazily

  $ (cd a
  > dune build @install --root .
  > dune install --prefix $PWD/../_install)
  Installing $TESTCASE_ROOT/a/../_install/lib/a/META
  Installing $TESTCASE_ROOT/a/../_install/lib/a/dune-package

  $ rm -rf a

  $ cat $PWD/_install/lib/a/META
  requires = "b"

  $ cat $PWD/_install/lib/a/dune-package | sed "s/(lang dune .*)/(lang dune <version>)/"
  (lang dune <version>)
  (name a)
  (deprecated_library_name (old_public_name a) (new_public_name b))

Now we install "b". We do need to install it as an installed
deprecated library will be resolved in the installed world only.

  $ (cd b
  > dune build @install --root .
  > dune install --prefix $PWD/../_install)
  Installing $TESTCASE_ROOT/b/../_install/lib/b/META
  Installing $TESTCASE_ROOT/b/../_install/lib/b/b.cmi
  Installing $TESTCASE_ROOT/b/../_install/lib/b/b.cmt
  Installing $TESTCASE_ROOT/b/../_install/lib/b/b.cmx
  Installing $TESTCASE_ROOT/b/../_install/lib/b/b.ml
  Installing $TESTCASE_ROOT/b/../_install/lib/b/dune-package
  Installing $TESTCASE_ROOT/b/../_install/lib/b/lib$ext_lib
  Installing $TESTCASE_ROOT/b/../_install/lib/b/lib.cma
  Installing $TESTCASE_ROOT/b/../_install/lib/b/lib.cmxa
  Installing $TESTCASE_ROOT/b/../_install/lib/b/lib.cmxs

  $ rm -rf b

  $ env OCAMLPATH=$PWD/_install/lib dune exec c/prog.exe
  Hello, world!

This error message could be improved to mention that it is in fact "b"
that wasn't found:

  $ rm -rf _install/lib/b

  $ env OCAMLPATH=$PWD/_install/lib dune exec c/prog.exe
  File "c/dune", line 1, characters 35-36:
  1 | (executable (name prog) (libraries a))
                                         ^
  Error: Library "a" not found.
  Hint: try: dune external-lib-deps --missing c/prog.exe
  [1]

Test that we can migrate top-level libraries
--------------------------------------------

  $ mkdir d

First the motivating case.

  $ cat >d/dune-project <<EOF
  > (lang dune 2.0)
  > (package (name menhir) (deprecated_package_names menhirLib menhirSdk dummy))
  > EOF

  $ cat >d/dune <<EOF
  > (rule (with-stdout-to lib.ml (progn)))
  > (library
  >  (name menhirLib)
  >  (public_name menhir.lib)
  >  (modules lib))
  > (deprecated_library_name
  >  (old_public_name menhirLib)
  >  (new_public_name menhir.lib))
  > (rule (with-stdout-to sdk.ml (echo "let version = ()")))
  > (library
  >  (name menhirSdk)
  >  (public_name menhir.sdk)
  >  (modules sdk))
  > (deprecated_library_name
  >  (old_public_name menhirSdk)
  >  (new_public_name menhir.sdk))
  > EOF

  $ (cd d && dune build --root . @install)

  $ find d/_build/install/default -name 'META' | sort
  d/_build/install/default/lib/dummy/META
  d/_build/install/default/lib/menhir/META
  d/_build/install/default/lib/menhirLib/META
  d/_build/install/default/lib/menhirSdk/META

  $ cat d/_build/install/default/lib/dummy/META
  

  $ cat d/_build/install/default/lib/menhirLib/META
  requires = "menhir.lib"
  $ cat d/_build/install/default/lib/menhirSdk/META
  requires = "menhir.sdk"

  $ find d/_build/install/default -name 'dune-package' | sort
  d/_build/install/default/lib/dummy/dune-package
  d/_build/install/default/lib/menhir/dune-package
  d/_build/install/default/lib/menhirLib/dune-package
  d/_build/install/default/lib/menhirSdk/dune-package

  $ cat d/_build/install/default/lib/dummy/dune-package | sed "s/(lang dune .*)/(lang dune <version>)/"
  (lang dune <version>)
  (name dummy)

  $ cat d/_build/install/default/lib/menhirLib/dune-package | sed "s/(lang dune .*)/(lang dune <version>)/"
  (lang dune <version>)
  (name menhirLib)
  (deprecated_library_name
   (old_public_name menhirLib)
   (new_public_name menhir.lib))

  $ cat d/_build/install/default/lib/menhirSdk/dune-package | sed "s/(lang dune .*)/(lang dune <version>)/"
  (lang dune <version>)
  (name menhirSdk)
  (deprecated_library_name
   (old_public_name menhirSdk)
   (new_public_name menhir.sdk))


Check that we can use the short name in library dependencies.

  $ cat >>d/dune <<EOF
  > (rule (with-stdout-to use.ml (echo "let _ = MenhirSdk.Sdk.version")))
  > (library
  >  (name foo)
  >  (public_name menhir.foo)
  >  (libraries menhirSdk menhirLib)
  >  (modules use))
  > EOF

  $ (cd d && dune build --root . @all)

Checks that we can migrate top-level libraries across packages.

  $ cat >d/dune-project <<EOF
  > (lang dune 2.0)
  > (package (name p) (deprecated_package_names top1 top2))
  > (package (name q))
  > EOF

  $ cat >d/dune <<EOF
  > (rule (with-stdout-to foo.ml (progn)))
  > (library
  >  (name foo)
  >  (public_name q.bar)
  >  (modules foo))
  > (deprecated_library_name
  >  (old_public_name top1)
  >  (new_public_name q.bar))
  > EOF

  $ (cd d && dune build --root . @install)

  $ cat d/_build/install/default/lib/top1/META
  requires = "q.bar"

Check that we can do it when the name of the new library is the same as the
old public name:

  $ cat >d/dune <<EOF
  > (rule (with-stdout-to bar.ml (progn)))
  > (library
  >  (name top2)
  >  (public_name q.top2)
  >  (modules bar))
  > (deprecated_library_name
  >  (old_public_name top2)
  >  (new_public_name q.top2))
  > EOF

  $ (cd d && dune build --root . @all)

  $ cat d/_build/install/default/lib/top2/META
  requires = "q.top2"

We check that there is an error when there is an actual ambiguity:

  $ cat >d/dune <<EOF
  > (rule (with-stdout-to bar.ml (progn)))
  > (rule (with-stdout-to bar2.ml (progn)))
  > (library
  >  (name top2)
  >  (public_name q.top2)
  >  (modules bar))
  > (library
  >  (name top3)
  >  (public_name q.top3)
  >  (modules bar2))
  > (deprecated_library_name
  >  (old_public_name top2)
  >  (new_public_name q.top3))
  > EOF

  $ (cd d && dune build --root . @all)
  Error: Library top2 is defined twice:
  - dune:13
  - dune:5
  [1]

Another case of ambiguity:

  $ cat >d/dune-project <<EOF
  > (lang dune 2.0)
  > (package (name p))
  > (package (name q) (deprecated_package_names p))
  > EOF

  $ cat >d/dune <<EOF
  > (rule (with-stdout-to bar.ml (progn)))
  > (library
  >  (name p)
  >  (public_name p)
  >  (modules bar))
  > (deprecated_library_name
  >  (old_public_name p)
  >  (new_public_name p))
  > EOF

  $ (cd d && dune build --root . --display=short @all)
  Error: Package name p is defined twice:
  - dune-project:3
  - dune-project:2
  [1]

Qualified, deprecated old_public_name:

  $ cat >d/dune-project <<EOF
  > (lang dune 2.0)
  > (package (name p) (deprecated_package_names q))
  > EOF

  $ cat >d/dune <<EOF
  > (rule (with-stdout-to bar.ml (progn)))
  > (library
  >  (name p)
  >  (public_name p)
  >  (modules bar))
  > (deprecated_library_name
  >  (old_public_name q.foo)
  >  (new_public_name p))
  > EOF

  $ (cd d && dune build --root . @all)

  $ find d/_build/install/default -name 'META' | sort
  d/_build/install/default/lib/p/META
  d/_build/install/default/lib/q/META

  $ cat d/_build/install/default/lib/q/META
  package "foo" (
    requires = "p"
  )

  $ find d/_build/install/default -name 'dune-package' | sort
  d/_build/install/default/lib/p/dune-package
  d/_build/install/default/lib/q/dune-package

  $ cat d/_build/install/default/lib/q/dune-package | sed "s/(lang dune .*)/(lang dune <version>)/"
  (lang dune <version>)
  (name q)
  (deprecated_library_name (old_public_name q.foo) (new_public_name p))

Two libraries redirecting to the same library:

  $ cat >d/dune-project <<EOF
  > (lang dune 2.0)
  > (package (name p) (deprecated_package_names q))
  > EOF

  $ cat >d/dune <<EOF
  > (rule (with-stdout-to bar.ml (progn)))
  > (library
  >  (name p)
  >  (public_name p)
  >  (modules bar))
  > (deprecated_library_name
  >  (old_public_name q.foo)
  >  (new_public_name p))
  > (deprecated_library_name
  >  (old_public_name q.bar)
  >  (new_public_name p))
  > EOF

  $ (cd d && dune build --root . @all)

  $ find d/_build/install/default -name 'META' | sort
  d/_build/install/default/lib/p/META
  d/_build/install/default/lib/q/META

  $ cat d/_build/install/default/lib/q/META
  package "bar" (
    requires = "p"
  )
  package "foo" (
    requires = "p"
  )

  $ find d/_build/install/default -name 'dune-package' | sort
  d/_build/install/default/lib/p/dune-package
  d/_build/install/default/lib/q/dune-package

  $ cat d/_build/install/default/lib/q/dune-package | sed "s/(lang dune .*)/(lang dune <version>)/"
  (lang dune <version>)
  (name q)
  (deprecated_library_name (old_public_name q.bar) (new_public_name p))
  (deprecated_library_name (old_public_name q.foo) (new_public_name p))

Check that we can use deprecated packages from within the same project and
across projects.

  $ mkdir -p d/p/a d/p/b

  $ cat >d/p/a/dune-project <<EOF
  > (lang dune 2.0)
  > (package (name a) (deprecated_package_names aa))
  > EOF

  $ cat >d/p/b/dune-project <<EOF
  > (lang dune 2.0)
  > (package (name b))
  > EOF

  $ cat >d/p/a/dune <<EOF
  > (rule (with-stdout-to empty1.ml (progn)))
  > (rule (with-stdout-to empty2.ml (progn)))
  > (deprecated_library_name
  >  (old_public_name aa.foo)
  >  (new_public_name a.p))
  > (library
  >  (name p)
  >  (public_name a.p)
  >  (modules empty1))
  > (library
  >  (name q)
  >  (libraries aa.foo)
  >  (modules empty2))
  > EOF

  $ cat >d/p/b/dune <<EOF
  > (rule (with-stdout-to empty.ml (progn)))
  > (library
  >  (name b)
  >  (libraries aa.foo))
  > EOF

  $ (cd d/p && dune build --root . @all)
