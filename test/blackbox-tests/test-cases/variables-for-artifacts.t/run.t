Most tests have two versions: one where the variable is used inside a dune file,
and one where the same variables are used in the command line.

  $ cat >sdune <<'EOF'
  > #!/usr/bin/env bash
  > DUNE_SANDBOX=symlink dune "$@"
  > EOF
  $ chmod +x sdune

We begin with a that tries to build several modules defined in the current
directory.

- a belongs to a wrapped library
- b belongs to an executable
- c belongs to an unwrapped library
- d belongs to a wrapped library (transition mode) : in this case, both the
prefixed and unprefixed modules are built.

  $ ./sdune clean
  $ ./sdune build --display short @t1
      ocamldep .a1.objs/a.ml.d
        ocamlc .b.eobjs/byte/dune__exe__B.{cmi,cmo,cmt}
        ocamlc .c1.objs/byte/c.{cmi,cmo,cmt}
      ocamldep .c2.objs/d.ml.d
        ocamlc .a1.objs/byte/a1.{cmi,cmo,cmt}
        ocamlc .c2.objs/byte/c2.{cmi,cmo,cmt}
        ocamlc .a1.objs/byte/a1__A.{cmi,cmo,cmt}
        ocamlc .c2.objs/byte/c2__D.{cmi,cmo,cmt}

Command line version.

  $ ./sdune build --verbose %{cmo:a} %{cmo:b} %{cmo:c} 2>&1 | grep -A100 'Actual targets'
  Actual targets:
  - _build/default/.a1.objs/byte/a1__A.cmo
  - _build/default/.b.eobjs/byte/dune__exe__B.cmo
  - _build/default/.c1.objs/byte/c.cmo

The next test tries to build a .cmi file (of a module in a wrapped library).

  $ ./sdune clean
  $ ./sdune build --display short @t2
      ocamldep .a1.objs/a.ml.d
        ocamlc .a1.objs/byte/a1.{cmi,cmo,cmt}
        ocamlc .a1.objs/byte/a1__A.{cmi,cmo,cmt}

Command line version.

  $ ./sdune build --verbose %{cmi:a} 2>&1 | grep -A100 'Actual targets'
  Actual targets:
  - _build/default/.a1.objs/byte/a1__A.cmi

The next test tries to build a module that does not exist.

  $ mkdir ex1
  $ echo "(lang dune 2.1)" > ex1/dune-project
  $ cat > ex1/dune << EOF
  > (alias
  >  (name t)
  >  (deps %{cmo:foo}))
  > EOF
  $ ./sdune build --root ex1 @t
  Entering directory 'ex1'
  File "dune", line 3, characters 9-17:
  3 |  (deps %{cmo:foo}))
               ^^^^^^^^
  Error: Module Foo does not exist.
  [1]

Command line version; note that the error message is slightly different.

  $ ./sdune build %{cmo:xxxx}
  File "command line", line 1, characters 2-11:
  Error: Module Xxxx does not exist.
  [1]

The next test builds a native .cmxa.

  $ ./sdune clean
  $ ./sdune build --display short @t4
        ocamlc .a1.objs/byte/a1.{cmi,cmo,cmt}
      ocamldep .a1.objs/a.ml.d
      ocamlopt .a1.objs/native/a1.{cmt,cmx,o}
        ocamlc .a1.objs/byte/a1__A.{cmi,cmo,cmt}
      ocamlopt .a1.objs/native/a1__A.{cmt,cmx,o}
      ocamlopt a1.{a,cmxa}

Command line version.

  $ ./sdune build --verbose %{cmxa:a1} 2>&1 | grep -A100 'Actual targets'
  Actual targets:
  - _build/default/a1.cmxa

This test tries to build a non-existent .cma.

  $ mkdir ex2
  $ echo "(lang dune 2.1)" > ex2/dune-project
  $ cat > ex2/dune << EOF
  > (alias
  >  (name t)
  >  (deps %{cma:bar}))
  > EOF
  $ ./sdune build --root ex2 @t
  Entering directory 'ex2'
  File "dune", line 3, characters 9-17:
  3 |  (deps %{cma:bar}))
               ^^^^^^^^
  Error: Library bar does not exist.
  [1]

Command line version.

  $ ./sdune build %{cma:bar_}
  File "command line", line 1, characters 2-11:
  Error: Library bar_ does not exist.
  [1]

This test tries to build a .cma in a subdirectory, where a different project is
defined. The library is public in this case, but we use the local name.

  $ ./sdune clean
  $ ./sdune build --display short @t6
        ocamlc sub2/.bar2.objs/byte/bar2.{cmi,cmo,cmt}
      ocamldep sub2/.bar2.objs/y2.ml.d
        ocamlc sub2/.bar2.objs/byte/bar2__Y2.{cmi,cmo,cmt}
        ocamlc sub2/bar2.cma

Command line version.

  $ ./sdune build --verbose %{cma:sub2/bar2} 2>&1 | grep -A100 'Actual targets'
  Actual targets:
  - _build/default/sub2/bar2.cma

This test builds a .cmo in a subdirectory (same project).

  $ ./sdune clean
  $ ./sdune build --display short @t7
      ocamldep sub/.bar.objs/x.ml.d
        ocamlc sub/.bar.objs/byte/bar.{cmi,cmo,cmt}
        ocamlc sub/.bar.objs/byte/bar__X.{cmi,cmo,cmt}

Command line version.

  $ ./sdune build --verbose %{cmo:sub/x} 2>&1 | grep -A100 'Actual targets'
  Actual targets:
  - _build/default/sub/.bar.objs/byte/bar__X.cmo

This test builds a module in a subdirectory (different project) belonging to a
private library.

  $ ./sdune clean
  $ ./sdune build --display short @t8
      ocamldep sub3/.c1.objs/x.ml.d
        ocamlc sub3/.c1.objs/byte/c1.{cmi,cmo,cmt}
        ocamlc sub3/.c1.objs/byte/c1__X.{cmi,cmo,cmt}

COmmand line version.

  $ ./sdune build --verbose %{cmo:sub3/x} 2>&1 | grep -A100 'Actual targets'
  Actual targets:
  - _build/default/sub3/.c1.objs/byte/c1__X.cmo

This test builds a private library in a subdirectory belonging to a different
project.

  $ ./sdune clean
  $ ./sdune build --display short @t9
        ocamlc sub3/.c1.objs/byte/c1.{cmi,cmo,cmt}
      ocamldep sub3/.c1.objs/x.ml.d
        ocamlc sub3/.c1.objs/byte/c1__X.{cmi,cmo,cmt}
        ocamlc sub3/c1.cma

Command line version.

  $ ./sdune build --verbose %{cma:sub3/c1} 2>&1 | grep -A100 'Actual targets'
  Actual targets:
  - _build/default/sub3/c1.cma

This test builds a library in the current directory that has the same name as a
public library defined in a subdirectory.

  $ ./sdune clean
  $ ./sdune build --display short @t10
        ocamlc .c1.objs/byte/c.{cmi,cmo,cmt}
        ocamlc c1.cma

Command line version.

  $ ./sdune build --verbose %{cma:c1} 2>&1 | grep -A100 'Actual targets'
  Actual targets:
  - _build/default/c1.cma

This test checks error handling.

  $ ./sdune build %{cma:../x}
  File "command line", line 1, characters 2-11:
  Error: Library x does not exist.
  [1]

This test checks that everything still works if we invoke dune from a
subdirectory.

  $ (cd sub && dune build --display short %{cmx:x})
      ocamldep .bar.objs/x.ml.d
        ocamlc .bar.objs/byte/bar.{cmi,cmo,cmt}
        ocamlc .bar.objs/byte/bar__X.{cmi,cmo,cmt}
      ocamlopt .bar.objs/native/bar__X.{cmt,cmx,o}

The following test checks that the variables can be used in the (action) field
of a (rule).

  $ ./sdune build --display short _build/default/my.cmxs
      ocamldep .dummy.objs/x3.ml.d
        ocamlc .plugin.objs/byte/plugin.{cmi,cmo,cmt}
      ocamldep .plugin.objs/x1.ml.d
      ocamldep .plugin.objs/x2.ml.d
        ocamlc .dummy.objs/byte/dummy.{cmi,cmo,cmt}
      ocamlopt .plugin.objs/native/plugin.{cmt,cmx,o}
        ocamlc .plugin.objs/byte/plugin__X1.{cmi,cmo,cmt}
        ocamlc .plugin.objs/byte/plugin__X2.{cmi,cmo,cmt}
        ocamlc .dummy.objs/byte/dummy__X3.{cmi,cmo,cmt}
      ocamlopt .plugin.objs/native/plugin__X1.{cmt,cmx,o}
      ocamlopt .plugin.objs/native/plugin__X2.{cmt,cmx,o}
      ocamlopt .dummy.objs/native/dummy__X3.{cmt,cmx,o}
      ocamlopt plugin.{a,cmxa}
      ocamlopt my.cmxs

The following (failing) test shows that the variables cannot yet be used in the (deps)
field of a (rule).

  $ mkdir deps-fail
  $ echo "(lang dune 2.1)" > deps-fail/dune-project
  $ cat > deps-fail/dune << EOF
  > (rule
  >  (target t)
  >  (deps %{cmo:x2})
  >  (action (with-stdout-to %{target} (progn))))
  > EOF
  $ ./sdune build --root deps-fail t
  Entering directory 'deps-fail'
  File "dune", line 3, characters 9-16:
  3 |  (deps %{cmo:x2})
               ^^^^^^^
  Error: %{cmo:..} isn't allowed in this position
  [1]
