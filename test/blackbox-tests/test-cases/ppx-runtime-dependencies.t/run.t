----------------------------------------------------------------------------------
Handling ppx_runtime_libraries dependencies correctly

  $ cat >sdune <<'EOF'
  > #!/usr/bin/env bash
  > DUNE_SANDBOX=symlink dune "$@"
  > EOF
  $ chmod +x sdune

----------------------------------------------------------------------------------
* Incorrect cycle detection due to ppx_runtime_libraries (TODO: fix this bug!)

  $ cat >dune-project <<EOF
  > (lang dune 2.0)
  > (implicit_transitive_deps true)
  > EOF

  $ cat >dune <<EOF
  > (library
  >  (name a)
  >  (modules a)
  >  (libraries b))
  > (library
  >  (name b)
  >  (kind ppx_rewriter)
  >  (modules b ppx)
  >  (libraries ppxlib)
  >  (ppx_runtime_libraries c))
  > (library
  >  (name c)
  >  (modules c)
  >  (libraries a))
  > EOF

  $ mkdir -p bin
  $ cat >bin/dune <<EOF
  > (executable
  >  (name main)
  >  (preprocess (pps b))
  >  (modules main))
  > EOF

  $ cat >a.ml <<EOF
  > include B
  > let a = B.b - 1
  > EOF

  $ cat >b.ml <<EOF
  > let b = 2
  > EOF

  $ cat >c.ml <<EOF
  > include A
  > let c = A.a + 2
  > EOF

  $ cat >bin/main.ml <<EOF
  > let () = Printf.printf "Should print 3: %d\n" [%get_c]
  > EOF

  $ ./sdune exec bin/main.exe
  Should print 3: 3

----------------------------------------------------------------------------------
* Ppx rewriters (and their ppx_runtime_libraries information) are collected recursively
In this case we have the following dependency graph:
main --[pps]--> b ---> ppx --[runtime]--> c ---> a
Note the direct dependency b ---> ppx that separates pps and runtime dependencies.

  $ cat >dune <<EOF
  > (library
  >  (name a)
  >  (modules a))
  > (library
  >  (name b)
  >  (modules b)
  >  (libraries ppx))
  > (library
  >  (name ppx)
  >  (modules ppx)
  >  (kind ppx_rewriter)
  >  (libraries ppxlib)
  >  (ppx_runtime_libraries c)
  >  )
  > (library
  >  (name c)
  >  (modules c)
  >  (libraries a))
  > EOF

  $ cat >a.ml <<EOF
  > let a = 2 - 1
  > EOF

  $ ./sdune exec bin/main.exe
  Should print 3: 3

----------------------------------------------------------------------------------
* Since Dune 2.2, it is not allowed to have a pps dependency on a library that has
not been marked with (kind ppx_rewriter).

  $ cat >dune-project <<EOF
  > (lang dune 2.2)
  > (implicit_transitive_deps true)
  > EOF

  $ ./sdune exec bin/main.exe
  File "bin/dune", line 3, characters 18-19:
  3 |  (preprocess (pps b))
                        ^
  Error: Ppx dependency on a non-ppx library "b". If "b" is in fact a ppx
  rewriter library, it should have (kind ppx_rewriter) in its dune file.
  [1]

----------------------------------------------------------------------------------
* Dependency cycle between ppx rewriters created via [ppx_runtime_libraries]
In this case we have the following dependency graph:
gen_c --[pps]--> ppx --[runtime]--> c --[pps]--> gen_c
Note that pps dependencies are separated by a runtime dependency.

  $ cat >dune <<EOF
  > (library
  >  (name gen_c)
  >  (modules gen_c)
  >  (kind ppx_rewriter)
  >  (libraries ppxlib)
  >  (preprocess (pps ppx)))
  > (library
  >  (name ppx)
  >  (modules ppx)
  >  (kind ppx_rewriter)
  >  (libraries ppxlib)
  >  (ppx_runtime_libraries c)
  >  )
  > (library
  >  (name c)
  >  (modules c)
  >  (libraries ppxlib)
  >  (preprocess (pps gen_c))
  > )
  > EOF

  $ cat >c.ml <<EOF
  > let c = [%c]
  > EOF

  $ cat >bin/dune <<EOF
  > (executable
  >  (name main)
  >  (libraries c)
  >  (modules main))
  > EOF

  $ cat >bin/main.ml <<EOF
  > let () = Printf.printf "Should be impossible: %d\n" C.c
  > EOF

  $ ./sdune exec bin/main.exe
  Error: Dependency cycle detected between the following libraries:
     "c" in _build/default
  -> "gen_c" in _build/default
  -> "c" in _build/default
  -> required by library "c" in _build/default
  -> required by executable main in bin/dune:2
  -> required by _build/default/bin/main.exe
  [1]
