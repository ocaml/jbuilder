rules with dependencies outside the build dir are allowed

  $ mkdir a

  $ cat >a/dune-project <<EOF
  > (lang dune 2.8)
  > EOF

# Test relative 1 level below
  $ cat >a/dune <<EOF
  > (rule
  >  (alias test)
  >  (action (with-stdin-from "../external.txt" (run cat -))))
  > EOF

  $ cat >external.txt <<EOF
  > txt1
  > EOF

  $ dune build --root=a @test
  Entering directory 'a'
  File "dune", line 1, characters 0-78:
  1 | (rule
  2 |  (alias test)
  3 |  (action (with-stdin-from "../external.txt" (run cat -))))
  Error: File unavailable: _build/external.txt
  [1]

  $ dune build --root=a @test
  Entering directory 'a'
  File "dune", line 1, characters 0-78:
  1 | (rule
  2 |  (alias test)
  3 |  (action (with-stdin-from "../external.txt" (run cat -))))
  Error: File unavailable: _build/external.txt
  [1]

  $ cat >external.txt <<EOF
  > txt2
  > EOF

  $ dune build --root=a @test
  Entering directory 'a'
  File "dune", line 1, characters 0-78:
  1 | (rule
  2 |  (alias test)
  3 |  (action (with-stdin-from "../external.txt" (run cat -))))
  Error: File unavailable: _build/external.txt
  [1]

# Test relative 1 level below
  $ cat >a/dune <<EOF
  > (rule
  >  (alias test)
  >  (action (with-stdin-from "../../external.txt" (run cat -))))
  > EOF

  $ cat >external.txt <<EOF
  > txt1
  > EOF

  $ dune build --root=a @test
  Entering directory 'a'
  File "dune", line 3, characters 26-46:
  3 |  (action (with-stdin-from "../../external.txt" (run cat -))))
                                ^^^^^^^^^^^^^^^^^^^^
  Error: path outside the workspace: ../../external.txt from default
  [1]

  $ dune build --root=a @test
  Entering directory 'a'
  File "dune", line 3, characters 26-46:
  3 |  (action (with-stdin-from "../../external.txt" (run cat -))))
                                ^^^^^^^^^^^^^^^^^^^^
  Error: path outside the workspace: ../../external.txt from default
  [1]

  $ cat >external.txt <<EOF
  > txt2
  > EOF

  $ dune build --root=a @test
  Entering directory 'a'
  File "dune", line 3, characters 26-46:
  3 |  (action (with-stdin-from "../../external.txt" (run cat -))))
                                ^^^^^^^^^^^^^^^^^^^^
  Error: path outside the workspace: ../../external.txt from default
  [1]

# Test absolute
  $ cat >a/dune <<EOF
  > (rule
  >  (alias test)
  >  (action (with-stdin-from "$(pwd)/external.txt" (run cat -))))
  > EOF

  $ cat >external.txt <<EOF
  > txt1
  > EOF

  $ dune build --root=a @test
  Entering directory 'a'
           cat alias test
  txt1

  $ dune build --root=a @test
  Entering directory 'a'

  $ cat >external.txt <<EOF
  > txt2
  > EOF

  $ dune build --root=a @test
  Entering directory 'a'
           cat alias test
  txt2

# Test copy files 1 level below
  $ cat >a/dune <<EOF
  > (rule
  >  (alias test)
  >  (action (with-stdin-from "external.txt" (run cat -))))
  > (copy_files ../external.txt)
  > EOF

  $ cat >external.txt <<EOF
  > txt1
  > EOF

  $ dune build --root=a @test
  Entering directory 'a'
  File "dune", line 4, characters 12-27:
  4 | (copy_files ../external.txt)
                  ^^^^^^^^^^^^^^^
  Error: path outside the workspace: ../external.txt from .
  [1]

  $ dune build --root=a @test
  Entering directory 'a'
  File "dune", line 4, characters 12-27:
  4 | (copy_files ../external.txt)
                  ^^^^^^^^^^^^^^^
  Error: path outside the workspace: ../external.txt from .
  [1]

  $ cat >external.txt <<EOF
  > txt2
  > EOF

  $ dune build --root=a @test
  Entering directory 'a'
  File "dune", line 4, characters 12-27:
  4 | (copy_files ../external.txt)
                  ^^^^^^^^^^^^^^^
  Error: path outside the workspace: ../external.txt from .
  [1]

# Test copy files absolute
  $ cat >a/dune <<EOF
  > (rule
  >  (alias test)
  >  (action (with-stdin-from "external.txt" (run cat -))))
  > (copy_files "$(pwd)/external.txt")
  > EOF

  $ cat >external.txt <<EOF
  > txt1
  > EOF

  $ dune build --root=a @test
  Entering directory 'a'
           cat alias test
  txt1

  $ dune build --root=a @test
  Entering directory 'a'

  $ cat >external.txt <<EOF
  > txt2
  > EOF

  $ dune build --root=a @test
  Entering directory 'a'
           cat alias test
  txt2
