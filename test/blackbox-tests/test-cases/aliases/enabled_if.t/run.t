# Without enabled_if the rule is ran even without the library

  $ cat >dune-project <<EOF
  > (lang dune 2.6)
  > EOF

  $ cat >dune <<EOF
  > (rule
  >   (alias bar)
  >   (deps (universe))
  >   (action (run echo %{lib-available:mylib}))
  > )
  > 
  > (alias
  >   (name foo)
  >   (deps (alias bar))
  > )
  > EOF

  $ dune build @foo
          echo alias bar
  false

# Extended enabled_if before 2.9 is an error
  $ cat >dune <<EOF
  > (rule
  >   (alias bar)
  >   (deps (universe))
  >   (action (run echo %{lib-available:mylib}))
  > )
  > 
  > (alias
  >   (name foo)
  >   (deps (alias bar))
  >   (enabled_if %{lib-available:mylib})
  > )
  > EOF

  $ dune build @foo
  File "dune", line 7, characters 0-80:
   7 | (alias
   8 |   (name foo)
   9 |   (deps (alias bar))
  10 |   (enabled_if %{lib-available:mylib})
  11 | )
  Error: Extended enabled_if is not compatible with dune lang < (2, 9).
  [1]

# If the library is not present the rule is not executed
# but lib-available is known without additional dependencies

  $ cat >dune-project <<EOF
  > (lang dune 2.9)
  > EOF

  $ dune build @foo

# If the library is present the rule is executed

  $ cat >dune <<EOF
  > (library
  >  (modules )
  >  (name mylib)
  > )
  > 
  > (rule
  >   (alias bar)
  >   (deps (universe))
  >   (action (run echo %{lib-available:mylib}))
  > )
  > 
  > (alias
  >   (name foo)
  >   (deps (alias bar))
  >   (enabled_if %{lib-available:mylib})
  > )
  > EOF

  $ dune build @foo
          echo alias bar
  true

# Enabled_if with a new dependency, without execution

  $ cat >dune <<EOF
  > (rule
  >   (alias bar)
  >   (deps (universe))
  >   (action (run echo executed))
  > )
  > 
  > (rule
  >   (action (with-stdout-to flag.bool (echo false)))
  > )
  > 
  > (alias
  >   (name foo)
  >   (deps (alias bar) flag.bool)
  >   (enabled_if %{read-lines:flag.bool})
  > )
  > EOF

  $ dune build @foo


# Enabled_if with a new dependency, with execution

  $ cat >dune <<EOF
  > (rule
  >   (alias bar)
  >   (deps (universe))
  >   (action (run echo executed))
  > )
  > 
  > (rule
  >   (action (with-stdout-to flag.bool (echo true)))
  > )
  > 
  > (alias
  >   (name foo)
  >   (deps (alias bar))
  >   (enabled_if %{read-lines:flag.bool})
  > )
  > EOF

  $ dune build @foo
          echo alias bar
  executed

  $ dune build @foo
          echo alias bar
  executed

# Enabled_if with a wrong value

  $ cat >dune <<EOF
  > (rule
  >   (alias bar)
  >   (deps (universe))
  >   (action (run echo executed))
  > )
  > 
  > (rule
  >   (action (with-stdout-to flag.bool (echo unknown)))
  > )
  > 
  > (alias
  >   (name foo)
  >   (deps (alias bar))
  >   (enabled_if %{read:flag.bool})
  > )
  > EOF

  $ dune build @foo
  File "dune", line 14, characters 14-31:
  14 |   (enabled_if %{read:flag.bool})
                     ^^^^^^^^^^^^^^^^^
  Error: This value must be either true or false got:
  "unknown"
  [1]

# Try to create a dependency cycle

  $ cat >dune <<EOF
  > (rule
  >  (target false.txt)
  >  (deps (alias foo))
  >  (action (write-file false.txt "false")))
  > 
  > (alias
  >  (name foo)
  >  (enabled_if %{read:false.txt}))
  > EOF

  $ dune build @foo
  Error: Dependency cycle between the following files:
     alias foo
  -> _build/default/false.txt
  -> alias foo
  [1]
