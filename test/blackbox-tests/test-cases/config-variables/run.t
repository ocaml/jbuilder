Variables can be listed.

  $ dune list-variables
  another_variable:
      Doc for another_variable
      (default: default value for another_variable)
  variable_name:
      Doc for variable_name
      (default: default value for variable_name)

With no configuration, they evaluate to their default value.

  $ dune build @print-variable
  default value for variable_name

Referring to unknown variables produces an error.

  $ dune build @print-unknown-variable
  File "dune", line 17, characters 17-38:
  17 |  (action (echo %{var:unknown_variable})))
                        ^^^^^^^^^^^^^^^^^^^^^
  Error: Unknown macro %{var:..}
  [1]
