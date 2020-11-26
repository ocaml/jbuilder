An action with source dependencies that are generated outside of dune
should work if wrapped in (no-infer ...) but not otherwise.

  $ cat > dune-project << EOF
  > (lang dune 2.6)
  > EOF

  $ cat >dune <<EOF
  > (rule
  >  (targets target)
  >  (action
  >   (progn
  >    (run touch source)
  >    (copy source target))))
  > EOF

  $ dune build --root .
  File "dune", line 1, characters 0-90:
  1 | (rule
  2 |  (targets target)
  3 |  (action
  4 |   (progn
  5 |    (run touch source)
  6 |    (copy source target))))
  Error: No rule found for source
  May I interest you in one of the following targets instead?
  - default/dune
  - default/dune-project
  - default/run.t
  - default/target
  [1]

  $ cat >dune <<EOF
  > (rule
  >  (targets target)
  >  (action
  >   (no-infer
  >    (progn
  >     (run touch source)
  >     (copy source target)))))
  > EOF

  $ dune build --root .
