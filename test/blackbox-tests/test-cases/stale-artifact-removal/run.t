
  $ cat > dune <<EOF
  > (library (name lib))
  > EOF

  $ cat > a.ml <<EOF
  > let _ = List.map
  > EOF

  $ mkdir -p _build/default/unrelated-dir
  $ test -d _build/default/unrelated-dir
  $ touch _build/default/unrelated-dir/unrelated-file

  $ mkdir -p _build/default/.lib.objs/native
  $ touch _build/default/.lib.objs/native/list.cmi
  $ dune build _build/default/.lib.objs/lib.cmxa

Dune deletes the [list.cmi] we created.
If it didn't do so, the compiler would have tried to use
it and fail with: "Corrupted compiled interface.

  $ ls _build/default/.lib.objs/native | grep list
  [1]

Dune also deletes the stale directory, but it's less clear what
relies on that. One benefit is that disk space is reclaimed.

  $ test -d _build/default/unrelated-dir
  [1]
