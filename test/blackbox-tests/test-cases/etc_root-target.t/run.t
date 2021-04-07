Test etc_root install target
============================

  $ cat >dune <<EOF
  > (install
  >  (section etc_root)
  >  (files (bar as foo/bar)))
  > EOF

  $ cat >dune-project <<EOF
  > (lang dune 2.8)
  > (package (name x))
  > EOF

  $ touch bar

  $ dune build @install

  $ cat _build/default/x.install
  lib: [
    "_build/install/default/lib/x/META"
    "_build/install/default/lib/x/dune-package"
  ]
  etc_root: [
    "_build/install/default/etc/foo/bar" {"foo/bar"}
  ]
