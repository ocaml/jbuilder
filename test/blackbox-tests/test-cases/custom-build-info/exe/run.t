  $ (git init -q;
  >    git add .;
  >    git commit -q -m _;
  >    git tag -a 1.0 -m _)

  $ dune build

  $ dune install --prefix _install
  Installing _install/lib/foo/META
  Installing _install/lib/foo/dune-package
  Installing _install/lib/foo/opam
  Installing _install/bin/foo

  $ _install/bin/foo
  version: 1.0
  custom: Some custom information accessible with `Build_info.V2.custom`
