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
  custom1: Some custom information in lib1 accessible with `Build_info.V2.custom_lib "lib1"`
  custom2: Custom info from lib2
