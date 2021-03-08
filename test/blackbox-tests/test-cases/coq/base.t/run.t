  $ dune build --display short --debug-dependency-path @all
        coqdep bar.v.d
        coqdep foo.v.d
          coqc .foo.aux,foo.{glob,vo}
          coqc foo.vos
          coqc .bar.aux,bar.{glob,vo}
          coqc bar.vos

  $ dune build --debug-dependency-path @default
  lib: [
    "_build/install/default/lib/base/META"
    "_build/install/default/lib/base/dune-package"
    "_build/install/default/lib/base/opam"
  ]
  lib_root: [
    "_build/install/default/lib/coq/user-contrib/basic/bar.v" {"coq/user-contrib/basic/bar.v"}
    "_build/install/default/lib/coq/user-contrib/basic/bar.vo" {"coq/user-contrib/basic/bar.vo"}
    "_build/install/default/lib/coq/user-contrib/basic/bar.vos" {"coq/user-contrib/basic/bar.vos"}
    "_build/install/default/lib/coq/user-contrib/basic/foo.v" {"coq/user-contrib/basic/foo.v"}
    "_build/install/default/lib/coq/user-contrib/basic/foo.vo" {"coq/user-contrib/basic/foo.vo"}
    "_build/install/default/lib/coq/user-contrib/basic/foo.vos" {"coq/user-contrib/basic/foo.vos"}
  ]
