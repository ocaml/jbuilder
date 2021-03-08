  $ dune build --display short --debug-dependency-path @all
        coqdep a/bar.v.d
        coqdep b/foo.v.d
        coqdep c/d/bar.v.d
        coqdep c/ooo.v.d
          coqc b/.foo.aux,b/foo.{glob,vo}
          coqc b/foo.vos
          coqc c/d/.bar.aux,c/d/bar.{glob,vo}
          coqc c/d/bar.vos
          coqc c/.ooo.aux,c/ooo.{glob,vo}
          coqc c/ooo.vos
          coqc a/.bar.aux,a/bar.{glob,vo}
          coqc a/bar.vos

  $ dune build --debug-dependency-path @default
  lib: [
    "_build/install/default/lib/rec/META"
    "_build/install/default/lib/rec/dune-package"
    "_build/install/default/lib/rec/opam"
  ]
  lib_root: [
    "_build/install/default/lib/coq/user-contrib/rec_module/a/bar.v" {"coq/user-contrib/rec_module/a/bar.v"}
    "_build/install/default/lib/coq/user-contrib/rec_module/a/bar.vo" {"coq/user-contrib/rec_module/a/bar.vo"}
    "_build/install/default/lib/coq/user-contrib/rec_module/a/bar.vos" {"coq/user-contrib/rec_module/a/bar.vos"}
    "_build/install/default/lib/coq/user-contrib/rec_module/b/foo.v" {"coq/user-contrib/rec_module/b/foo.v"}
    "_build/install/default/lib/coq/user-contrib/rec_module/b/foo.vo" {"coq/user-contrib/rec_module/b/foo.vo"}
    "_build/install/default/lib/coq/user-contrib/rec_module/b/foo.vos" {"coq/user-contrib/rec_module/b/foo.vos"}
    "_build/install/default/lib/coq/user-contrib/rec_module/c/d/bar.v" {"coq/user-contrib/rec_module/c/d/bar.v"}
    "_build/install/default/lib/coq/user-contrib/rec_module/c/d/bar.vo" {"coq/user-contrib/rec_module/c/d/bar.vo"}
    "_build/install/default/lib/coq/user-contrib/rec_module/c/d/bar.vos" {"coq/user-contrib/rec_module/c/d/bar.vos"}
    "_build/install/default/lib/coq/user-contrib/rec_module/c/ooo.v" {"coq/user-contrib/rec_module/c/ooo.v"}
    "_build/install/default/lib/coq/user-contrib/rec_module/c/ooo.vo" {"coq/user-contrib/rec_module/c/ooo.vo"}
    "_build/install/default/lib/coq/user-contrib/rec_module/c/ooo.vos" {"coq/user-contrib/rec_module/c/ooo.vos"}
  ]
