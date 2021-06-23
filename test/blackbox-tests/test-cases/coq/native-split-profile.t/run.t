  $ dune build --profile=dev --display short --debug-dependency-path @all
        coqdep bar/bar.v.d
        coqdep foo/foo.v.d
          coqc foo/.foo.aux,foo/foo.{glob,vo}
     coqnative foo/Nfoo_foo.{cmi,cmxs}
          coqc bar/.bar.aux,bar/bar.{glob,vo}

  $ dune build --profile=custom --display short --debug-dependency-path @all
          coqc foo/.foo.aux,foo/foo.{glob,vo}
     coqnative foo/Nfoo_foo.{cmi,cmxs}
          coqc bar/.bar.aux,bar/bar.{glob,vo}
     coqnative bar/Nbar_bar.{cmi,cmxs}

  $ dune build --profile=release --display short --debug-dependency-path @all
          coqc foo/.foo.aux,foo/foo.{glob,vo}
     coqnative foo/Nfoo_foo.{cmi,cmxs}
          coqc bar/.bar.aux,bar/bar.{glob,vo}
     coqnative bar/Nbar_bar.{cmi,cmxs}

  $ dune build --profile=release --debug-dependency-path @default
  lib: [
    "_build/install/default/lib/base/META"
    "_build/install/default/lib/base/dune-package"
    "_build/install/default/lib/base/opam"
  ]
  lib_root: [
    "_build/install/default/lib/coq/user-contrib/bar/.coq-native/Nbar_bar.cmi" {"coq/user-contrib/bar/.coq-native/Nbar_bar.cmi"}
    "_build/install/default/lib/coq/user-contrib/bar/.coq-native/Nbar_bar.cmxs" {"coq/user-contrib/bar/.coq-native/Nbar_bar.cmxs"}
    "_build/install/default/lib/coq/user-contrib/bar/bar.v" {"coq/user-contrib/bar/bar.v"}
    "_build/install/default/lib/coq/user-contrib/bar/bar.vo" {"coq/user-contrib/bar/bar.vo"}
    "_build/install/default/lib/coq/user-contrib/foo/.coq-native/Nfoo_foo.cmi" {"coq/user-contrib/foo/.coq-native/Nfoo_foo.cmi"}
    "_build/install/default/lib/coq/user-contrib/foo/.coq-native/Nfoo_foo.cmxs" {"coq/user-contrib/foo/.coq-native/Nfoo_foo.cmxs"}
    "_build/install/default/lib/coq/user-contrib/foo/foo.v" {"coq/user-contrib/foo/foo.v"}
    "_build/install/default/lib/coq/user-contrib/foo/foo.vo" {"coq/user-contrib/foo/foo.vo"}
  ]
