  $ dune build --profile=release --display short --debug-dependency-path @all
        coqdep bar/bar.v.d
        coqdep foo/foo.v.d
        coqdep baz/a/a.v.d
        coqdep baz/b/uu.v.d
          coqc foo/.foo.aux,foo/foo.{glob,vo}
          coqc baz/a/.a.aux,baz/a/a.{glob,vo}
          coqc baz/b/.uu.aux,baz/b/uu.{glob,vo}
     coqnative foo/Nfoo_foo.{cmi,cmxs}
          coqc bar/.bar.aux,bar/bar.{glob,vo}
     coqnative baz/a/Nmoo_baz_a_a.{cmi,cmxs}
     coqnative baz/b/Nmoo_baz_b_uu.{cmi,cmxs}
     coqnative bar/Nbar_bar.{cmi,cmxs}

  $ dune build --profile=release --debug-dependency-path @default
  lib: [
    "_build/install/default/lib/base/META"
    "_build/install/default/lib/base/dune-package"
  ]
  lib_root: [
    "_build/install/default/lib/coq/user-contrib/bar/bar.v" {"coq/user-contrib/bar/bar.v"}
    "_build/install/default/lib/coq/user-contrib/bar/bar.vo" {"coq/user-contrib/bar/bar.vo"}
    "_build/install/default/lib/coq/user-contrib/foo/.coq-native/Nfoo_foo.cmi" {"coq/user-contrib/foo/.coq-native/Nfoo_foo.cmi"}
    "_build/install/default/lib/coq/user-contrib/foo/.coq-native/Nfoo_foo.cmxs" {"coq/user-contrib/foo/.coq-native/Nfoo_foo.cmxs"}
    "_build/install/default/lib/coq/user-contrib/foo/foo.v" {"coq/user-contrib/foo/foo.v"}
    "_build/install/default/lib/coq/user-contrib/foo/foo.vo" {"coq/user-contrib/foo/foo.vo"}
    "_build/install/default/lib/coq/user-contrib/moo/baz/a/.coq-native/Nmoo_baz_a_a.cmi" {"coq/user-contrib/moo/baz/a/.coq-native/Nmoo_baz_a_a.cmi"}
    "_build/install/default/lib/coq/user-contrib/moo/baz/a/.coq-native/Nmoo_baz_a_a.cmxs" {"coq/user-contrib/moo/baz/a/.coq-native/Nmoo_baz_a_a.cmxs"}
    "_build/install/default/lib/coq/user-contrib/moo/baz/a/a.v" {"coq/user-contrib/moo/baz/a/a.v"}
    "_build/install/default/lib/coq/user-contrib/moo/baz/a/a.vo" {"coq/user-contrib/moo/baz/a/a.vo"}
    "_build/install/default/lib/coq/user-contrib/moo/baz/b/.coq-native/Nmoo_baz_b_uu.cmi" {"coq/user-contrib/moo/baz/b/.coq-native/Nmoo_baz_b_uu.cmi"}
    "_build/install/default/lib/coq/user-contrib/moo/baz/b/.coq-native/Nmoo_baz_b_uu.cmxs" {"coq/user-contrib/moo/baz/b/.coq-native/Nmoo_baz_b_uu.cmxs"}
    "_build/install/default/lib/coq/user-contrib/moo/baz/b/uu.v" {"coq/user-contrib/moo/baz/b/uu.v"}
    "_build/install/default/lib/coq/user-contrib/moo/baz/b/uu.vo" {"coq/user-contrib/moo/baz/b/uu.vo"}
  ]
  lib: [
    "_build/install/default/lib/base-native/META"
    "_build/install/default/lib/base-native/dune-package"
  ]
  lib_root: [
    "_build/install/default/lib/coq/user-contrib/bar/.coq-native/Nbar_bar.cmi" {"coq/user-contrib/bar/.coq-native/Nbar_bar.cmi"}
    "_build/install/default/lib/coq/user-contrib/bar/.coq-native/Nbar_bar.cmxs" {"coq/user-contrib/bar/.coq-native/Nbar_bar.cmxs"}
  ]
