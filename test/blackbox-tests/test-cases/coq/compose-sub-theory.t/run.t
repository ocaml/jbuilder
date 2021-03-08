  $ dune build --display short --debug-dependency-path
        coqdep b/b.v.d
        coqdep a/a.v.d
          coqc a/.a.aux,a/a.{glob,vo}
          coqc b/.b.aux,b/b.{glob,vo}
          coqc a/a.vos (exit 1)
  (cd _build/default && /Users/rgrinberg/github/ocaml/dune/_opam/bin/coqc -q -w -native-compiler-disabled -native-compiler ondemand -R a foo.a a/a.v)
  Error: System error: "./a/.a.aux: Permission denied"
  
  -> required by a/a.vos
  -> required by install lib/coq/user-contrib/foo/a/a.vos
  -> required by subtheory.install
  -> required by alias default
  -> required by alias default
  [1]
