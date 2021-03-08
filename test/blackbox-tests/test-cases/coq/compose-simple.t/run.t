  $ dune build --display short --debug-dependency-path
        coqdep a/a.v.d
        coqdep b/b.v.d
          coqc a/.a.aux,a/a.{glob,vo}
          coqc b/.b.aux,b/b.{glob,vo}
          coqc a/a.vos (exit 1)
  (cd _build/default && /Users/rgrinberg/github/ocaml/dune/_opam/bin/coqc -q -w -native-compiler-disabled -native-compiler ondemand -R a a a/a.v)
  Error: System error: "./a/.a.aux: Permission denied"
  
  -> required by a/a.vos
  -> required by install lib/coq/user-contrib/a/a.vos
  -> required by csimple.install
  -> required by alias default
  -> required by alias default
  [1]
