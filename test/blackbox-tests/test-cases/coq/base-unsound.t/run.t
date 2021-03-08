  $ dune build --display short --profile unsound --debug-dependency-path @all
        coqdep bar.v.d
        coqdep foo.v.d
          coqc .foo.aux,foo.{glob,vo}
          coqc .bar.aux,bar.{glob,vo}
          coqc foo.vos (exit 1)
  (cd _build/default && /Users/rgrinberg/github/ocaml/dune/_opam/bin/coqc -type-in-type -R . basic foo.v)
  Error: System error: "./.foo.aux: Permission denied"
  
  -> required by foo.vos
  -> required by alias all
  [1]
