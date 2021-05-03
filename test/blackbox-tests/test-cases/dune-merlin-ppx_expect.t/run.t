  $ dune build

Expected behavior with Merlin >= 4.2
  $ ocamlmerlin single errors -filename lib/foo.ml < lib/foo.ml | jq '.value'
  [
    {
      "start": {
        "line": 0,
        "col": -1
      },
      "end": {
        "line": 0,
        "col": -1
      },
      "type": "typer",
      "sub": [],
      "valid": true,
      "message": "I/O error: foo.ml: No such file or\ndirectory"
    }
  ]
