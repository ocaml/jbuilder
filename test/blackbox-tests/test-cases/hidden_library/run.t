  $ dune build


  $ dune exec -- dune exec --root sub ./b.exe
  Entering directory 'sub'
  Entering directory 'sub'
  With a library A A


  $ dune exec -- dune exec --root sub --workspace sub/dune-workspace.without_a_opt ./b.exe
  Entering directory 'sub'
  Entering directory 'sub'
  Without a library A
