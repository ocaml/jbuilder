Some tests about dune and camlp5

For now dune relies on camlp5 8.x and a tool `mkcamlp5.opt` which is shipped with it

Before running tests you need to run:

  ln -s `realpath camlp5-dune-package` `ocamlfind query camlp5`/dune-package -f

To avoid it we can parse camlp5 preprocessing commands from META file but
for now it is not done even for PPX and I postponed it
