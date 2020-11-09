INSTALL_ARGS := $(if $(PREFIX),--prefix $(PREFIX),)

default:
	dune runtest

test:
	dune runtest

install:
	dune install $(INSTALL_ARGS)

uninstall:
	dune uninstall $(INSTALL_ARGS)

reinstall: uninstall install

clean:
	dune clean

all-supported-ocaml-versions:
	dune build @install @runtest --workspace dune-workspace.dev

.PHONY: default install uninstall reinstall clean test
