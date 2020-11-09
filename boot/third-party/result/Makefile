INSTALL_ARGS := $(if $(PREFIX),--prefix $(PREFIX),)

default:
	dune build @install

install:
	dune install $(INSTALL_ARGS)

uninstall:
	dune uninstall $(INSTALL_ARGS)

reinstall: uninstall reinstall

clean:
	dune clean

.PHONY: default install uninstall reinstall clean
