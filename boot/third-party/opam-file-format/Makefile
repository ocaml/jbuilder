TARGETS = opam-file-format.cma opam-file-format.cmxa opam-file-format.cmxs

all: $(TARGETS)
	$(MAKE) -C src META

byte: $(filter %.cma,$(TARGETS))
	$(MAKE) -C src META

native: $(filter %.cmxa,$(TARGETS))
	$(MAKE) -C src META

.PHONY: dune clean
dune:
	dune build --profile=dev @all

%:
	$(MAKE) -C src $@

tests: opam-file-format.cmxa
	$(MAKE) -C tests run

PREFIX ?= /usr/local
LIBDIR ?= $(PREFIX)/lib

install:
	mkdir -p $(DESTDIR)$(LIBDIR)/opam-file-format
	install -m 0644 \
	  $(wildcard $(addprefix src/*.,cmi cmo cmx cmti lib a cma cmxa cmxs)) \
	    src/META \
	  $(DESTDIR)$(LIBDIR)/opam-file-format/

uninstall:
	rm -f $(DESTDIR)$(LIBDIR)/opam-file-format/*
	rmdir $(DESTDIR)$(LIBDIR)/opam-file-format

clean::
	rm -rf _build
	$(MAKE) -C src $@
	$(MAKE) -C tests $@
