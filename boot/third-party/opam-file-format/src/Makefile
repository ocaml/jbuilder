all: opam-file-format.cmxa opam-file-format.cma

%.ml: %.mll
	ocamllex $<

%.ml %.mli: %.mly
	ocamlyacc $<

%.cmi: %.mli
	ocamlc -c $<

%.cmo: %.ml %.cmi
	ocamlc -c $<

%.cmx: %.ml %.cmi
	ocamlopt -c $<

MODULES = $(sort $(basename $(wildcard *.ml *.mll *.mly)))

GENERATED = $(patsubst %.mly,%.ml,$(wildcard *.mly)) \
            $(patsubst %.mly,%.mli,$(wildcard *.mly)) \
            $(patsubst %.mll,%.ml,$(wildcard *.mll)) \
	    META

opam-file-format.cma: $(MODULES:%=%.cmo)
	ocamlc -a $^ -o $@

opam-file-format.cmxa: $(MODULES:%=%.cmx)
	ocamlopt -a $^ -o $@

opam-file-format.cmxs: $(MODULES:%=%.cmx)
	ocamlopt -shared $^ -o $@

META: META.in ../opam-file-format.opam $(wildcard *.cm*)
	sed -e 's/VERSION/$(shell sed -ne "s/^version: //p" ../opam-file-format.opam)/' \
	    $(if $(wildcard *.cmx*),,-e '/archive(native)/d') \
	    $(if $(wildcard *.cma),,-e '/archive(byte)/d') $< > $@

.PHONY: clean
clean:
	rm -f *.cm* *.o *.a $(GENERATED) .depend .merlin

.depend: *.ml *.mli $(GENERATED)
	ocamldep *.mli *.ml > .depend

-include .depend
