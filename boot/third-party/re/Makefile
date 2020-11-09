DUNE ?= dune

all:
	@$(DUNE) build

test:
	@$(DUNE) runtest

check: test

clean:
	@$(DUNE) clean

.PHONY: check test all clean

.PHONY: all-supported-ocaml-versions
all-supported-ocaml-versions:
	dune build @runtest --workspace dune-workspace.dev
