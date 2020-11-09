(lang dune 2.8)
;          ^^^
; When changing the version, don't forget to regenerate *.opam files
; by running [dune build].
(name dune)

(generate_opam_files true)

(cram enable)

; Reserved for Dune itself. This is to help with the bootstrap
(using dune-bootstrap-info 0.1)

(license MIT)
(maintainers "Jane Street Group, LLC <opensource@janestreet.com>")
(authors "Jane Street Group, LLC <opensource@janestreet.com>")
(source (github ocaml/dune))
(documentation "https://dune.readthedocs.io/")

(implicit_transitive_deps false)

(package
 (name dune)
 ; The "depends" and "build" field are written in dune.opam.template
 (conflicts
  (dune-configurator (< 2.3.0))
  (odoc (< 1.3.0))
  (dune-release (< 1.3.0))
  (js_of_ocaml-compiler (< 3.6.0))
  (jbuilder (= transition)))
 (synopsis "Fast, portable, and opinionated build system")
 (description "
dune is a build system that was designed to simplify the release of
Jane Street packages. It reads metadata from \"dune\" files following a
very simple s-expression syntax.

dune is fast, has very low-overhead, and supports parallel builds on
all platforms. It has no system dependencies; all you need to build
dune or packages using dune is OCaml. You don't need make or bash
as long as the packages themselves don't use bash explicitly.

dune supports multi-package development by simply dropping multiple
repositories into the same directory.

It also supports multi-context builds, such as building against
several opam roots/switches simultaneously. This helps maintaining
packages across several versions of OCaml and gives cross-compilation
for free.
"))

(package
 (name dune-build-info)
 (synopsis "Embed build informations inside executable")
 (description "\
The build-info library allows to access information about how the
executable was built, such as the version of the project at which it
was built or the list of statically linked libraries with their
versions.  It supports reporting the version from the version control
system during development to get an precise reference of when the
executable was built.
"))

(package
 (name dune-private-libs)
 (depends
  pp
  result
  csexp
  (ocaml (>= 4.08)))
 (synopsis "Private libraries of Dune")
 (description "\
!!!!!!!!!!!!!!!!!!!!!!
!!!!! DO NOT USE !!!!!
!!!!!!!!!!!!!!!!!!!!!!

This package contains code that is shared between various dune-xxx
packages. However, it is not meant for public consumption and provides
no stability guarantee.
"))

(package
 (name dune-configurator)
 (depends
  (ocaml (>= 4.03.0))
  result
  (csexp (>= 1.3.0))
  (dune (>= 2.6.0)))
 (synopsis "Helper library for gathering system configuration")
 (description "\
dune-configurator is a small library that helps writing OCaml scripts that
test features available on the system, in order to generate config.h
files for instance.
Among other things, dune-configurator allows one to:
- test if a C program compiles
- query pkg-config
- import #define from OCaml header files
- generate config.h file
"))

(package
 (name dune-action-plugin)
 (depends
  dune-glob
  csexp
  (ppx_expect :with-test)
  (dune-private-libs (= :version)))
 (synopsis "[experimental] API for writing dynamic Dune actions")
 (description "\

This library is experimental. No backwards compatibility is implied.

dune-action-plugin provides an API for writing dynamic Dune actions.
Dynamic dune actions do not need to declare their dependencies
upfront; they are instead discovered automatically during the
execution of the action.
"))

(package
 (name dune-glob)
 (depends
  (dune-private-libs (= :version)))
 (synopsis "Glob string matching language supported by dune")
 (description "\
dune-glob provides a parser and interpreter for globs as \
understood by dune language."))

(package
 (name dune-site)
 (depends (dune-private-libs (= :version)))
 (synopsis "Embed locations informations inside executable and libraries")
 (description ""))

(package
 (name dune-tests)
 (depends
  ; List test dependencies here
  lwt
  ocamlfind
  ppxlib
  (ppx_expect (<= v0.14))
  (odoc (>= 1.5.0))
  menhir
  ocaml-migrate-parsetree
  result
  utop
  (mdx (>= 1.6.0))
  bisect_ppx)
 (synopsis "Dummy package so that we use the duniverse with dune"))
