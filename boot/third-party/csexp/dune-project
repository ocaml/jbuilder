(lang dune 1.11)
(name csexp)

(allow_approximate_merlin)

(license MIT)
(maintainers "Jeremie Dimino <jeremie@dimino.org>")
(authors
  "Quentin Hocquet <mefyl@gruntech.org>"
  "Jane Street Group, LLC <opensource@janestreet.com>"
  "Jeremie Dimino <jeremie@dimino.org>")
(source (github ocaml-dune/csexp))
(documentation "https://ocaml-dune.github.io/csexp/")

(generate_opam_files true)

(package
 (name csexp)
 (depends
   (ocaml (>= 4.02.3))
;  (ppx_expect :with-test)
; Disabled because of a dependency cycle 
; (see https://github.com/ocaml-opam/opam-depext/issues/121)
   (result (>= 1.5)))
 (synopsis "Parsing and printing of S-expressions in Canonical form")
 (description "
This library provides minimal support for Canonical S-expressions
[1]. Canonical S-expressions are a binary encoding of S-expressions
that is super simple and well suited for communication between
programs.

This library only provides a few helpers for simple applications. If
you need more advanced support, such as parsing from more fancy input
sources, you should consider copying the code of this library given
how simple parsing S-expressions in canonical form is.

To avoid a dependency on a particular S-expression library, the only
module of this library is parameterised by the type of S-expressions.

[1] https://en.wikipedia.org/wiki/Canonical_S-expressions
"))
