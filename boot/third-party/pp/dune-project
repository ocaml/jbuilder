(lang dune 2.0)
(name pp)

(license MIT)
(maintainers "Jeremie Dimino <jeremie@dimino.org>")
(authors
  "Jane Street Group, LLC <opensource@janestreet.com>"
  "Jeremie Dimino <jeremie@dimino.org>")
(source (github diml/pp))
(documentation "https://diml.github.io/pp/")

(generate_opam_files true)

(package
 (name pp)
 (depends
   (ocaml (>= 4.04.0))
   (ppx_expect :with-test))
 (synopsis "Pretty-printing library")
 (description "
This library provides a lean alternative to the Format [1] module of
the OCaml standard library. It aims to make it easy for users to do
the right thing. If you have tried Format before but find its API
complicated and difficult to use, then Pp might be a good choice for
you.

Pp uses the same concepts of boxes and break hints, and the final
rendering is done to formatter from the Format module. However it
defines its own algebra which some might find easier to work with and
reason about. No previous knowledge is required to start using this
library, however the various guides for the Format module such as this
one [2] should be applicable to Pp as well.

[1]: https://caml.inria.fr/pub/docs/manual-ocaml/libref/Format.html
[2]: http://caml.inria.fr/resources/doc/guides/format.en.html
"))
