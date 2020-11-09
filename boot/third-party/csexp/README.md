Csexp - Canonical S-expressions
===============================

This project provides minimal support for parsing and printing
[S-expressions in canonical form][wikipedia], which is a very simple
and canonical binary encoding of S-expressions.

[wikipedia]: https://en.wikipedia.org/wiki/Canonical_S-expressions

Example
-------

```ocaml
# #require "csexp";;
# module Sexp = struct type t = Atom of string | List of t list end;;
module Sexp : sig type t = Atom of string | List of t list end
# module Csexp = Csexp.Make(Sexp);;
module Csexp :
  sig
    val parse_string : string -> (Sexp.t, int * string) result
    val parse_string_many : string -> (Sexp.t list, int * string) result
    val input : in_channel -> (Sexp.t, string) result
    val input_opt : in_channel -> (Sexp.t option, string) result
    val input_many : in_channel -> (Sexp.t list, string) result
    val serialised_length : Sexp.t -> int
    val to_string : Sexp.t -> string
    val to_buffer : Buffer.t -> Sexp.t -> unit
    val to_channel : out_channel -> Sexp.t -> unit
  end
# Csexp.to_string (List [ Atom "Hello"; Atom "world!" ]);;
- : string = "(5:Hello6:world!)"
```

