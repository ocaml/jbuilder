open Import

type stanza = Stanza.t = ..

module Stanza : sig
  module Jsoo_compilation : sig
    type t =
      | Separate
      | Classic

    val default : profile:string -> t
  end

  type config =
    { flags          : Ordered_set_lang.Unexpanded.t
    ; ocamlc_flags   : Ordered_set_lang.Unexpanded.t
    ; ocamlopt_flags : Ordered_set_lang.Unexpanded.t

    ; jsoo_compilation : Jsoo_compilation.t option
    }

  type pattern =
    | Profile of string
    | Any

  type t =
    { loc   : Loc.t
    ; rules : (pattern * config) list
    }

  val t : t Sexp.Of_sexp.t
end

type stanza +=
  | T of Stanza.t
