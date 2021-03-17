open! Dune_engine

module Ppx_args : sig
  module Cookie : sig
    type t =
      { name : string
      ; value : String_with_vars.t
      }
  end

  type t = { cookies : Cookie.t list }

  val empty : t
end

type t =
  | Normal
  | Ppx_deriver of Ppx_args.t
  | Ppx_rewriter of Ppx_args.t
  | Camlp5_rewriter

val to_dyn : t Stdune.Dyn.Encoder.t

include Dune_lang.Conv.S with type t := t
