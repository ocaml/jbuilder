(** This module provides parsing for the [(custom-build-info)] field of the
    library and executable stanzas *)
open! Stdune

type t =
  { max_size : int
  ; action : Loc.t * Action_dune_lang.t
  }

val output_file : string -> string

val decode :
  unit -> (t option, Dune_lang.Decoder.fields) Dune_lang.Decoder.parser

val to_dyn : t -> Dyn.t

val encode : t -> Dune_lang.t
