open! Dune_engine
open Stdune

module Select : sig
  module Choice : sig
    type t =
      { required : Lib_name.Set.t
      ; forbidden : Lib_name.Set.t
      ; file : string
      }
  end

  type t =
    { result_fn : string
    ; choices : Choice.t list
    ; loc : Loc.t
    }

  val to_dyn : t -> Dyn.t
end

type t =
  | Direct of (Loc.t * Lib_name.t)
  | Re_export of (Loc.t * Lib_name.t)
  | Select of Select.t
  | NonPP of Lib_name.t list

val to_dyn : t -> Dyn.t

val direct : Loc.t * Lib_name.t -> t

val re_export : Loc.t * Lib_name.t -> t

val non_pp : Lib_name.t list -> t

val decode : allow_re_export:bool -> t Dune_lang.Decoder.t

module L : sig
  val field_encode : t list -> name:string -> Dune_lang.Encoder.field
end
