open! Dune_engine
open! Stdune
open Import

module Op : sig
  type t =
    | Eq
    | Gt
    | Gte
    | Lte
    | Lt
    | Neq
end

type t =
  | Const of bool
  | Expr of String_with_vars.t
  | And of t list
  | Or of t list
  | Compare of Op.t * String_with_vars.t * String_with_vars.t

val true_ : t

val eval : t -> dir:Path.t -> f:Value.t list String_with_vars.expander -> bool

val eval_partial :
     t
  -> dir:Path.t
  -> f:Value.t list Action_builder.t String_with_vars.expander
  -> bool Action_builder.t

val to_dyn : t -> Dyn.t

val decode : t Dune_lang.Decoder.t

(** Resolve variables manually. For complex cases such as [enabled_if] *)
val decode_manually :
     (Pform.Env.t -> Dune_lang.Template.Pform.t -> Pform.t)
  -> t Dune_lang.Decoder.t
