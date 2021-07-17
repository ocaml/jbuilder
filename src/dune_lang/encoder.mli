(** Producing dune lang values from OCaml values *)

include Combinators.S with type 'a t = 'a -> T.t

val sexp : T.t t

val record : (string * T.t) list -> T.t

val constr : string -> 'a t -> 'a t

type field

val field :
  string -> 'a t -> ?equal:('a -> 'a -> bool) -> ?default:'a -> 'a -> field

val field_o : string -> 'a t -> 'a option -> field

val field_b : string -> bool -> field

(** Field with inlined list as value

    The field is left absent if the list is empty. *)
val field_l : string -> 'a t -> 'a list -> field

(** Same as [field_l] but to represent a single value *)
val field_i : string -> ('a -> T.t list) -> 'a -> field

val record_fields : field list -> T.t list

val unknown : _ t
