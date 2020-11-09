(** Categories represent the various kinds of characters that can be tested
    by look-ahead and look-behind operations.

    This is more restricted than Cset, but faster.
*)

type t
val (++) : t -> t -> t
val from_char : char -> t

val dummy : t
val inexistant : t
val letter : t
val not_letter : t
val newline : t
val lastnewline : t
val search_boundary : t
val to_int : t -> int
val equal : t -> t -> bool
val compare : t -> t -> int

val intersect : t -> t -> bool


val pp : Format.formatter -> t -> unit
