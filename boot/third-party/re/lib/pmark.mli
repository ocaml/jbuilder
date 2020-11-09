
type t = private int
val equal : t -> t -> bool
val compare : t -> t -> int
val gen : unit -> t
val pp : Format.formatter -> t -> unit

module Set : Set.S with type elt = t
