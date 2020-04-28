open Stdune

type t

val hash : t -> int

include Stringlike_intf.S with type t := t

module Local : sig
  include Stringlike_intf.S

  (** Description of valid library names *)
  val valid_format_doc : User_message.Style.t Pp.t
end

val compare : t -> t -> Ordering.t

val equal : t -> t -> bool

val of_local : Loc.t * Local.t -> t

val to_local : t -> Local.t option

val split : t -> Package.Name.t * string list

val package_name : t -> Package.Name.t

val of_package_name : Package.Name.t -> t

module Map : Map.S with type key = t

module Set : sig
  include Set.S with type elt = t

  val to_string_list : t -> string list
end

val nest : t -> t -> t

val is_valid_module_name: ?with_hyphen:bool -> string -> bool
