(** Provide locations information *)

module Location : sig
  type t = string
end

val site :
     package:string
  -> section:Dune_section.t
  -> suffix:string
  -> encoded:string
  -> Location.t list

val relocatable : bool Lazy.t

val relocatable_prefix : string Lazy.t

val ocamlpath : string list Lazy.t

val sourceroot : string -> string option

val stdlib : string Lazy.t

val path_sep : char

module Hardcoded_ocaml_path : sig
  type t =
    | None
    | Relocatable
    | Hardcoded of string list
    | Findlib_config of string

  val t : t Lazy.t
end
