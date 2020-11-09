type regexp = Core.re

type flag = [ `CASELESS | `MULTILINE | `ANCHORED ]

type groups = Core.Group.t

(** Result of a {!Pcre.full_split} *)
type split_result =
  | Text  of string       (** Text part of splitted string *)
  | Delim of string       (** Delimiter part of splitted string *)
  | Group of int * string (** Subgroup of matched delimiter (subgroup_nr, subgroup_str) *)
  | NoGroup               (** Unmatched subgroup *)

val re : ?flags:(flag list) -> string -> Core.t
(** [re ~flags s] creates the regexp [s] using the pcre syntax. *)

val regexp : ?flags:(flag list) -> string -> regexp
(** [re ~flags s] compiles the regexp [s] using the pcre syntax. *)

val extract : rex:regexp -> string -> string array
(** [extract ~rex s] executes [rex] on [s] and returns the matching groups. *)

val exec : rex:regexp -> ?pos:int -> string -> groups
(** Equivalent to {!Core.exec}. *)

val get_substring : groups -> int -> string
(** Equivalent to {!Core.Group.get}. *)

val get_substring_ofs : groups -> int -> int * int
(** Equivalent to {!Core.Group.offset}. *)

val pmatch : rex:regexp -> string -> bool
(** Equivalent to {!Core.execp}. *)

val substitute : rex:Core.re -> subst:(string -> string) -> string -> string

val full_split : ?max:int -> rex:regexp -> string -> split_result list

val split : rex:regexp -> string -> string list

val quote : string -> string

(** {2 Deprecated} *)

type substrings = Group.t
