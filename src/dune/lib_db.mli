(** A [Lib_db.t] is only responsible for resolving library names into their
    location and metadata *)
open Stdune

type t

module Info : sig
  (** Resolved libraries *)
  type t

  val equal : t -> t -> bool

  val info : t -> Lib_info.external_

  module Set : Set.S with type elt = t
end

module Resolve_result : sig
  type nonrec 'a t =
    | Not_found
    | Found of 'a
    | Hidden of
        { info : 'a
        ; reason : string
        }
    | Redirect of t option * (Loc.t * Lib_name.t)

  val to_dyn : ('a -> Dyn.t) -> 'a t -> Dyn.t
end

(** Create a new library database. [resolve] is used to resolve library names in
    this database.

    When a library is not found, it is looked up in the parent database if any.

    [all] returns the list of names of libraries available in this database. *)
val create :
     ?parent:t
  -> stdlib_dir:Path.t
  -> resolve:(Lib_name.t -> Lib_info.external_ Resolve_result.t)
  -> all:(unit -> Lib_name.t list)
  -> unit
  -> t

module Library_related_stanza : sig
  type t =
    | Library of Path.Build.t * Dune_file.Library.t
    | External_variant of Dune_file.External_variant.t
    | Deprecated_library_name of Dune_file.Deprecated_library_name.t
end

(** Create a database from a list of library/variants stanzas *)
val create_from_stanzas :
  ?parent:t -> lib_config:Lib_config.t -> Library_related_stanza.t list -> t

val create_from_findlib :
  ?external_lib_deps_mode:bool -> stdlib_dir:Path.t -> Findlib.t -> t

(** Return the list of all libraries in this database. If [recursive] is true,
    also include libraries in parent databases recursively. *)
val all : ?recursive:bool -> t -> Info.Set.t

val resolve : t -> Loc.t * Lib_name.t -> Info.t Or_exn.t
