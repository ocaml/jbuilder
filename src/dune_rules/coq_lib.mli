open! Dune_engine

(* This file is licensed under The MIT License *)
(* (c) MINES ParisTech 2018-2019               *)
(* Written by: Emilio Jesús Gallego Arias *)

open! Stdune

type t

val name : t -> Coq_lib_name.t

val implicit : t -> bool

(* this is not really a wrapper for the prefix path *)
val wrapper : t -> string

(** ml libraries *)
val libraries : t -> (Loc.t * Lib_name.t) list

val src_root : t -> Path.Build.t

val obj_root : t -> Path.Build.t

val package : t -> Package.t option

module DB : sig
  type lib

  type t

  val create_from_coqlib_stanzas :
    (Path.Build.t * Coq_stanza.Theory.t) list -> t

  val find_many : t -> loc:Loc.t -> Coq_lib_name.t list -> lib list Or_exn.t

  val boot_library : t -> (Loc.t * lib) option

  val resolve : t -> Loc.t * Coq_lib_name.t -> lib Or_exn.t

  (** Return the list of dependencies needed for compiling this library *)
  val requires : t -> lib -> lib list Or_exn.t

  val requires_for_user_written :
    t -> (Loc.t * Coq_lib_name.t) list -> lib list Or_exn.t

  (** Reverse map from source names to a matching lib, plus the matching suffix;
      this is only required because coqdep doesn't handle native dependencies,
      so for a dependency [path/bar/foo.vo] we must correctly translate it to
      its mangled Coq native form, which requires locating the library which a
      source does belong to. The current matching function does return a triple
      [lib,
     prefix, name] for a given input path, or raises if not found.

      Note that this may be useful in the future if we are able to implement a
      [-modules] option in coqdep, so we could do the resolution
      [file -> module] ourselves. *)
  val module_of_source_file : t -> Path.Build.t -> lib * string list * string
end
with type lib := t
