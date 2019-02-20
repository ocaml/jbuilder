(** Findlib database *)

open! Stdune
open Import

(** Findlib database *)
type t

val create
  :  stdlib_dir:Path.t
  -> paths:Path.t list
  -> version:Ocaml_version.t
  -> t

(** The search path for this DB *)
val paths : t -> Path.t list

module Package : sig
  val preds : Variant.Set.t
end

module Unavailable_reason : sig
  type t =
    | Not_found
    (** The package is hidden because it contains an unsatisfied
        'exist_if' clause *)
    | Hidden of Sub_system_info.t Dune_package.Lib.t

  val pp : Format.formatter -> t -> unit
end

(** Lookup a package in the given database *)
val find
  : t
  -> Lib_name.t
  -> ( Sub_system_info.t Dune_package.Lib.t
     , Unavailable_reason.t
     ) result

val available : t -> Lib_name.t -> bool

(** List all the packages available in this Database *)
val all_packages  : t -> Sub_system_info.t Dune_package.Lib.t list

(** List all the packages that are not available in this database *)
val all_unavailable_packages : t -> (Lib_name.t * Unavailable_reason.t) list

(** A dummy package. This is used to implement [external-lib-deps] *)
val dummy_package
  :  t
  -> name:Lib_name.t
  -> Sub_system_info.t Dune_package.Lib.t

module Config : sig
  type t

  val pp : t Fmt.t

  val load : Path.t -> t

  val get : t -> string -> string option

  val env : t -> Env.t

  val toolchain : t -> toolchain:string -> t
end
