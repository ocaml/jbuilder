open! Stdune
open Import

(** {1 Generals} *)

(** Representation of a library *)
type t

val to_dyn : t -> Dyn.t

(** For libraries defined in the workspace, this is the [public_name] if
    present or the [name] if not. *)
val name : t -> Lib_name.t

val implements : t -> t Or_exn.t option

(** Directory where the object files for the library are located. *)
val obj_dir : t -> Path.t Obj_dir.t

(** Same as [Path.is_managed (obj_dir t)] *)
val is_local : t -> bool

val info : t -> Path.t Lib_info.t

val main_module_name : t -> Module_name.t option Or_exn.t
val wrapped : t -> Wrapped.t option Or_exn.t

(** [is_impl lib] returns [true] if the library is an implementation
    of a virtual library *)
val is_impl : t -> bool

(** A unique integer identifier. It is only unique for the duration of
    the process *)
module Id : sig
  type t

  val compare : t -> t -> Ordering.t
end
val unique_id : t -> Id.t

module Set : Set.S with type elt = t

module Map : Map.S with type key = t

val package : t -> Package.Name.t option

val equal : t -> t -> bool
val hash : t -> int

(** Operations on list of libraries *)
module L : sig
  type lib
  type nonrec t = t list

  val to_iflags : Path.Set.t -> 'a Command.Args.t

  val include_paths : t -> Path.Set.t
  val include_flags : t -> _ Command.Args.t

  val c_include_flags : t -> _ Command.Args.t

  val link_flags : t -> mode:Mode.t -> _ Command.Args.t

  val compile_and_link_flags
    :  compile:t
    -> link:t
    -> mode:Mode.t
    -> _ Command.Args.t

  (** All the library archive files (.a, .cmxa, _stubs.a, ...)  that
      should be linked in when linking an executable. *)
  val archive_files : t -> mode:Mode.t -> Path.t list

  val jsoo_runtime_files : t -> Path.t list

  val remove_dups : t -> t

  val top_closure
    :  'a list
    -> key:('a -> lib)
    -> deps:('a -> 'a list)
    -> ('a list, 'a list) Result.t
end with type lib := t

(** Operation on list of libraries and modules *)
module Lib_and_module : sig
  type lib = t
  type t =
    | Lib of lib
    | Module of Path.t Obj_dir.t * Module.t

  module L : sig
    type nonrec t = t list
    val of_libs : lib list -> t
    val link_flags :
      t -> lib_config:Lib_config.t -> mode:Mode.t -> _ Command.Args.t
  end
end with type lib := t

(** {1 Compilation contexts} *)

(** See {!Sub_system} *)
type sub_system = ..

(** For compiling a library or executable *)
module Compile : sig
  type t

  (** Return the list of dependencies needed for linking this library/exe *)
  val requires_link : t -> L.t Or_exn.t Lazy.t

  (** Dependencies listed by the user + runtime dependencies from ppx *)
  val direct_requires : t -> L.t Or_exn.t

  module Resolved_select : sig
    type t =
      { src_fn : string Or_exn.t
      ; dst_fn : string
      }
  end

  (** Resolved select forms *)
  val resolved_selects : t -> Resolved_select.t list

  (** Transitive closure of all used ppx rewriters *)
  val pps : t -> L.t Or_exn.t

  val lib_deps_info : t -> Lib_deps_info.t

  (** Sub-systems used in this compilation context *)
  val sub_systems : t -> sub_system list
end

(** {1 Library name resolution} *)

(** Collection of libraries organized by names *)
module DB : sig
  type lib = t

  (** A database allow to resolve library names *)
  type t

  module Resolve_result : sig
    type nonrec t =
      | Not_found
      | Found         of Lib_info.external_
      | Hidden        of Lib_info.external_ * string
      | User_hidden   of Lib_info.external_ * string
      | Redirect      of t option * Lib_name.t
  end

  (** Create a new library database. [resolve] is used to resolve
      library names in this database.

      When a library is not found, it is looked up in the parent
      database if any.

      [all] returns the list of names of libraries available in this database.
  *)
  val create
    :  ?parent:t
    -> stdlib_dir:Path.t
    -> resolve:(Lib_name.t -> Resolve_result.t)
    -> all:(unit -> Lib_name.t list)
    -> unit
    -> t

  (** Create a database from a list of library/variants stanzas *)
  val create_from_library_stanzas
    :  ?parent:t
    -> lib_config:Lib_config.t
    -> (Path.Build.t * Dune_file.Library.t) list
    -> Dune_file.External_variant.t list
    -> t

  val create_from_findlib
    :  ?external_lib_deps_mode:bool
    -> stdlib_dir:Path.t
    -> Findlib.t
    -> t

  val find : t -> Lib_name.t -> lib option
  val find_even_when_hidden : t -> Lib_name.t -> lib option

  val available : t -> Lib_name.t -> bool

  (** Retrieve the compile information for the given library. Works
      for libraries that are optional and not available as well. *)
  val get_compile_info : t -> ?allow_overlaps:bool -> Lib_name.t -> Compile.t

  val resolve : t -> Loc.t * Lib_name.t -> lib Or_exn.t

  (** Resolve libraries written by the user in a jbuild file. The
      resulting list of libraries is transitively closed and sorted by
      order of dependencies.

      This function is for executables stanzas.  *)
  val resolve_user_written_deps_for_exes
    :  t
    -> (Loc.t * string) list
    -> ?allow_overlaps:bool
    -> Dune_file.Lib_dep.t list
    -> pps:(Loc.t * Lib_name.t) list
    -> variants: (Loc.t * Variant.Set.t) option
    -> optional:bool
    -> Compile.t

  val resolve_pps
    :  t
    -> (Loc.t * Lib_name.t) list
    -> L.t Or_exn.t

  (** Return the list of all libraries in this database. If
      [recursive] is true, also include libraries in parent databases
      recursively. *)
  val all : ?recursive:bool -> t -> Set.t
end with type lib := t

(** {1 Transitive closure} *)

val closure : L.t -> linking:bool -> L.t Or_exn.t

(** {1 Sub-systems} *)

module Sub_system : sig
  type lib = t

  type t = sub_system = ..

  module type S = sig
    module Info : Sub_system_info.S
    type t
    type sub_system += T of t
    val instantiate
      :  resolve:(Loc.t * Lib_name.t -> lib Or_exn.t)
      -> get:(loc:Loc.t -> lib -> t option)
      -> lib
      -> Info.t
      -> t
    val encode : (t -> Syntax.Version.t * Dune_lang.t list) option
  end

  module Register(M : S) : sig
    (** Get the instance of the subsystem for this library *)
    val get : lib -> M.t option
  end

  val dump_config
    : lib
    -> (Syntax.Version.t * Dune_lang.t list) Sub_system_name.Map.t
end with type lib := t

(** {1 Dependencies for META files} *)

module Meta : sig
  val requires                               : t -> Lib_name.Set.t
  val ppx_runtime_deps                       : t -> Lib_name.Set.t
  val ppx_runtime_deps_for_deprecated_method : t -> Lib_name.Set.t
end

val to_dune_lib
  :  t
  -> modules:Modules.t
  -> foreign_objects:Path.t list
  -> dir:Path.t
  -> (Syntax.Version.t * Dune_lang.t list) Dune_package.Lib.t

module Local : sig
  type lib
  type t = private lib

  val to_dyn : t -> Dyn.t
  val equal : t -> t -> bool
  val hash : t -> int

  val of_lib : lib -> t option
  val of_lib_exn : lib -> t
  val to_lib : t -> lib

  val info    : t -> Lib_info.local
  val obj_dir : t -> Path.Build.t Obj_dir.t

  module Set : Stdune.Set.S with type elt = t
  module Map : Stdune.Map.S with type key = t

end with type lib := t
