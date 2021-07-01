open! Dune_engine
open! Stdune
open Import

(** {1 Generals} *)

(** Representation of a library *)
type t

val to_dyn : t -> Dyn.t

(** For libraries defined in the workspace, this is the [public_name] if present
    or the [name] if not. *)
val name : t -> Lib_name.t

val lib_config : t -> Lib_config.t

val implements : t -> t Resolve.Build.t option

(** Directory where the object files for the library are located. *)
val obj_dir : t -> Path.t Obj_dir.t

(** Same as [Path.is_managed (obj_dir t)] *)
val is_local : t -> bool

val info : t -> Path.t Lib_info.t

val main_module_name : t -> Module_name.t option Resolve.Build.t

val entry_module_names : t -> Module_name.t list Resolve.Build.t

val src_dirs : t -> Path.Set.t Memo.Build.t

val wrapped : t -> Wrapped.t option Resolve.Build.t

(** [is_impl lib] returns [true] if the library is an implementation of a
    virtual library *)
val is_impl : t -> bool

(** Direct library dependencies of this library *)
val requires : t -> t list Resolve.Build.t

val ppx_runtime_deps : t -> t list Resolve.Build.t

(** A unique integer identifier. It is only unique for the duration of the
    process *)
module Id : sig
  type t

  val compare : t -> t -> Ordering.t
end

val unique_id : t -> Id.t

module Set : Set.S with type elt = t

module Map : Map.S with type key = t

val equal : t -> t -> bool

val hash : t -> int

(** The list of files that will be read by the compiler when linking an
    executable against this library *)
val link_deps : t -> Link_mode.t -> Path.t list Memo.Build.t

(** Operations on list of libraries *)
module L : sig
  type lib

  type nonrec t = t list

  val to_iflags : Path.Set.t -> _ Command.Args.t

  val include_paths : ?project:Dune_project.t -> t -> Mode.t -> Path.Set.t

  val include_flags : ?project:Dune_project.t -> t -> Mode.t -> _ Command.Args.t

  val c_include_paths : t -> Path.Set.t

  val c_include_flags : t -> _ Command.Args.t

  val toplevel_include_paths : t -> Path.Set.t

  val compile_and_link_flags :
    compile:t -> link:t -> mode:Link_mode.t -> _ Command.Args.t

  val jsoo_runtime_files : t -> Path.t list

  val remove_dups : t -> t

  val top_closure :
       'a list
    -> key:('a -> lib)
    -> deps:('a -> 'a list Resolve.Build.t)
    -> ('a list, 'a list) Result.t Resolve.Build.t
end
with type lib := t

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
      t -> lib_config:Lib_config.t -> mode:Link_mode.t -> _ Command.Args.t
  end
end
with type lib := t

(** {1 Compilation contexts} *)

(** See {!Sub_system} *)
type sub_system = ..

(** For compiling a library or executable *)
module Compile : sig
  type t

  type lib

  (** Return the list of dependencies needed for linking this library/exe *)
  val requires_link : t -> L.t Resolve.t Memo.Lazy.t

  (** Dependencies listed by the user + runtime dependencies from ppx *)
  val direct_requires : t -> L.t Resolve.Build.t

  module Resolved_select : sig
    type t =
      { src_fn : string Resolve.Build.t
      ; dst_fn : string
      }
  end

  (** Resolved select forms *)
  val resolved_selects : t -> Resolved_select.t list Resolve.Build.t

  (** Transitive closure of all used ppx rewriters *)
  val pps : t -> L.t Resolve.Build.t

  val merlin_ident : t -> Merlin_ident.t

  (** Sub-systems used in this compilation context *)
  val sub_systems : t -> sub_system list Memo.Build.t
end
with type lib := t

(** {1 Library name resolution} *)

(** Collection of libraries organized by names *)
module DB : sig
  type lib = t

  (** A database allow to resolve library names *)
  type t

  module Resolve_result : sig
    type t

    type db

    val not_found : t

    val found : Lib_info.external_ -> t

    val to_dyn : t Dyn.Encoder.t

    val redirect : db option -> Loc.t * Lib_name.t -> t
  end
  with type db := t

  (** Create a new library database. [resolve] is used to resolve library names
      in this database.

      When a library is not found, it is looked up in the parent database if
      any.

      [all] returns the list of names of libraries available in this database. *)
  val create :
       parent:t option
    -> resolve:(Lib_name.t -> Resolve_result.t)
    -> projects_by_package:Dune_project.t Package.Name.Map.t
    -> all:(unit -> Lib_name.t list)
    -> modules_of_lib:
         (dir:Path.Build.t -> name:Lib_name.t -> Modules.t Memo.Build.t) Fdecl.t
    -> lib_config:Lib_config.t
    -> unit
    -> t

  val create_from_findlib :
       lib_config:Lib_config.t
    -> projects_by_package:Dune_project.t Package.Name.Map.t
    -> Findlib.t
    -> t

  val find : t -> Lib_name.t -> lib option Memo.Build.t

  val find_even_when_hidden : t -> Lib_name.t -> lib option Memo.Build.t

  val available : t -> Lib_name.t -> bool Memo.Build.t

  (** Retrieve the compile information for the given library. Works for
      libraries that are optional and not available as well. *)
  val get_compile_info :
    t -> ?allow_overlaps:bool -> Lib_name.t -> Compile.t Memo.Build.t

  val resolve : t -> Loc.t * Lib_name.t -> lib Resolve.Build.t

  (** Like [resolve], but will return [None] instead of an error if we are
      unable to find the library. *)
  val resolve_when_exists :
    t -> Loc.t * Lib_name.t -> lib Resolve.t option Memo.Build.t

  (** Resolve libraries written by the user in a [dune] file. The resulting list
      of libraries is transitively closed and sorted by the order of
      dependencies.

      This function is for executables stanzas. *)
  val resolve_user_written_deps_for_exes :
       t
    -> (Loc.t * string) list
    -> ?allow_overlaps:bool
    -> ?forbidden_libraries:(Loc.t * Lib_name.t) list
    -> Lib_dep.t list
    -> pps:(Loc.t * Lib_name.t) list
    -> dune_version:Dune_lang.Syntax.Version.t
    -> Compile.t

  val resolve_pps : t -> (Loc.t * Lib_name.t) list -> L.t Resolve.Build.t

  (** Return the list of all libraries in this database. If [recursive] is true,
      also include libraries in parent databases recursively. *)
  val all : ?recursive:bool -> t -> Set.t Memo.Build.t

  val instrumentation_backend :
       t
    -> Loc.t * Lib_name.t
    -> Preprocess.Without_instrumentation.t option Resolve.Build.t
end
with type lib := t

(** {1 Transitive closure} *)

val closure : L.t -> linking:bool -> L.t Resolve.Build.t

(** {1 Sub-systems} *)

module Sub_system : sig
  type lib = t

  type t = sub_system = ..

  module type S = sig
    module Info : Sub_system_info.S

    type t

    type sub_system += T of t

    val instantiate :
         resolve:(Loc.t * Lib_name.t -> lib Resolve.Build.t)
      -> get:(loc:Loc.t -> lib -> t option Memo.Build.t)
      -> lib
      -> Info.t
      -> t Memo.Build.t

    val public_info : (t -> Info.t Resolve.Build.t) option
  end

  module Register (M : S) : sig
    (** Get the instance of the subsystem for this library *)
    val get : lib -> M.t option Memo.Build.t
  end
end
with type lib := t

val to_dune_lib :
     t
  -> modules:Modules.t
  -> foreign_objects:Path.t list
  -> dir:Path.t
  -> Dune_package.Lib.t Resolve.Build.t

(** Local libraries *)
module Local : sig
  type lib

  type t = private lib

  val to_dyn : t -> Dyn.t

  val equal : t -> t -> bool

  val hash : t -> int

  val of_lib : lib -> t option

  val of_lib_exn : lib -> t

  val to_lib : t -> lib

  val info : t -> Lib_info.local

  val obj_dir : t -> Path.Build.t Obj_dir.t

  module Set : Stdune.Set.S with type elt = t

  module Map : Stdune.Map.S with type key = t
end
with type lib := t
