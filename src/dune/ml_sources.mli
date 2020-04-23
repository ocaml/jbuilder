(** OCaml sources *)

(** This module encapsulates the structure of source files in a particular
    directory. *)

open Import

module Artifacts : sig
  type t

  val lookup_module :
    t -> Module_name.t -> (Path.Build.t Obj_dir.t * Module.t) option Or_exn.t

  val lookup_library : t -> Lib_name.t -> Dune_file.Library.t option
end

type t

val artifacts : t -> Artifacts.t Or_exn.t

(** Modules attached to a library. [name] is the library best name. *)
val modules_of_library : t -> name:Lib_name.t -> Modules.t Or_exn.t

(** Modules attached to a set of executables. *)
val modules_of_executables :
  t -> obj_dir:Path.Build.t Obj_dir.t -> first_exe:string -> Modules.t Or_exn.t

(** Find out what buildable a module is part of *)
val lookup_module : t -> Module_name.t -> Dune_file.Buildable.t option Or_exn.t

val empty : t

(** This [lookup_vlib] argument is required for constructing the collection of
    modules for an implementation of a virtual library.

    We need to know the contents of the virtual library to: - verify conditions
    all virtual modules are implemented - make sure that we construct [Module.t]
    with the correct [kind] *)

val make :
     Stanza.t list Dir_with_dune.t
  -> loc:Loc.t
  -> lookup_vlib:(dir:Path.Build.t -> t)
  -> include_subdirs:Loc.t * Dune_file.Include_subdirs.t
  -> dirs:(Path.Build.t * 'a list * String.Set.t) list
  -> t
