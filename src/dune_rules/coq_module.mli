open! Dune_engine

(* This file is licensed under The MIT License *)
(* (c) MINES ParisTech 2018-2019               *)
(* Written by: Emilio Jesús Gallego Arias *)

open! Stdune

module Name : sig
  type t

  val make : string -> t

  val compare : t -> t -> Ordering.t

  val equal : t -> t -> bool

  val to_dyn : t -> Dyn.t

  val to_string : t -> string
end

type t

(** A Coq module [a.b.foo] defined in file [a/b/foo.v] *)
val make :
     source:Path.Build.t
       (** file = .v source file; module name has to be the same so far *)
  -> prefix:string list (** Library-local qualified prefix *)
  -> name:Name.t (** Name of the module *)
  -> t

(** Coq does enforce some invariants wrt module vs file names *)

val source : t -> Path.Build.t

val prefix : t -> string list

val name : t -> Name.t

val dep_file : t -> obj_dir:Path.Build.t -> Path.Build.t

(** Some of the object files should not be installed, we control this with the
    following parameter *)
type obj_files_mode =
  | Build
  | Install

(** [obj_files t wrapper_name mode obj_dir obj_files_mode] returns the list of
    object files produced by a [coqc] invocation.

    Note that This returns a list of pairs [(obj_file, install_path)] due to
    native files having a different install / build location *)
val obj_files :
     t
  -> wrapper_name:string
  -> mode:Coq_mode.t
  -> obj_dir:Path.Build.t
  -> obj_files_mode:obj_files_mode
  -> (Path.Build.t * string) list

(** [obj_file t] returns just the .vo file of a module, this is the input to the
    standalone Coq native compiler; we should likely refactor this to pass the
    kind of object to obj_files as it was done some time ago by Rudi *)
val vo_obj_file : t -> obj_dir:Path.Build.t -> Path.Build.t

(** [native_obj_files] is the equivalent of [obj_files] but for the new
    [coqnative] separate Coq native compiler, it is used in the setup of the
    separate rules. *)
val native_obj_files :
     t
  -> wrapper_name:string
  -> obj_dir:Path.Build.t
  -> (Path.Build.t * string) list

(** [native_mangle_filename ~wrapper_name ~prefix ~name ~ext] will generate a
    mangled Coq native module file name from its arguments, determining a Coq
    module. Coq's native compiler needs to mangle filenames for a module
    [bar/foo.vo] bound to a [wrapper_name] [baz] to [NBaz_Bar_Foo.cmxs] to
    prevent collisions, as the OCaml compiler doesn't support encoding hiearchy
    of modules using the filesystem. We should eventually improve the clients of
    this API so we can just pass here a [Coq_module.t] *)
val native_mangle_filename :
     wrapper_name:string
  -> prefix:string list
  -> name:string
  -> obj_dir:Path.Build.t
  -> ext:string
  -> Path.Build.t

(** [vo_to_native file ~ext] will transform a file of the form , note we need a
    reverse lookup from .vo file to Coq module *)
(* val vo_to_native : Path.Build.t -> ext:string -> Path.Build.t *)

val to_dyn : t -> Dyn.t

val eval : dir:Path.Build.t -> standard:t list -> Ordered_set_lang.t -> t list
