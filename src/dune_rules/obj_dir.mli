(** Representation of the object directory for libraries *)

(** Dune store the artifacts of a library or a set of executables in a dedicated
    dot directory (name starting with a '.').

    This is mainly for hygiene reasons. Since the compiler might look at any
    artifact in an include directory, it is important that we control precisely
    what it can see. This is important when a directory contains several
    libraries and/or executables.

    This also allows us to provide a different API for a given library. In
    particular, depending on the context we might choose to expose or not the
    private modules.

    In the rest of this API, "local" and "external" have their usual Dune
    meaning: "local" is for libraries or executables that are local to the
    current worksapce and "extenal" for libraries that are part of the installed
    world.

    For local libraries, the path are reported as [Path.Build.t] values given
    that they are all inside the build directory. For external libraries the
    path are reported as [Path.t] values. However, it is possible to get a view
    of the object directory for a local library where the path are reported as
    [Path.t] values with [of_local]. This is convenient in places where we need
    to treat object directories of both local and external library in the same
    way. *)

open! Dune_engine
open! Import

type 'path t

val of_local : Path.Build.t t -> Path.t t

(** The source_root directory *)
val dir : 'path t -> 'path

(** The directory for ocamldep files *)
val obj_dir : 'path t -> 'path

(** The private compiled native file directory *)
val native_dir : 'path t -> 'path

(** The private compiled byte file directories, and all cmi *)
val byte_dir : 'path t -> 'path

val all_cmis : 'path t -> 'path list

(** The public compiled cmi file directory *)
val public_cmi_dir : 'path t -> 'path

val odoc_dir : 'path t -> 'path

val all_obj_dirs : 'path t -> mode:Mode.t -> 'path list

(** Create the object directory for a library *)
val make_lib :
     dir:Path.Build.t
  -> has_private_modules:bool
  -> private_lib:bool
  -> Lib_name.Local.t
  -> Path.Build.t t

(** Create the object directory for an external library that has no private
    directory for private modules *)
val make_external_no_private : dir:Path.t -> Path.t t

val encode : Path.t t -> Dune_lang.t list

val decode : dir:Path.t -> Path.t t Dune_lang.Decoder.t

val convert_to_external : Path.Build.t t -> dir:Path.t -> Path.t t

val cm_dir : 'path t -> Cm_kind.t -> Visibility.t -> 'path

val to_dyn : _ t -> Dyn.t

val make_exe : dir:Path.Build.t -> name:string -> Path.Build.t t

val for_pp : dir:Path.Build.t -> Path.Build.t t

val as_local_exn : Path.t t -> Path.Build.t t

(** For local libraries with private modules, all public cmi's are symlinked to
    their own directory. Such a public cmi dir is only necessary if a library
    contains private modules *)
val need_dedicated_public_dir : Path.Build.t t -> bool

val to_local : Path.t t -> Path.Build.t t option

module Module : sig
  (** The functions in this this module gives the paths to the various object
      files produced from the compilation of a module (.cmi files, .cmx files,
      .o files, ...) *)

  val cm_file : 'path t -> Module.t -> kind:Cm_kind.t -> 'path option

  val cm_public_file : 'path t -> Module.t -> kind:Cm_kind.t -> 'path option

  val cmt_file : 'path t -> Module.t -> ml_kind:Ml_kind.t -> 'path option

  val obj_file : 'path t -> Module.t -> kind:Cm_kind.t -> ext:string -> 'path

  (** Same as [cm_file] but raises if [cm_kind] is [Cmo] or [Cmx] and the module
      has no implementation.*)
  val cm_file_exn : 'path t -> Module.t -> kind:Cm_kind.t -> 'path

  val o_file : 'path t -> Module.t -> ext_obj:string -> 'path option

  val o_file_exn : 'path t -> Module.t -> ext_obj:string -> 'path

  val cm_public_file_exn : 'path t -> Module.t -> kind:Cm_kind.t -> 'path

  (** Either the .cmti, or .cmt if the module has no interface *)
  val cmti_file : 'path t -> Module.t -> 'path

  val odoc : 'path t -> Module.t -> 'path

  val odocl : 'path t -> Module.t -> 'path

  module L : sig
    val o_files : 'path t -> Module.t list -> ext_obj:string -> Path.t list

    val cm_files : 'path t -> Module.t list -> kind:Cm_kind.t -> Path.t list
  end

  module Dep : sig
    type t =
      | Immediate of Module.File.t
      | Transitive of Module.t * Ml_kind.t
  end

  val dep : Path.Build.t t -> Dep.t -> Path.Build.t
end
