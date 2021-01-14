open! Dune_engine
open Stdune

val drop_source_extension :
     string
  -> dune_version:Dune_lang.Syntax.Version.t
  -> (string * Foreign_language.t) option

val possible_sources :
     language:Foreign_language.t
  -> string
  -> dune_version:Dune_lang.Syntax.Version.t
  -> string list

(* CR-soon cwong: I'd really prefer to keep the convension that these functions
   are spelled [Foreign_language.encode] and [Foreign_language.decode], but due
   to some organizational reasons (see the rant at the top of
   [foreign_language.mli]), these need to be here instead, as they cannot reside
   in the backend. *)

(** The string used to encode a language in Dune files, e.g. "cxx" for [Cxx]. *)
val encode_lang : Foreign_language.t -> string

val decode_lang : Foreign_language.t Dune_lang.Decoder.t

(** Foreign archives appear in the [(foreign_archives ...)] field of libraries
    and executables, for example [(foreign_archives some/dir/lib)]. When parsing
    such fields, we separate the directory [some/dir] from the name [lib] of the
    archive and store them as a [{dir : Dir.t; name : Name.t}] record. *)
module Archive : sig
  (** Archive names appear as part of the [(foreign_archives ...)] fields, as
      well as in the [(archive_name ...)] field of the [(foreign_library ...)]
      stanza. For example, both [(foreign_archives some/dir/lib)] and
      [(archive_name lib)] specify the same archive name [lib]. Archive names
      are not allowed to contain path separators. *)
  module Name : sig
    type t

    module Map : Map.S with type key = t

    val to_dyn : t -> Dyn.t

    val to_string : t -> string

    val path : dir:Path.Build.t -> t -> Path.Build.t

    val decode : t Dune_lang.Decoder.t

    val stubs : string -> t

    val lib_file_prefix : string

    val lib_file : t -> dir:Path.Build.t -> ext_lib:string -> Path.Build.t

    val dll_file : t -> dir:Path.Build.t -> ext_dll:string -> Path.Build.t
  end

  type t

  val dir_path : dir:Path.Build.t -> t -> Path.Build.t

  val name : t -> Name.t

  val stubs : string -> t

  val decode : t Dune_lang.Decoder.t

  val lib_file : archive:t -> dir:Path.Build.t -> ext_lib:string -> Path.Build.t

  val dll_file : archive:t -> dir:Path.Build.t -> ext_dll:string -> Path.Build.t
end

(** In some situations, it is useful to make compilation of foreign source files
    conditional, and include the resulting objects only into native or bytecode
    libraries, but not both. *)
module Compilation_mode : sig
  type t =
    | Both_byte_and_native
    | Only_byte
    | Only_native
end

(** A type of foreign library "stubs", which includes all fields of the
    [Library.t] type except for the [archive_name] field. The type is parsed as
    an optional [foreign_stubs] field of the [library] stanza, or as part of the
    top-level [foreign_library] stanza. *)
module Stubs : sig
  (* Foreign sources can depend on a directly specified directory [Dir] or on a
     source directory of a library [Lib]. *)
  module Include_dir : sig
    type t =
      | Dir of String_with_vars.t
      | Lib of Loc.t * Lib_name.t

    val decode : t Dune_lang.Decoder.t
  end

  type t =
    { loc : Loc.t
    ; language : Foreign_language.t
    ; mode : Compilation_mode.t
    ; names : Ordered_set_lang.t
    ; flags : Ordered_set_lang.Unexpanded.t
    ; include_dirs : Include_dir.t list
    ; extra_deps : Dep_conf.t list
    }

  (** Construct foreign library stubs with some fields set to default values. *)
  val make :
       loc:Loc.t
    -> language:Foreign_language.t
    -> names:Ordered_set_lang.t
    -> flags:Ordered_set_lang.Unexpanded.t
    -> t

  val decode : t Dune_lang.Decoder.t
end

(** Foreign libraries.

    This data type represents the contents of the top-level stanza
    [foreign_library].

    The fields have the following semantics.

    [language] selects the compiler. At the moment, we support only [c] and
    [cxx] settings, but in future other languages/compilers could be supported,
    e.g. Rust and Clang.

    [archive_name] determines the names of the resulting [.a] archive files.

    [names] are names of source files. The full paths to the files are
    determined by scanning package directories. Duplicate file names are
    disallowed to avoid conflicting object names in the resulting archive file.

    [flags] are passed when compiling source files.

    [include_dirs] are tracked as dependencies and passed to the compiler via
    the "-I" flag.

    [extra_deps] are tracked as dependencies. *)
module Library : sig
  type t =
    { archive_name : Archive.Name.t
    ; archive_name_loc : Loc.t
    ; stubs : Stubs.t
    }

  val decode : t Dune_lang.Decoder.t
end

(** A foreign source file that has a [path] and all information of the
    corresponnding [Foreign.Stubs.t] declaration. *)
module Source : sig
  type t =
    { stubs : Stubs.t
    ; path : Path.Build.t
    }

  val language : t -> Foreign_language.t

  val mode : t -> Compilation_mode.t

  val flags : t -> Ordered_set_lang.Unexpanded.t

  val path : t -> Path.Build.t

  (* The name of the corresponding object file; for example, [name] for a source
     file [some/path/name.cpp]. *)
  val object_name : t -> string

  val make : stubs:Stubs.t -> path:Path.Build.t -> t
end

(** The [path] to a foreign object file along with the compilation [mode] that
    specfies whether it should be included into native or bytecode libraries, or
    into both (the latter is the most common case). *)
module Object : sig
  type t =
    { path : Path.Build.t
    ; mode : Compilation_mode.t
    }

  (** Should this file be included into both native and bytecode libraries? *)
  val both_byte_and_native : t -> bool

  (** Path to the object file in the build directory. *)
  val build_path : t -> Path.t

  (** Like [build_path] but returns [Some] only if the object should be included
      into bytecode libraries. *)
  val build_path_byte : t -> Path.t option

  (** Like [build_path] but returns [Some] only if the object should be included
      into native libraries. *)
  val build_path_native : t -> Path.t option
end

(** A map from object names to the corresponding sources. *)
module Sources : sig
  type t = (Loc.t * Source.t) String.Map.t

  val object_files : t -> dir:Path.Build.t -> ext_obj:string -> Object.t list

  (** A map from object names to lists of possible language/path combinations. *)
  module Unresolved : sig
    type t = (Foreign_language.t * Path.Build.t) String.Map.Multi.t

    val to_dyn : t -> Dyn.t

    (** [load ~dir ~files] loads foreign sources in [dir] into a map keyed by
        the object name. *)
    val load :
         dune_version:Dune_lang.Syntax.Version.t
      -> dir:Path.Build.t
      -> files:String.Set.t
      -> t
  end
end
