open Import

(** This module loads and validates foreign sources from directories. *)

type t

val empty : t

val for_lib : t -> name:Lib_name.t -> Foreign.Sources.t

val for_archive : t -> archive_name:Foreign.Archive.Name.t -> Foreign.Sources.t

val for_exes : t -> first_exe:string -> Foreign.Sources.t

val standalone :
     Stanza.t list Dir_with_dune.t
  -> lib_config:Lib_config.t
  -> files:String.Set.t
  -> t

val group :
     Stanza.t list Dir_with_dune.t
  -> loc:Stdune.Loc.t
  -> include_subdirs:Dune_file.Include_subdirs.t
  -> lib_config:Lib_config.t
  -> subdirs:(Path.Build.t * 'a * String.Set.t) list
  -> t
