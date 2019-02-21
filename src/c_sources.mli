(** This module loads and validates C/C++ sources from directories. *)

open Stdune

type t

val empty : t

val for_lib : t -> dir:Path.t -> name:Lib_name.t -> C.Sources.t

val for_exes : t -> first_exe:string -> C.Sources.t

(** [load_sources dir ~files] will load the C sources in [dir] into a two double
    map. The first level will is keyed by C vs. C++ sources. The second level is
    keyed by the object name of the source. *)
val load_sources
  :  dune_version:Syntax.Version.t
  -> dir:Path.t
  -> files:String.Set.t
  -> C.Source.t String.Map.t C.Kind.Dict.t

(** [make stanzas ~c_sources] will load and validate C/C++ sources. [c_sources]
    should be a two level map such as the one returned by [load_sources] *)
val make
  :  Stanza.t list Dir_with_dune.t
  -> c_sources:C.Source.t String.Map.t C.Kind.Dict.t
  -> t
