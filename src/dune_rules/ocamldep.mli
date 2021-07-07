(** ocamldep management *)

open! Dune_engine
open Import

val parse_module_names :
  unit:Module.t -> modules:Modules.t -> string list -> Module.t list

val parse_deps_exn : file:Path.t -> string list -> string list

val read_deps_of :
     obj_dir:Path.Build.t Obj_dir.t
  -> modules:Modules.t
  -> ml_kind:Ml_kind.t
  -> Module.t
  -> Module.t list Action_builder.t

val read_raw_deps_of :
     obj_dir:Path.Build.t Obj_dir.t
  -> ml_kind:Ml_kind.t
  -> Module.t
  -> Module_name.t list Action_builder.t option
