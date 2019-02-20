open Stdune

type t

val ocamlpath_sep : char

val ocamlpath : Env.t -> Path.t list

val discover_from_env
  :  env:Env.t
  -> ocamlpath:Path.t list
  -> which:(string -> Path.t option)
  -> t Fiber.t option

val tool : t -> prog:string -> Path.t option

val conf_path : t -> Path.t list

val set_toolchain : t -> toolchain:string -> t

val extra_env : t -> Env.t
