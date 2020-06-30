open! Stdune
open! Import

type workspace =
  { contexts : Context.t list
  ; conf : Dune_load.conf
  ; env : Env.t
  }

type build_system =
  { workspace : workspace
  ; scontexts : Super_context.t Context_name.Map.t
  }

(* Returns [Error ()] if [pkg] is unknown *)
val package_install_file :
     findlib_toolchain:Context_name.t option
  -> workspace
  -> Package.Name.t
  -> (Path.Source.t, unit) result

(** Scan the source tree and discover the overall layout of the workspace. *)
val scan_workspace :
     ?workspace_file:Path.t
  -> ?x:Context_name.t
  -> ?capture_outputs:bool
  -> ?profile:Profile.t
  -> ancestor_vcs:Vcs.t option
  -> unit
  -> workspace Fiber.t

(** Load dune files and initializes the build system *)
val init_build_system :
     ?only_packages:Package.t Package.Name.Map.t
  -> sandboxing_preference:Sandbox_mode.t list
  -> ?caching:Build_system.caching
  -> workspace
  -> build_system Fiber.t

val find_context_exn : workspace -> name:Context_name.t -> Context.t

(** Setup the environment *)
val setup_env : capture_outputs:bool -> Env.t

(** Set the concurrency level according to the user configuration *)
val set_concurrency : Config.t -> unit Fiber.t
