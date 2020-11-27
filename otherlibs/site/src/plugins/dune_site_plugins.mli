module V1 : sig
  (** [load name] loads the given library *)
  val load : string -> unit

  (** [available name] tests if a library is available for loading. The loading
      can still fail if a dependency of the library is unavailable or if the
      loading of the modules fails. *)
  val available : string -> bool

  (** Compile and load an ocaml module. All the currently loaded libraries are
      made available to the script. It returns the standard and error output of
      the compiler *)
  val load_script :
       ?open_:string list
    -> ?warnings:string
    -> string
    -> [> `Compilation_failed | `Ok ] * string list * string list
end

(** **/ *)

module Private_ : sig
  module Plugins : module type of Plugins

  module Meta_parser : module type of Meta_parser
end
