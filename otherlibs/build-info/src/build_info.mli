(** Provide build information *)

module V1 : sig
  module Version : sig
    type t

    val to_string : t -> string
  end

  (** The version at which the current executable was built. The version is
      [None] during development, it is only [Some _] once the executable is
      installed or promoted to the source tree. *)
  val version : unit -> Version.t option

  module Statically_linked_library : sig
    type t

    (** The most visible name of the library. If it is a public library, this
        its public name otherwise it is its private name. *)
    val name : t -> string

    val version : t -> Version.t option
  end

  module Statically_linked_libraries : sig
    (** All the libraries that where statically linked in *)

    val to_list : unit -> Statically_linked_library.t list

    val find : name:string -> Statically_linked_library.t option
  end
end

module V2 : sig
  include module type of V1

  module Custom : sig
    type t

    val to_string : t -> string
  end

  val custom : unit -> Custom.t option

  val custom_lib : string -> Custom.t option
end
