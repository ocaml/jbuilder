open Import

type t =
  | Lib
  | Lib_root
  | Libexec
  | Libexec_root
  | Bin
  | Sbin
  | Toplevel
  | Share
  | Share_root
  | Etc
  | Etc_root
  | Doc
  | Stublibs
  | Man
  | Misc

val compare : t -> t -> Ordering.t

include Comparable_intf.S with type Key.t = t

val enum_decoder : (string * t) list

val all : Set.t

val to_string : t -> string

val of_string : string -> t option

val parse_string : string -> (t, string) Result.t

val decode : t Dune_lang.Decoder.t

val encode : t Dune_lang.Encoder.t

val to_dyn : t -> Dyn.t

(** [true] iff the executable bit should be set for files installed in this
    location. *)
val should_set_executable_bit : t -> bool

module Site : sig
  type t

  include Interned_intf.S with type t := t

  include Dune_lang.Conv.S with type t := t

  module Infix : Comparator.OPS with type t = t

  include Stringlike_intf.S with type t := t
end

val dune_site_syntax : Dune_lang.Syntax.t

module Modulelike (S : sig
  type t

  (** The name of the module, for use in error messages. For example
      ["Lib_name"], ["Context_name"]. *)
  val module_ : string

  (** A short description of the type, for use in user-facing error messages.
      For example "context name", "library name". *)
  val description : string

  val to_string : t -> string

  (** The string is always a correct module name, except not capitalized *)
  val make : string -> t
end) : Stringlike_intf.S with type t = S.t

val valid_format_doc : User_message.Style.t Pp.t
