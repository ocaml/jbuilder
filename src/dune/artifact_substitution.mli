(** Handling of substitutions in artifacts during promotion and installation *)

open Stdune

(** A symbolic representation of the value to substitute to *)
type t =
  | Vcs_describe of Path.Source.t
  | Custom of string * Path.Build.t
  | Repeat of int * string
      (** [Repeat (n, s)] evaluates to [s] repeated [n] times. This substitution
          is used for unit tests. *)

val to_dyn : t -> Dyn.t

(** A string encoding of a substitution. The resulting string is what should be
    written inside generated source files. {!copy_file} recognise such strings
    and expand them.

    The resulting string is guaranteed to be of length at least [min_len], which
    defaults to [0]. *)
val encode : ?min_len:int -> t -> string

(** [decode s] returns the value [t] such that [encode t = s]. *)
val decode : string -> t option

(** Copy a file, performing all required substitutions *)
val copy_file :
     get_vcs:(Path.Source.t -> Vcs.t option)
  -> ?chmod:(int -> int)
  -> src:Path.t
  -> dst:Path.t
  -> unit
  -> unit Fiber.t

(** Generic version of [copy_file]. Rather than filenames, it takes an input and
    output functions. Their semantic must match the ones of the [input] and
    [output] functions from the OCaml standard library.

    [input_file] is used only for debugging purposes. It must be the name of the
    source file. *)
val copy :
     get_vcs:(Path.Source.t -> Vcs.t option)
  -> input_file:Path.t
  -> input:(Bytes.t -> int -> int -> int)
  -> output:(Bytes.t -> int -> int -> unit)
  -> unit Fiber.t

(** Produce the string that would replace the placeholder with the given value .*)
val encode_replacement : len:int -> repl:string -> string
