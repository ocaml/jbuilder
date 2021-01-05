module Style : sig
  type t

  val fg_default : t

  val fg_black : t

  val fg_red : t

  val fg_green : t

  val fg_yellow : t

  val fg_blue : t

  val fg_magenta : t

  val fg_cyan : t

  val fg_white : t

  val fg_bright_black : t

  val fg_bright_red : t

  val fg_bright_green : t

  val fg_bright_yellow : t

  val fg_bright_blue : t

  val fg_bright_magenta : t

  val fg_bright_cyan : t

  val fg_bright_white : t

  val bg_default : t

  val bg_black : t

  val bg_red : t

  val bg_green : t

  val bg_yellow : t

  val bg_blue : t

  val bg_magenta : t

  val bg_cyan : t

  val bg_white : t

  val bg_bright_black : t

  val bg_bright_red : t

  val bg_bright_green : t

  val bg_bright_yellow : t

  val bg_bright_blue : t

  val bg_bright_magenta : t

  val bg_bright_cyan : t

  val bg_bright_white : t

  val bold : t

  val dim : t

  val underlined : t

  (** Ansi escape sequence that set the terminal style to exactly these styles *)
  val escape_sequence : t list -> string
end

module Codes : sig
  type t

  val erase_from_cursor_to_eol : t
end

(** Print to [Format.std_formatter] *)
val print : Style.t list Pp.t -> unit

(** Print to [Format.err_formatter] *)
val prerr : Style.t list Pp.t -> unit

(** Whether [stdout]/[stderr] support colors *)
val stdout_supports_color : bool Lazy.t

val stderr_supports_color : bool Lazy.t

(** Filter out escape sequences in a string *)
val strip : string -> string

(** Parse a string containing ANSI escape sequences *)
val parse : string -> Style.t list Pp.t
