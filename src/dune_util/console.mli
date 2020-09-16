(** Manages the console *)

(** The console is a system than can report messages and a status to the user.
    It is usually the terminal the application is connected to, however it could
    be something else. This module allow to set a global backend for the
    application as well as composing backends. *)

open Stdune

module Backend : sig
  module type S = sig
    (** Format and print a user message to the console *)
    val print_user_message : User_message.t -> unit

    (** Change the status line *)
    val set_status_line : User_message.Style.t Pp.t option -> unit

    (** Reset the log output *)
    val reset : unit -> unit
  end

  type t = (module S)

  val set : t -> unit

  (** [compose a b] produce a backend that sends message to both [a] and [b]
      backends. *)
  val compose : t -> t -> t

  (** A dumb backend that hides the status line and simply dump the messages to
      the terminal *)
  val dumb : t

  (** A backend that just displays the status line in the terminal *)
  val progress : t
end

(** The main backend for the application *)
include Backend.S

(** [print paragraphs] is a short-hand for:

    {[ print_user_message (User_message.make paragraphs) ]} *)
val print : User_message.Style.t Pp.t list -> unit

(** [with_terminal_lock f] will run [f ()] under a mutex, during which all
    writes to the console (from [Backend]) will be buffered to be displayed upon
    release of the lock. *)
val with_terminal_lock : (unit -> 'a Fiber.t) -> 'a Fiber.t

module Status_line : sig
  (** This module allows to buffer status updates so that they don't slow down
      the application *)

  (** Function that produces the current status line *)
  type t = unit -> User_message.Style.t Pp.t option

  (** Change the status line generator *)
  val set : t -> unit

  val set_temporarily : t -> (unit -> 'a) -> 'a

  val refresh : unit -> unit
end
