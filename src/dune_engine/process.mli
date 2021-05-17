(** Running external programs *)

open Import

module With_directory_annot :
  User_error.Annotations.Entry.S with type payload = Path.t

(** How to handle sub-process failures *)
type ('a, 'b) failure_mode =
  | Strict : ('a, 'a) failure_mode
      (** Fail if the process exits with anything else than [0] *)
  | Accept : int Predicate_lang.t -> ('a, ('a, int) result) failure_mode
      (** Accept the following non-zero exit codes, and return [Error code] if
          the process exists with one of these codes. *)

module Io : sig
  (** Where to redirect stdout/stderr/stdin *)
  type input = Input

  type output = Output

  type 'a mode =
    | In : input mode
    | Out : output mode

  type 'a t

  val stdout : output t

  val make_stdout : Execution_parameters.Action_output_on_success.t -> output t

  val stderr : output t

  val make_stderr : Execution_parameters.Action_output_on_success.t -> output t

  val stdin : input t

  val null : 'a mode -> 'a t

  (** Return a buffered channel for this output. The channel is created lazily. *)
  val out_channel : output t -> out_channel

  (** Create a [t] representing redirecting the input or to a file or reading
      input from the file. The returned channel can only be used by a single
      call to {!run}. If you want to use it multiple times, you need to use
      [clone]. *)
  val file : Path.t -> ?perm:int -> 'a mode -> 'a t

  (** Call this when you no longer need this redirection *)
  val release : 'a t -> unit

  (** [multi_use t] returns a copy for which [release] does nothing *)
  val multi_use : 'a t -> 'a t
end

(** Why a Fiber.t was run *)
type purpose =
  | Internal_job
  | Build_job of Path.Build.Set.t

(** [run ?dir ?stdout_to prog args] spawns a sub-process and wait for its
    termination. [stdout_to] [stderr_to] are released *)
val run :
     ?dir:Path.t
  -> ?stdout_to:Io.output Io.t
  -> ?stderr_to:Io.output Io.t
  -> ?stdin_from:Io.input Io.t
  -> ?env:Env.t
  -> ?purpose:purpose
  -> (unit, 'a) failure_mode
  -> Path.t
  -> string list
  -> 'a Fiber.t

val run_with_times :
     ?dir:Path.t
  -> ?stdout_to:Io.output Io.t
  -> ?stderr_to:Io.output Io.t
  -> ?stdin_from:Io.input Io.t
  -> ?env:Env.t
  -> ?purpose:purpose
  -> Path.t
  -> string list
  -> Proc.Times.t Fiber.t

(** Run a command and capture its output *)
val run_capture :
     ?dir:Path.t
  -> ?stderr_to:Io.output Io.t
  -> ?stdin_from:Io.input Io.t
  -> ?env:Env.t
  -> ?purpose:purpose
  -> (string, 'a) failure_mode
  -> Path.t
  -> string list
  -> 'a Fiber.t

val run_capture_line :
     ?dir:Path.t
  -> ?stderr_to:Io.output Io.t
  -> ?stdin_from:Io.input Io.t
  -> ?env:Env.t
  -> ?purpose:purpose
  -> (string, 'a) failure_mode
  -> Path.t
  -> string list
  -> 'a Fiber.t

val run_capture_lines :
     ?dir:Path.t
  -> ?stderr_to:Io.output Io.t
  -> ?stdin_from:Io.input Io.t
  -> ?env:Env.t
  -> ?purpose:purpose
  -> (string list, 'a) failure_mode
  -> Path.t
  -> string list
  -> 'a Fiber.t

val run_capture_zero_separated :
     ?dir:Path.t
  -> ?stderr_to:Io.output Io.t
  -> ?stdin_from:Io.input Io.t
  -> ?env:Env.t
  -> ?purpose:purpose
  -> (string list, 'a) failure_mode
  -> Path.t
  -> string list
  -> 'a Fiber.t
