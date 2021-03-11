(** Canonical S-expression RPC.

    This module implements a RPC mechanism for exchanging canonical
    S-expressions over unix or internet sockets. It allows a server to accept
    connections and a client to connect to a server.

    However, it doesn't explain how to encode queries, responses or generally
    any kind of messages as Canonical S-expressions. This part should be built
    on top of this module.

    The actual system calls to connect to a server, accept connections, read and
    write messages are performed in separate threads. To integrate with the
    application scheduler, this module requires a way to create an ivar than can
    be filled from a separate system thread. *)

open Stdune

module Session : sig
  (** Rpc session backed by two threads. One thread for reading, and another for
      writing *)
  type t

  val create : in_channel -> out_channel -> Task_queue.Scheduler.t -> t

  (* [write t x] writes the s-expression when [x] is [Some sexp], and closes the
     session if [x = None ] *)
  val write : t -> Sexp.t option -> unit Fiber.t

  (** If [read] returns [None], the session is closed and all subsequent reads
      will return [None] *)
  val read : t -> Sexp.t option Fiber.t
end

module Client : sig
  (** RPC Client *)
  type t

  val create : Unix.sockaddr -> Task_queue.Scheduler.t -> t

  val stop : t -> unit

  val connect : t -> Session.t Fiber.t
end

module Server : sig
  (** RPC Server *)
  type t

  val create : Unix.sockaddr -> backlog:int -> Task_queue.Scheduler.t -> t

  val stop : t -> unit

  val serve : t -> Session.t Fiber.Stream.In.t Fiber.t

  val listening_address : t -> Unix.sockaddr
end
