open! Stdune
open Import

(** Rpc related functions *)

module Config : sig
  type t =
    | Client
    | Server of
        { handler : Dune_rpc_server.t
        ; pool : Fiber.Pool.t
        ; backlog : int
        }
end

(** Stop accepting new rpc connections. Fiber returns when all existing
    connetions terminate *)
val stop : unit -> unit Fiber.t

val run : Config.t -> Dune_stats.t option -> unit Fiber.t

(** [client t where init ~on_notification ~f] Establishes a client connection to
    [where], initializes it with [init]. Once initialization is done, cals [f]
    with the active client. All notifications are fed to [on_notification]*)
val client :
     Dune_rpc.Where.t
  -> Dune_rpc.Initialize.Request.t
  -> on_notification:(Client.t -> Dune_rpc.Call.t -> unit Fiber.t)
  -> f:(Client.t -> 'a Fiber.t)
  -> 'a Fiber.t

(** Like [client], but start with an already-established session. *)
val client_with_session :
     Dune_rpc.Initialize.Request.t
  -> session:Csexp_rpc.Session.t
  -> on_notification:(Client.t -> Dune_rpc.Call.t -> unit Fiber.t)
  -> f:(Client.t -> 'a Fiber.t)
  -> 'a Fiber.t

module Connect : sig
  (** [csexp_client t path] connects to [path] and returns the client.

      This is needed for implementing low level functions such as
      [$ dune rpc init] *)
  val csexp_client : Dune_rpc.Where.t -> Csexp_rpc.Client.t Fiber.t
end
