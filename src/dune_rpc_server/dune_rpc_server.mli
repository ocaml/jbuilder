open Stdune
open Dune_rpc_private

module Session : sig
  (** Session representing a connected client with a custom state. *)
  type 'a t

  (* [initialize session] returns the initialize request used to initialize this
     session *)
  val initialize : _ t -> Initialize.Request.t

  (** [get session] returns the current session state. It is an error to access
      the state after [on_terminate] finishes. *)
  val get : 'a t -> 'a

  (** [get session a] sets the curent state to [a].*)
  val set : 'a t -> 'a -> unit

  val active : _ t -> bool

  (** [notification session n a] Send notification [a] defined by [n] to
      [session] *)
  val notification : _ t -> 'a Decl.notification -> 'a -> unit Fiber.t

  val compare : 'a t -> 'a t -> Ordering.t

  val request_close : 'a t -> unit Fiber.t

  val to_dyn : ('a -> Dyn.t) -> 'a t -> Dyn.t
end

module Subscription : sig
  (** Represents a live subscription for values of type ['a] *)

  type 'a t

  val compare : 'a t -> 'a t -> Ordering.t

  (** Send the next value in the subscription *)
  val update : 'a t -> 'a -> unit Fiber.t

  (** Signal that this subscription is terminated. It is an error to call
      [update] after [finish]. Subsequent calls to [finish ] do nothing. *)
  val finish : _ t -> unit Fiber.t

  (** Checks if the subscription is still active. A subscription can be
      terminated either by calling [finish] or waiting for the client to
      terminate it. It is illegal to call [update] if [is_alive] returns [false] *)
  val active : _ t -> bool

  val finished : _ t -> unit Fiber.t
end

module Handler : sig
  (** A handler defines everything necessary to serve a session. It contains

      - A way to defined requests and notifications
      - A hook for handling session initiation/termination *)

  type 'a t

  (** information about a request or notification *)
  type info

  (** this request/notification is private and will be only be used internally
      by dune *)
  val private_ : info

  (** [public ?until ~since ()] request/notification available [since] and until
      [until] (if it's provided) *)
  val public : ?until:int * int -> since:int * int -> unit -> info

  val create :
       ?on_terminate:('a Session.t -> unit Fiber.t)
         (** Termination hook. It is guaranteed that the session state will not
             modified after this function is called *)
    -> on_init:('a Session.t -> Initialize.Request.t -> 'a Fiber.t)
         (** Initiation hook. It's guaranteed to be called before any
             requests/notifications. It's job is to initialize the session
             state. *)
    -> version:int * int
         (** version of the rpc. it's expected to support all earlier versions *)
    -> unit
    -> 'a t

  (** Callback function for requests or notifications *)
  type ('state, 'input, 'output) callback

  (** Create a callback that can access the input and current session. *)
  val callback' :
    info -> ('s Session.t -> 'a -> 'b Fiber.t) -> ('s, 'a, 'b) callback

  (** Create a callback that can only access the input. XXX perhaps we don't
      need this? *)
  val callback : info -> ('a -> 'b Fiber.t) -> ('s, 'a, 'b) callback

  (** [request handler callback decl] Add a request to [handler] using
      [callback] as the implementation and [decl] as the metadata *)
  val request : 's t -> ('s, 'a, 'b) callback -> ('a, 'b) Decl.request -> unit

  (** [notification handler callback decl] Add a notification to [handler] using
      [callback] as the implementation and [decl] as the metadata *)
  val notification :
    's t -> ('s, 'a, unit) callback -> 'a Decl.notification -> unit

  val subscription :
       's t
    -> ('init, 'diff) Sub.t
    -> on_subscribe:('s Session.t -> 'init Fiber.t)
    -> subscription:
         ('s Session.t -> 'init -> 'diff Subscription.t -> unit Fiber.t)
    -> unit
end

type t

val make : 'a Handler.t -> t

(** Functor to create a server implementation *)
module Make (S : sig
  type t

  (* [write t x] writes the s-expression when [x] is [Some _], and closes the
     session if [x = None] *)
  val write : t -> Sexp.t list option -> unit Fiber.t

  (* [read t] attempts to read from [t]. If an s-expression is read, it is
     returned as [Some sexp], otherwise [None] is returned and the session is
     closed. *)
  val read : t -> Sexp.t option Fiber.t
end) : sig
  (** [serve sessions handler] serve all [sessions] using [handler] *)
  val serve : S.t Fiber.Stream.In.t -> Dune_stats.t option -> t -> unit Fiber.t
end
