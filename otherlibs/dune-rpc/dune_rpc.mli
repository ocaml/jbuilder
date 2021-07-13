(** Implementation of the protocol used by dune rpc. Independent of IO and any
    specific rpc requests. The protocol described here is stable and is relied
    on by 3rd party clients.

    The implementation is loosely modelled on jsonrpc. It defines the following
    concepts:

    Session - An active rpc session

    Request - A unique id with a call sent by a client. A server must respond to
    every request

    Notification - A call send by a client. A server must not respond to a
    notification

    It contains hooks that make it possible to use with any custom scheduler
    that uses fibers

    The API in this version is versioned. When using this library, we expect
    that the module corresponding to a particular version is used exclusively.

    While we guarantee stability of the API, we reserve the right to:

    - Add optional arguments to functions
    - Add new fields to records
    - New variant constructors that will not cause runtime errors in existing
      user programs.

    This means that you must refrain from re-exporting any values, constructing
    any records, using any module types as functor arguments, or make non
    exhaustive matches an error to guarantee compatibility. *)

(* TODO make records private *)

module V1 : sig
  module Id : sig
    (** Id's for requests, responses, sessions.

        Id's are permitted to be arbtirary s-expressions to allow users pick
        descriptive tokens to ease debugging. *)

    type t

    val make : Csexp.t -> t
  end

  module Response : sig
    module Error : sig
      type kind =
        | Invalid_request
        | Code_error
        | Version_error

      type t

      val payload : t -> Csexp.t option

      val message : t -> string

      val kind : t -> kind

      exception E of t
    end

    type t = (Csexp.t, Error.t) result
  end

  module Initialize : sig
    type t

    val create : id:Id.t -> t
  end

  module Loc : sig
    type t

    val start : t -> Lexing.position

    val stop : t -> Lexing.position
  end

  module Target : sig
    type t =
      | Path of string
      | Alias of string
      | Library of string
      | Executables of string list
      | Preprocess of string list
      | Loc of Loc.t
  end

  module Diagnostic : sig
    type severity =
      | Error
      | Warning

    module Promotion : sig
      type t

      val in_build : t -> string

      val in_source : t -> string
    end

    module Id : sig
      type t

      val compare : t -> t -> int

      val hash : t -> int

      val create : int -> t
    end

    module Related : sig
      type t

      val loc : t -> Loc.t

      val message : t -> unit Pp.t
    end

    type t

    val related : t -> Related.t list

    val loc : t -> Loc.t option

    val id : t -> Id.t

    val message : t -> unit Pp.t

    val severity : t -> severity option

    val promotion : t -> Promotion.t list

    (* The list of targets is ordered such that the first element is the
       immediate ("innermost") target being built when the error was
       encountered, which was required by the next element, and so on. *)
    val targets : t -> Target.t list

    (* The directory from which the action producing the error was run, relative
       to the workspace root. This is often, but not always, the directory of
       the first target in [targets].

       If this is [None], then the error does not have an associated error (for
       example, if your opam installation is too old). *)
    val directory : t -> string option

    module Event : sig
      type nonrec t =
        | Add of t
        | Remove of t
    end
  end

  module Progress : sig
    type t =
      | Waiting
      | In_progress of
          { complete : int
          ; remaining : int
          }
      | Failed
      | Interrupted
      | Success
  end

  module Subscribe : sig
    type t =
      | Diagnostics
      | Build_progress
  end

  module Message : sig
    type t

    val payload : t -> Csexp.t option

    val message : t -> string
  end

  module Notification : sig
    type 'a t

    val subscribe : Subscribe.t t

    val unsubscribe : Subscribe.t t

    (** Request dune to shutdown. The current build job will be cancelled. *)
    val shutdown : unit t
  end

  module Request : sig
    type ('a, 'b) t

    val ping : (unit, unit) t

    val diagnostics : (unit, Diagnostic.t list) t
  end

  module Client : sig
    module type S = sig
      (** Rpc client *)

      type t

      type 'a fiber

      type chan

      module Handler : sig
        type t

        val create :
             ?log:(Message.t -> unit fiber)
          -> ?diagnostic:(Diagnostic.Event.t list -> unit fiber)
               (** Called whenever diagnostics are added or removed. When
                   subscribing to diagnostics, this function will immediately be
                   called with the current set of diagnostics. *)
          -> ?build_progress:(Progress.t -> unit fiber)
          -> ?abort:(Message.t -> unit fiber)
               (** If [abort] is called, the server has terminated the
                   connection due to a protcol error. This should never be
                   called unless there's a bug. *)
          -> unit
          -> t
      end

      (** [request ?id client decl req] send a request [req] specified by [decl]
          to [client]. If [id] is [None], it will be automatically generated. *)
      val request :
           ?id:Id.t
        -> t
        -> ('a, 'b) Request.t
        -> 'a
        -> ('b, Response.Error.t) result fiber

      val notification : t -> 'a Notification.t -> 'a -> unit fiber

      (** [disconnected client] produces a fiber that only becomes determined
          when the session is ended from the server side (such as if the build
          server is killed entirely). *)
      val disconnected : t -> unit fiber

      module Batch : sig
        type t

        type client

        val create : client -> t

        val request :
             ?id:Id.t
          -> t
          -> ('a, 'b) Request.t
          -> 'a
          -> ('b, Response.Error.t) result fiber

        val notification : t -> 'a Notification.t -> 'a -> unit

        val submit : t -> unit fiber
      end
      with type client := t

      (** [connect ?on_handler session init ~f] connect to [session], initialize
          with [init] and call [f] once the client is initialized. [handler] is
          called for some notifications sent to [session] *)
      val connect :
           ?handler:Handler.t
        -> chan
        -> Initialize.t
        -> f:(t -> 'a fiber)
        -> 'a fiber
    end

    (** Functor to create a client implementation *)
    module Make (Fiber : sig
      type 'a t

      val return : 'a -> 'a t

      val fork_and_join_unit : (unit -> unit t) -> (unit -> 'a t) -> 'a t

      val parallel_iter : (unit -> 'a option t) -> f:('a -> unit t) -> unit t

      val finalize : (unit -> 'a t) -> finally:(unit -> unit t) -> 'a t

      module O : sig
        val ( let* ) : 'a t -> ('a -> 'b t) -> 'b t

        val ( let+ ) : 'a t -> ('a -> 'b) -> 'b t
      end

      module Ivar : sig
        type 'a fiber

        type 'a t

        val create : unit -> 'a t

        val read : 'a t -> 'a fiber

        val fill : 'a t -> 'a -> unit fiber
      end
      with type 'a fiber := 'a t
    end) (Chan : sig
      type t

      (* [write t x] writes the s-expression when [x] is [Some _], and closes
         the session if [x = None] *)
      val write : t -> Csexp.t list option -> unit Fiber.t

      (* [read t] attempts to read from [t]. If an s-expression is read, it is
         returned as [Some sexp], otherwise [None] is returned and the session
         is closed. *)
      val read : t -> Csexp.t option Fiber.t
    end) : S with type 'a fiber := 'a Fiber.t and type chan := Chan.t
  end

  module Where : sig
    type t =
      [ `Unix of string
      | `Ip of [ `Host of string ] * [ `Port of int ]
      ]

    module type S = sig
      type 'a fiber

      val get : build_dir:string -> t option fiber

      val default : build_dir:string -> t
    end

    module Make (Fiber : sig
      type 'a t

      val return : 'a -> 'a t

      module O : sig
        val ( let* ) : 'a t -> ('a -> 'b t) -> 'b t

        val ( let+ ) : 'a t -> ('a -> 'b) -> 'b t
      end
    end) (Sys : sig
      val getenv : string -> string option

      val is_win32 : unit -> bool

      val read_file : string -> string Fiber.t

      val readlink : string -> string option Fiber.t

      val analyze_path :
        string -> [ `Unix_socket | `Normal_file | `Other ] Fiber.t
    end) : S with type 'a fiber := 'a Fiber.t
  end
end
