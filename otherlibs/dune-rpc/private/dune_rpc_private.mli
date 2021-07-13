module Conv : module type of Conv

module Id : sig
  type t

  val sexp : t Conv.value

  val to_dyn : t -> Stdune.Dyn.t

  val equal : t -> t -> bool

  val hash : t -> int

  val make : Csexp.t -> t

  val to_sexp : t -> Csexp.t

  module Set : Stdune.Set.S with type elt = t
end

module Call : sig
  type t =
    { method_ : string
    ; params : Csexp.t
    }

  val create : ?params:Csexp.t -> method_:string -> unit -> t
end

module Request : sig
  type t = Id.t * Call.t
end

module Response : sig
  module Error : sig
    type kind =
      | Invalid_request
      | Code_error
      | Version_error

    type t =
      { payload : Csexp.t option
      ; message : string
      ; kind : kind
      }

    val payload : t -> Csexp.t option

    val message : t -> string

    val kind : t -> kind

    exception E of t

    val to_dyn : t -> Stdune.Dyn.t

    val of_conv : Conv.error -> t

    val create : ?payload:Csexp.t -> kind:kind -> message:string -> unit -> t
  end

  type t = (Csexp.t, Error.t) result
end

module Initialize : sig
  module Request : sig
    type t =
      { version : int * int
      ; id : Id.t
      }

    val create : id:Id.t -> t

    val version : t -> int * int

    val id : t -> Id.t

    val of_call : Call.t -> version:int * int -> (t, Response.Error.t) result
  end

  module Response : sig
    type t

    val create : unit -> t

    val to_response : t -> Csexp.t

    val sexp : t Conv.value
  end
end

module Decl : sig
  type ('req, 'resp) request =
    { method_ : string
    ; req : 'req Conv.value
    ; resp : 'resp Conv.value
    }

  type 'req notification =
    { method_ : string
    ; req : 'req Conv.value
    }

  val notification : method_:string -> 'a Conv.value -> 'a notification

  val request :
    method_:string -> 'a Conv.value -> 'b Conv.value -> ('a, 'b) request
end

module Where : sig
  type t =
    [ `Unix of string
    | `Ip of [ `Host of string ] * [ `Port of int ]
    ]

  val of_string : string -> t

  val to_string : t -> string

  val add_to_env : t -> Stdune.Env.t -> Stdune.Env.t

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

module Loc : sig
  type t = Stdune.Loc.t =
    { start : Lexing.position
    ; stop : Lexing.position
    }

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
    type t =
      { in_build : string
      ; in_source : string
      }

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
    type t =
      { message : unit Pp.t
      ; loc : Loc.t
      }

    val message : t -> unit Pp.t

    val loc : t -> Loc.t
  end

  type t =
    { targets : Target.t list
    ; id : Id.t
    ; message : unit Pp.t
    ; loc : Loc.t option
    ; severity : severity option
    ; promotion : Promotion.t list
    ; directory : string option
    ; related : Related.t list
    }

  val related : t -> Related.t list

  val id : t -> Id.t

  val loc : t -> Loc.t option

  val message : t -> unit Pp.t

  val severity : t -> severity option

  val promotion : t -> Promotion.t list

  val targets : t -> Target.t list

  val directory : t -> string option

  module Event : sig
    type nonrec t =
      | Add of t
      | Remove of t

    val to_dyn : t -> Stdune.Dyn.t
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

module Message : sig
  type t =
    { payload : Csexp.t option
    ; message : string
    }

  val payload : t -> Csexp.t option

  val message : t -> string
end

module Subscribe : sig
  type t =
    | Diagnostics
    | Build_progress
end

module Client : sig
  module type S = sig
    type t

    type 'a fiber

    type chan

    val request :
         ?id:Id.t
      -> t
      -> ('a, 'b) Decl.request
      -> 'a
      -> ('b, Response.Error.t) result fiber

    val notification : t -> 'a Decl.notification -> 'a -> unit fiber

    val disconnected : t -> unit fiber

    module Batch : sig
      type t

      type client

      val create : client -> t

      val request :
           ?id:Id.t
        -> t
        -> ('a, 'b) Decl.request
        -> 'a
        -> ('b, Response.Error.t) result fiber

      val notification : t -> 'a Decl.notification -> 'a -> unit

      val submit : t -> unit fiber
    end
    with type client := t

    module Handler : sig
      type t

      val create :
           ?log:(Message.t -> unit fiber)
        -> ?diagnostic:(Diagnostic.Event.t list -> unit fiber)
        -> ?build_progress:(Progress.t -> unit fiber)
        -> ?abort:(Message.t -> unit fiber)
        -> unit
        -> t
    end

    val connect :
         ?handler:Handler.t
      -> chan
      -> Initialize.Request.t
      -> f:(t -> 'a fiber)
      -> 'a fiber
  end

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

    val write : t -> Csexp.t list option -> unit Fiber.t

    val read : t -> Csexp.t option Fiber.t
  end) : S with type 'a fiber := 'a Fiber.t and type chan := Chan.t
end

module Packet : sig
  module Reply : sig
    type t =
      | Response of (Id.t * Response.t)
      | Notification of Call.t

    val sexp : t Conv.value
  end

  module Query : sig
    type t =
      | Request of Request.t
      | Notification of Call.t

    val sexp : t Conv.value
  end
end

module Version : sig
  type t = int * int

  val latest : t

  val sexp : t Conv.value
end

module Public : sig
  (** Public requests and notifications *)

  module Request : sig
    type ('a, 'b) t = ('a, 'b) Decl.request

    val ping : (unit, unit) t

    val diagnostics : (unit, Diagnostic.t list) t
  end

  module Notification : sig
    type 'a t = 'a Decl.notification

    val subscribe : Subscribe.t t

    val unsubscribe : Subscribe.t t

    val shutdown : unit t
  end
end

module Server_notifications : sig
  (** Notification sent from server to client *)

  val diagnostic : Diagnostic.Event.t list Decl.notification

  val progress : Progress.t Decl.notification

  val log : Message.t Decl.notification

  val abort : Message.t Decl.notification
end
