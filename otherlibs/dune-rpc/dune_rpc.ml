module V1 = struct
  module Sexp = Stdune.Sexp
  open Dune_rpc_private
  module Id = Id
  module Response = Response
  module Initialize = Initialize.Request
  module Call = Call
  module Loc = Loc
  module Target = Target
  module Diagnostic = Diagnostic
  module Progress = Progress
  module Subscribe = Subscribe
  module Message = Message
  module Where = Where
  include Public

  module Client = struct
    module type S = sig
      type t

      type 'a fiber

      type chan

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

      val request :
           ?id:Id.t
        -> t
        -> ('a, 'b) Request.t
        -> 'a
        -> ('b, Response.Error.t) result fiber

      val notification : t -> 'a Notification.t -> 'a -> unit fiber

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

      val connect :
           ?handler:Handler.t
        -> chan
        -> Initialize.t
        -> f:(t -> 'a fiber)
        -> 'a fiber
    end

    module Make = Client.Make
  end
end
