(** Error meant for humans *)

(** A data structure carrying extra information that can be attached to errors.

    This is mostly used to provide further context, such as location information
    for the RPC. *)
module Annotations : sig
  module Entry : sig
    type 'a t

    module type S = sig
      type payload

      val entry : payload t
    end

    module Make (Payload : sig
      type t

      val name : string

      val to_dyn : t -> Dyn.t
    end) : S with type payload = Payload.t
  end

  type t

  val none : t

  val annotate : t -> 'a Entry.t -> 'a -> t

  val singleton : 'a Entry.t -> 'a -> t

  val lookup : t -> 'a Entry.t -> 'a option

  val is_empty : t -> bool

  (** The message has a location embed in the text. *)
  module Has_embedded_location : Entry.S with type payload = unit
end

(** User errors are errors that users need to fix themselves in order to make
    progress. Since these errors are read by users, they should be simple to
    understand for people who are not familiar with the dune codebase. *)
exception E of User_message.t * Annotations.t

(** Raise a user error. The arguments are interpreted in the same way as
    [User_message.make]. The first paragraph is prefixed with "Error:". *)
val raise :
     ?loc:Loc0.t
  -> ?hints:User_message.Style.t Pp.t list
  -> ?annots:Annotations.t
  -> User_message.Style.t Pp.t list
  -> _

(** Create a user error. *)
val make :
     ?loc:Loc0.t
  -> ?hints:User_message.Style.t Pp.t list
  -> User_message.Style.t Pp.t list
  -> User_message.t

(** The "Error:" prefix *)
val prefix : User_message.Style.t Pp.t

(** Returns [true] if the message has an explicit location or one embed in the
    text. *)
val has_location : User_message.t -> Annotations.t -> bool

(** Returns [true] if the following list of annotations contains
    [Annot.Has_embedded_location]. *)
val has_embed_location : Annotations.t -> bool
