(** This module type is accessible as [Stdune.Monad.Basic] outside of [Stdune]. *)
module type Basic = sig
  type 'a t

  val return : 'a -> 'a t

  val bind : 'a t -> f:('a -> 'b t) -> 'b t
end

(** This module type is accessible as just [Stdune.Monad] outside of [Stdune]. *)
module type S = sig
  include Basic

  val map : 'a t -> f:('a -> 'b) -> 'b t

  module O : sig
    val ( >>| ) : 'a t -> ('a -> 'b) -> 'b t

    val ( >>= ) : 'a t -> ('a -> 'b t) -> 'b t

    val ( >>> ) : unit t -> 'a t -> 'a t

    val ( let+ ) : 'a t -> ('a -> 'b) -> 'b t

    val ( and+ ) : 'a t -> 'b t -> ('a * 'b) t

    val ( let* ) : 'a t -> ('a -> 'b t) -> 'b t

    val ( and* ) : 'a t -> 'b t -> ('a * 'b) t
  end
end

module type List = sig
  type 'a t

  val find_map : 'a list -> f:('a -> 'b option t) -> 'b option t

  val map : 'a list -> f:('a -> 'b t) -> 'b list t

  val concat_map : 'a list -> f:('a -> 'b list t) -> 'b list t

  val exists : 'a list -> f:('a -> bool t) -> bool t

  val iter : 'a list -> f:('a -> unit t) -> unit t

  val filter : 'a list -> f:('a -> bool t) -> 'a list t

  val filter_map : 'a list -> f:('a -> 'b option t) -> 'b list t

  val fold_left : 'a list -> f:('acc -> 'a -> 'acc t) -> init:'acc -> 'acc t

  val for_all : 'a list -> f:('a -> bool t) -> bool t
end

module type Option = sig
  type 'a t

  val iter : 'a option -> f:('a -> unit t) -> unit t
end
