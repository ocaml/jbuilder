module Make (S : sig
  type t
end)
(M : Monad_intf.S) : sig
  include Monad_intf.S

  val run : 'a t -> S.t -> (S.t * 'a) M.t

  val get : S.t t

  val set : S.t -> unit t

  val lift : 'a M.t -> 'a t

  val modify : (S.t -> S.t) -> unit t
end
