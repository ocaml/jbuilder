(** Monad signatures *)

module type Basic = Monad_intf.Basic

module Make (M : Basic) : Monad_intf.S with type 'a t := 'a M.t
[@@inlined always]

module Id : Monad_intf.S with type 'a t = 'a

module List (M : Monad_intf.S) : Monad_intf.List with type 'a t := 'a M.t
