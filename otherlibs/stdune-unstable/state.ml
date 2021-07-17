module Make (S : sig
  type t
end)
(M : Monad_intf.S) =
struct
  module T = struct
    type 'a t = S.t -> (S.t * 'a) M.t

    let return a s = M.return (s, a)

    let bind x ~f s =
      let open M.O in
      let* s', a = x s in
      (f a) s'
  end

  open M.O
  include T

  let lift m s = m >>| fun a -> (s, a)

  let modify f s = M.return (f s, ())

  let get : S.t T.t = fun s -> M.return (s, s)

  let set s _ = M.return (s, ())

  let run t s = t s

  include Monad.Make (T)
end
