type ('a, 'b) t = T : ('a, 'a) t

let cast (type a b) (T : (a, b) t) (x : a) : b = x

type ('a, 'b) eq = ('a, 'b) t

module Id = struct
  type _ w = ..

  module type T = sig
    type a

    type _ w += W : a w
  end

  type 'a t = (module T with type a = 'a)

  let hash (type a) ((module T) : a t) = Poly.hash T.W

  let equal (type a b) ((module M1) : a t) ((module M2) : b t) =
    match M1.W with
    | M2.W -> true
    | _ -> false

  let create (type a) () : a t =
    (module struct
      type nonrec a = a

      type _ w += W : a w
    end)

  let same (type a b) ((module M1) : a t) ((module M2) : b t) =
    match M1.W with
    | M2.W -> Some (T : (a, b) eq)
    | _ -> None
end
