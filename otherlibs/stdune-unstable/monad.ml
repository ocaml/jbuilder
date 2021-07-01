module type Basic = Monad_intf.Basic

module Make (M : Monad_intf.Basic) = struct
  include M

  let map t ~f = bind t ~f:(fun x -> return (f x))

  module O = struct
    let ( >>= ) t f = bind t ~f

    let ( >>| ) t f = map t ~f

    let ( >>> ) a b = bind a ~f:(fun () -> b)

    let ( let+ ) t f = map t ~f

    let ( and+ ) x y =
      let open M in
      x >>= fun x ->
      y >>= fun y -> return (x, y)

    let ( let* ) t f = bind t ~f

    let ( and* ) = ( and+ )
  end
end
[@@inlined always]

module Id = Make (struct
  type 'a t = 'a

  let return x = x

  let bind x ~f = f x
end)

module List (M : Monad_intf.S) = struct
  open M
  open M.O

  let rec find_map xs ~f =
    match xs with
    | [] -> return None
    | x :: xs -> (
      let* x = f x in
      match x with
      | None -> find_map xs ~f
      | Some s -> return (Some s))

  let rec fold_left xs ~f ~init =
    match xs with
    | [] -> return init
    | x :: xs ->
      let* init = f init x in
      fold_left xs ~f ~init

  let filter_map xs ~f =
    let rec loop acc = function
      | [] -> return (List.rev acc)
      | x :: xs -> (
        let* y = f x in
        match y with
        | None -> loop acc xs
        | Some y -> loop (y :: acc) xs)
    in
    loop [] xs

  let filter xs ~f =
    filter_map xs ~f:(fun x ->
        let+ pred = f x in
        Option.some_if pred x)

  let exists xs ~f =
    let+ res =
      find_map xs ~f:(fun x ->
          let+ x = f x in
          Option.some_if x ())
    in
    Option.is_some res

  let map xs ~f =
    filter_map xs ~f:(fun x ->
        let+ x = f x in
        Some x)

  let concat_map xs ~f = map xs ~f >>| List.concat

  let rec iter xs ~f =
    match xs with
    | [] -> return ()
    | x :: xs ->
      let* () = f x in
      iter xs ~f
end
