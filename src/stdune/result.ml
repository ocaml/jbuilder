type ('a, 'error) t = ('a, 'error) result =
  | Ok of 'a
  | Error of 'error

let ok x = Ok x

let return = ok

let value r ~default =
  match r with
  | Ok v -> v
  | Error _ -> default

let is_ok = function
  | Ok _ -> true
  | Error _ -> false

let is_error = function
  | Ok _ -> false
  | Error _ -> true

let ok_exn = function
  | Ok x -> x
  | Error e -> raise e

let try_with f =
  match f () with
  | s -> Ok s
  | exception e -> Error e

let bind t ~f =
  match t with
  | Ok x -> f x
  | Error _ as t -> t

let ( >>= ) x f = bind x ~f

let map x ~f =
  match x with
  | Ok x -> Ok (f x)
  | Error _ as x -> x

let map_error x ~f =
  match x with
  | Ok _ as res -> res
  | Error x -> Error (f x)

let iter t ~f =
  match t with
  | Ok x -> f x
  | Error _ -> ()

let to_option = function
  | Ok p -> Some p
  | Error _ -> None

let errorf fmt = Printf.ksprintf (fun x -> Error x) fmt

let both a b =
  match a with
  | Error e -> Error e
  | Ok a -> (
    match b with
    | Error e -> Error e
    | Ok b -> Ok (a, b))

module O = struct
  let ( >>= ) t f = bind t ~f

  let ( >>| ) t f = map t ~f

  let ( let* ) = ( >>= )

  let ( let+ ) = ( >>| )

  let ( and+ ) = both
end

open O

type ('a, 'error) result = ('a, 'error) t

module List = struct
  let map t ~f =
    let rec loop acc = function
      | [] -> Ok (List.rev acc)
      | x :: xs -> f x >>= fun x -> loop (x :: acc) xs
    in
    loop [] t

  let all =
    let rec loop acc = function
      | [] -> Ok (List.rev acc)
      | t :: l -> t >>= fun x -> loop (x :: acc) l
    in
    fun l -> loop [] l

  let concat_map =
    let rec loop f acc = function
      | [] -> Ok (List.rev acc)
      | x :: l -> f x >>= fun y -> loop f (List.rev_append y acc) l
    in
    fun l ~f -> loop f [] l

  let rec iter t ~f =
    match t with
    | [] -> Ok ()
    | x :: xs -> f x >>= fun () -> iter xs ~f

  let rec fold_left t ~f ~init =
    match t with
    | [] -> Ok init
    | x :: xs -> f init x >>= fun init -> fold_left xs ~f ~init

  let filter_map t ~f =
    fold_left t ~init:[] ~f:(fun acc x ->
        f x >>| function
        | None -> acc
        | Some y -> y :: acc)
    >>| List.rev
end

let hash h1 h2 t =
  Stdlib.Hashtbl.hash
    (match t with
    | Ok s -> h1 s
    | Error e -> h2 e)

let equal e1 e2 x y =
  match (x, y) with
  | Ok x, Ok y -> e1 x y
  | Error x, Error y -> e2 x y
  | _, _ -> false

module Option = struct
  let iter t ~f =
    match t with
    | None -> Ok ()
    | Some x -> x >>= f
end

let to_dyn ok err = function
  | Ok e -> Dyn.Encoder.constr "Ok" [ ok e ]
  | Error e -> Dyn.Encoder.constr "Error" [ err e ]
