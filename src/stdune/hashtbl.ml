module type S = Hashtbl_intf.S

module Make (H : sig
  include Hashable.S

  val to_dyn : t -> Dyn.t
end) =
struct
  include MoreLabels.Hashtbl.Make (H)

  let[@ocaml.warning "-32"] add = `Use_set

  let find = find_opt

  let find_exn t key =
    match find_opt t key with
    | Some v -> v
    | None -> Code_error.raise "Hashtbl.find_exn" [ ("key", H.to_dyn key) ]

  let set t key data = replace t ~key ~data

  let find_or_add t key ~f =
    match find t key with
    | Some x -> x
    | None ->
      let x = f key in
      set t key x;
      x

  let foldi t ~init ~f = fold t ~init ~f:(fun ~key ~data acc -> f key data acc)

  let fold t ~init ~f = foldi t ~init ~f:(fun _ x -> f x)

  let of_list l =
    let h = create (List.length l) in
    let rec loop = function
      | [] -> Result.Ok h
      | (k, v) :: xs -> (
        match find h k with
        | None ->
          set h k v;
          loop xs
        | Some v' -> Error (k, v', v))
    in
    loop l

  let of_list_exn l =
    match of_list l with
    | Result.Ok h -> h
    | Error (key, _, _) ->
      Code_error.raise "Hashtbl.of_list_exn duplicate keys"
        [ ("key", H.to_dyn key) ]

  let add_exn t key data =
    match find t key with
    | None -> set t key data
    | Some _ ->
      Code_error.raise "Hastbl.add_exn: key already exists"
        [ ("key", H.to_dyn key) ]

  let add t key data =
    match find t key with
    | None ->
      set t key data;
      Result.Ok ()
    | Some p -> Result.Error p

  let keys t = foldi t ~init:[] ~f:(fun key _ acc -> key :: acc)

  let to_dyn f t =
    Dyn.Map
      (foldi t ~init:[] ~f:(fun key data acc -> (H.to_dyn key, f data) :: acc)
      |> List.sort ~compare:(fun (k, _) (k', _) -> Dyn.compare k k'))

  let filteri_inplace t ~f =
    (* Surely there's a more performant way of writing this. (e.g. using
       filter_map_inplace), but starting with a simple thing for now, in part
       because [filter_map_inplace] is not available in 4.02. *)
    let to_delete = ref [] in
    iter t ~f:(fun ~key ~data ->
        match f ~key ~data with
        | false -> to_delete := key :: !to_delete
        | true -> ());
    List.iter !to_delete ~f:(fun k -> remove t k)

  let iter t ~f = iter t ~f:(fun ~key:_ ~data -> f data)
end

let hash = MoreLabels.Hashtbl.hash
