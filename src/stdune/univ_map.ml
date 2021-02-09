module Key = struct
  module Witness = struct
    type 'a t = ..
  end

  module type T = sig
    type t

    type 'a Witness.t += T : t Witness.t

    val id : int

    val name : string

    val to_dyn : t -> Dyn.t
  end

  type 'a t = (module T with type t = 'a)

  let next = ref 0

  let create (type a) ~name to_dyn =
    let n = !next in
    next := n + 1;
    let module M = struct
      type t = a

      type 'a Witness.t += T : t Witness.t

      let id = n

      let to_dyn = to_dyn

      let name = name
    end in
    (module M : T with type t = a)

  let id (type a) (module M : T with type t = a) = M.id

  let eq (type a b) (module A : T with type t = a)
      (module B : T with type t = b) : (a, b) Type_eq.t =
    match A.T with
    | B.T -> Type_eq.T
    | _ -> assert false
end

module Binding = struct
  type t = T : 'a Key.t * 'a -> t
end

type t = Binding.t Int.Map.t

let empty = Int.Map.empty

let is_empty = Int.Map.is_empty

let set (type a) t (key : a Key.t) x =
  let (module M) = key in
  let data = Binding.T (key, x) in
  Int.Map.set t M.id data

let add (type a) t (key : a Key.t) (x : a) : (t, a) Result.t =
  let (module M) = key in
  let data = Binding.T (key, x) in
  match Int.Map.add t M.id data with
  | Ok x -> Ok x
  | Error (Binding.T (key', x)) ->
    let eq = Key.eq key' key in
    Error (Type_eq.cast eq x)

let update (type a) t (key : a Key.t) ~f =
  let (module M) = key in
  Int.Map.update t M.id ~f:(function
    | None -> f None |> Option.map ~f:(fun x -> Binding.T (key, x))
    | Some (Binding.T (key', x)) ->
      let eq = Key.eq key' key in
      let x = Type_eq.cast eq x in
      f (Some x) |> Option.map ~f:(fun x -> Binding.T (key, x)))

let mem t key = Int.Map.mem t (Key.id key)

let remove t key = Int.Map.remove t (Key.id key)

let find t key =
  match Int.Map.find t (Key.id key) with
  | None -> None
  | Some (Binding.T (key', v)) ->
    let eq = Key.eq key' key in
    Some (Type_eq.cast eq v)

let find_exn t key =
  match Int.Map.find t (Key.id key) with
  | None -> failwith "Univ_map.find_exn"
  | Some (Binding.T (key', v)) ->
    let eq = Key.eq key' key in
    Type_eq.cast eq v

let singleton key v = Int.Map.singleton (Key.id key) (Binding.T (key, v))

let superpose = Int.Map.superpose

let to_dyn (t : t) =
  let open Dyn.Encoder in
  Dyn.Map
    (Int.Map.values t
    |> List.map ~f:(fun (Binding.T (key, v)) ->
           let (module K) = key in
           (string K.name, K.to_dyn v)))
