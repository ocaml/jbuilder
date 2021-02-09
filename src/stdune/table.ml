module type Hashtbl = sig
  module Key : sig
    type t

    val equal : t -> t -> bool

    val hash : t -> int

    val to_dyn : t -> Dyn.t
  end

  module H : module type of Hashtbl.Make (Key)

  type value

  val value : value H.t
end

type ('input, 'output) t =
  (module Hashtbl with type value = 'output and type Key.t = 'input)

module type Key = sig
  type t

  val equal : t -> t -> bool

  val hash : t -> int

  val to_dyn : t -> Dyn.t
end

let create (type key value) (module Key : Key with type t = key) size :
    (key, value) t =
  (module struct
    module Key = Key
    module H = Hashtbl.Make (Key)

    type nonrec value = value

    let value = H.create size
  end)

let find (type input output) ((module T) : (input, output) t) x =
  T.H.find T.value x

let find_exn (type input output) (t : (input, output) t) key =
  let (module T) = t in
  match find t key with
  | Some v -> v
  | None ->
    Code_error.raise "Table.find_exn: key doesn't exist"
      [ ("key", T.Key.to_dyn key) ]

let set (type input output) ((module T) : (input, output) t) k v =
  T.H.set T.value k v

let add_exn (type input output) (t : (input, output) t) k v =
  let (module T) = t in
  match find t k with
  | None -> set t k v
  | Some _ ->
    Code_error.raise "Table.add_exn: key already exists"
      [ ("key", T.Key.to_dyn k) ]

let add t k v =
  match find t k with
  | None ->
    set t k v;
    Result.Ok ()
  | Some e -> Error e

let clear (type input output) ((module T) : (input, output) t) =
  T.H.clear T.value

let mem (type input output) ((module T) : (input, output) t) k =
  T.H.mem T.value k

let keys (type input output) ((module T) : (input, output) t) = T.H.keys T.value

let foldi (type input output) ((module T) : (input, output) t) ~init ~f =
  T.H.foldi T.value ~init ~f

let fold (type input output) ((module T) : (input, output) t) ~init ~f =
  T.H.fold T.value ~init ~f

let to_dyn (type input output) (f : output -> Dyn.t)
    ((module T) : (input, output) t) =
  T.H.to_dyn f T.value

let find_or_add (type input output) ((module T) : (input, output) t) (k : input)
    ~f =
  T.H.find_or_add T.value k ~f

let remove (type input output) ((module T) : (input, output) t) k =
  T.H.remove T.value k

let iter (type input output) ((module T) : (input, output) t) ~f =
  T.H.iter T.value ~f

let filteri_inplace (type input output) ((module T) : (input, output) t) ~f =
  T.H.filteri_inplace T.value ~f

let length (type input output) ((module T) : (input, output) t) =
  T.H.length T.value
