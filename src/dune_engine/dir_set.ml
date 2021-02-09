open! Stdune

type t0 =
  | Empty
  | Universal
  | Nontrivial of nontrivial

and nontrivial =
  { default : bool
  ; here : bool
  ; exceptions : t0 String.Map.t
  }

type _ t = t0

let here = function
  | Empty -> false
  | Universal -> true
  | Nontrivial t -> t.here

let default = function
  | Empty -> false
  | Universal -> true
  | Nontrivial t -> t.default

let exceptions = function
  | Empty
  | Universal ->
    String.Map.empty
  | Nontrivial t -> t.exceptions

let empty = Empty

let universal = Universal

let trivial = function
  | false -> Empty
  | true -> Universal

let create ~default ~here ~exceptions =
  if String.Map.is_empty exceptions && here = default then
    trivial default
  else
    Nontrivial { here; default; exceptions }

let is_empty = function
  | Empty -> true
  | _ -> false

let is_universal = function
  | Universal -> true
  | _ -> false

let merge_exceptions a b ~default ~f =
  String.Map.merge a.exceptions b.exceptions ~f:(fun _ x y ->
      let x = Option.value x ~default:(trivial a.default) in
      let y = Option.value y ~default:(trivial b.default) in
      match (default, f x y) with
      | false, Empty
      | true, Universal ->
        None
      | _, res -> Some res)

let merge_nontrivial a b ~f_one ~f_set =
  let default = f_one a.default b.default in
  create ~here:(f_one a.here b.here) ~default
    ~exceptions:(merge_exceptions a b ~default ~f:f_set)
  [@@inline always]

let rec union x y =
  match (x, y) with
  | Empty, v
  | v, Empty ->
    v
  | Universal, _
  | _, Universal ->
    Universal
  | Nontrivial x, Nontrivial y ->
    merge_nontrivial x y ~f_one:( || ) ~f_set:union

let rec inter x y =
  match (x, y) with
  | Universal, v
  | v, Universal ->
    v
  | Empty, _
  | _, Empty ->
    Empty
  | Nontrivial x, Nontrivial y ->
    merge_nontrivial x y ~f_one:( && ) ~f_set:inter

let rec negate x =
  match x with
  | Universal -> Empty
  | Empty -> Universal
  | Nontrivial { here; default; exceptions } ->
    Nontrivial
      { here = not here
      ; default = not default
      ; exceptions = String.Map.map exceptions ~f:negate
      }

let rec diff x y =
  match x with
  | Empty -> Empty
  | Universal -> negate y
  | Nontrivial nx -> (
    match y with
    | Empty -> x
    | Universal -> Empty
    | Nontrivial ny ->
      merge_nontrivial nx ny ~f_one:(fun a b -> a && not b) ~f_set:diff)

let rec mem t dir =
  match t with
  | Empty -> false
  | Universal -> true
  | Nontrivial { here; default; exceptions } -> (
    match dir with
    | [] -> here
    | child :: rest -> (
      match String.Map.find exceptions child with
      | None -> default
      | Some t -> mem t rest))

let mem t dir = mem t (Path.Local_gen.explode dir)

let descend t child =
  match t with
  | Empty -> Empty
  | Universal -> Universal
  | Nontrivial { here = _; default; exceptions } -> (
    match String.Map.find exceptions child with
    | None -> trivial default
    | Some t -> t)

let union_all = List.fold_left ~init:empty ~f:union

let inter_all = List.fold_left ~init:empty ~f:inter

let of_subtree_gen =
  let rec loop subtree = function
    | [] -> subtree
    | component :: rest ->
      create ~here:false ~default:false
        ~exceptions:(String.Map.singleton component (loop subtree rest))
  in
  loop

let just_the_root =
  Nontrivial { here = true; default = false; exceptions = String.Map.empty }

let subtree' = of_subtree_gen universal

let singleton' = of_subtree_gen just_the_root

let subtree p = subtree' (Path.Local_gen.explode p)

let singleton p = singleton' (Path.Local_gen.explode p)

let rec is_subset t ~of_ =
  match (t, of_) with
  | _, Universal
  | Empty, _ ->
    true
  | Universal, _
  | _, Empty ->
    false
  | Nontrivial x, Nontrivial y ->
    ((not x.here) || y.here)
    && ((not x.default) || y.default)
    && String.Map.is_subset x.exceptions ~of_:y.exceptions ~f:is_subset

let rec to_dyn =
  let open Dyn.Encoder in
  function
  | Empty -> constr "Empty" []
  | Universal -> constr "Universal" []
  | Nontrivial { here; default; exceptions } ->
    let open Dyn in
    List
      (((match here with
        | true -> [ (".", String "true") ]
        | false -> [])
       @ (String.Map.to_list exceptions
         |> List.map ~f:(fun (s, t) -> (s, to_dyn t)))
       @
       match default with
       | false -> []
       | true -> [ ("*", String "Universal") ])
      |> List.map ~f:(fun (k, v) -> List [ String k; v ]))

let forget_root t = t

type toplevel_subdirs =
  | Infinite
  | Finite of String.Set.t

let toplevel_subdirs t =
  match t with
  | Universal -> Infinite
  | Empty -> Finite String.Set.empty
  | Nontrivial t ->
    if t.default then
      Infinite
    else
      Finite (String.Set.of_list (String.Map.keys t.exceptions))

let of_list paths =
  union_all (List.map paths ~f:(fun p -> singleton' (Path.Local_gen.explode p)))
