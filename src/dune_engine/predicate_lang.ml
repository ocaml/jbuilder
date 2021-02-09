open Stdune

type 'a t =
  | Element of 'a
  | Compl of 'a t
  | Standard
  | Union of 'a t list
  | Inter of 'a t list

let diff a = function
  | Union []
  | Inter [] ->
    a
  | b -> Inter [ a; Compl b ]

let inter a = Inter a

let compl a = Compl a

let union a = Union a

let not_union a = compl (union a)

let any = not_union []

let empty = Union []

let rec decode_one f =
  let open Dune_lang.Decoder in
  let bool_ops () =
    sum
      [ ("or", many f union [])
      ; ("and", many f inter [])
      ; ("not", many f not_union [])
      ]
  in
  let elt =
    let+ e = f in
    Element e
  in
  peek_exn >>= function
  | Atom (loc, A "\\") -> User_error.raise ~loc [ Pp.text "unexpected \\" ]
  | Atom (_, A "")
  | Quoted_string (_, _)
  | Template _ ->
    elt
  | Atom (loc, A s) -> (
    match s with
    | ":standard" -> junk >>> return Standard
    | ":include" ->
      User_error.raise ~loc
        [ Pp.text ":include isn't supported in the predicate language" ]
    | _ when s.[0] = ':' ->
      User_error.raise ~loc [ Pp.textf "undefined symbol %s" s ]
    | _ -> elt)
  | List (_, Atom (loc, A s) :: _) -> (
    match s with
    | ":include" ->
      User_error.raise ~loc
        [ Pp.text ":include isn't supported in the predicate language" ]
    | "or"
    | "and"
    | "not" ->
      bool_ops ()
    | s when s <> "" && s.[0] <> '-' && s.[0] <> ':' ->
      User_error.raise ~loc
        [ Pp.text
            "This atom must be quoted because it is the first element of a \
             list and doesn't start with - or:"
        ]
    | _ -> enter (many f union []))
  | List _ -> enter (many f union [])

and many f k acc =
  let open Dune_lang.Decoder in
  peek >>= function
  | None -> return (k (List.rev acc))
  | Some (Atom (_, A "\\")) ->
    junk >>> many f union [] >>| fun to_remove ->
    diff (k (List.rev acc)) to_remove
  | Some _ ->
    let* x = decode_one f in
    many f k (x :: acc)

and decode f = many f union []

let rec encode f =
  let open Dune_lang.Encoder in
  function
  | Element a -> f a
  | Compl a -> constr "not" (encode f) a
  | Standard -> string ":standard"
  | Union xs -> constr "or" (list (encode f)) xs
  | Inter xs -> constr "and" (list (encode f)) xs

let rec to_dyn f =
  let open Dyn.Encoder in
  function
  | Element a -> f a
  | Compl a -> constr "compl" [ to_dyn f a ]
  | Standard -> string ":standard"
  | Union xs -> constr "or" (List.map ~f:(to_dyn f) xs)
  | Inter xs -> constr "and" (List.map ~f:(to_dyn f) xs)

let rec exec t ~standard elem =
  match (t : _ t) with
  | Compl t -> not (exec t ~standard elem)
  | Element f -> elem f
  | Union xs -> List.exists ~f:(fun t -> exec t ~standard elem) xs
  | Inter xs -> List.for_all ~f:(fun t -> exec t ~standard elem) xs
  | Standard -> exec standard ~standard elem

module Glob = struct
  type glob = string -> bool

  type nonrec t = glob t

  let to_dyn t = to_dyn (fun _ -> Dyn.Encoder.string "opaque") t

  let decode : t Dune_lang.Decoder.t =
    let open Dune_lang.Decoder in
    decode (Glob.decode >>| Glob.test)

  let exec (t : t) ~standard elem = exec t ~standard (fun f -> f elem)

  let filter (t : t) ~standard elems =
    match t with
    | Inter []
    | Union [] ->
      []
    | _ -> List.filter elems ~f:(fun elem -> exec t ~standard elem)

  let of_glob g = Element (Glob.test g)

  let of_pred p = Element p

  let true_ = Element (fun _ -> true)

  let of_string_set s = Element (String.Set.mem s)
end
