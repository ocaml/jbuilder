open Import

(* Mini clone of Dune_lang.Decoder. Main advantage is that it forbids all the
   crazy stuff and is automatically bi-directional *)

(* TODO error handling is complete crap for now.

   This should be unified with [Dune_lang.Decoder] eventually. *)

type error =
  | Parse_error of
      { message : string
      ; payload : (string * Sexp.t) list
      }
  | Version_error of
      { since : int * int
      ; until : (int * int) option
      ; message : string
      ; payload : (string * Sexp.t) list
      }

let dyn_of_error =
  let open Dyn.Encoder in
  function
  | Version_error { message; payload; until; since } ->
    record
      [ ("message", string message)
      ; ("payload", list (pair string Sexp.to_dyn) payload)
      ; ("until", option (pair int int) until)
      ; ("since", (pair int int) since)
      ]
  | Parse_error { message; payload } ->
    record
      [ ("message", string message)
      ; ("payload", list (pair string Sexp.to_dyn) payload)
      ]

exception Of_sexp of error

let raise_of_sexp ?(payload = []) message =
  raise (Of_sexp (Parse_error { message; payload }))

let raise_version_error ?until ?(payload = []) ~since message =
  raise (Of_sexp (Version_error { since; until; message; payload }))

let () =
  Printexc.register_printer (function
    | Of_sexp (Parse_error { message; payload }) ->
      Some (message ^ " " ^ Sexp.to_string (Sexp.record payload))
    | _ -> None)

module Fields = struct
  type t = Unparsed of Sexp.t String.Map.t

  let check_empty (Unparsed s) =
    if not (String.Map.is_empty s) then
      let payload =
        [ ( "unparsed"
          , Sexp.List
              (String.Map.to_list s
              |> List.map ~f:(fun (k, v) -> Sexp.List [ Sexp.Atom k; v ])) )
        ]
      in
      raise_of_sexp ~payload "unexpected fields"

  let empty = Unparsed String.Map.empty

  let merge (Unparsed a) (Unparsed b) =
    Unparsed
      (String.Map.union a b ~f:(fun _ _ _ ->
           (* field names are guaranteed to be different at construction time in
              [Both] *)
           assert false))

  let of_field name sexp = Unparsed (String.Map.singleton name sexp)

  let of_sexp (x : Sexp.t) =
    match x with
    | Atom _ -> raise_of_sexp "Unexpected atom"
    | List x -> (
      match
        String.Map.of_list_map x ~f:(function
          | List [ Atom s; v ] -> (s, v)
          | _ -> raise_of_sexp "unable to read field")
      with
      | Error (s, _, _) ->
        raise_of_sexp "duplicate fields" ~payload:[ ("field", Atom s) ]
      | Ok s -> Unparsed s)

  let optional (Unparsed t) name =
    match String.Map.find t name with
    | None -> (None, Unparsed t)
    | Some v -> (Some v, Unparsed (String.Map.remove t name))

  let required t name =
    let r, t = optional t name in
    match r with
    | Some s -> (s, t)
    | None ->
      raise_of_sexp "missing required field" ~payload:[ ("name", Atom name) ]

  let to_sexp (Unparsed t) : Sexp.t =
    List
      (String.Map.to_list t
      |> List.map ~f:(fun (k, v) -> Sexp.List [ Atom k; v ]))
end

type values = Sexp.t

type fields = Fields.t

type version =
  { since : int * int
  ; until : (int * int) option
  }

type ('a, 'kind) t =
  | Iso : ('a, 'kind) t * ('a -> 'b) * ('b -> 'a) -> ('b, 'kind) t
  | Version : ('a, 'kind) t * version -> ('a, 'kind) t
  | Both :
      (* Invariant: field names must be different *)
      ('a, fields) t
      * ('b, fields) t
      -> ('a * 'b, fields) t
  | Sexp : (Sexp.t, values) t
  | List : ('a, values) t -> ('a list, values) t
  | Field : string * 'a field -> ('a, fields) t
  | Enum : (string * 'a) list -> ('a, values) t
  | Sum : 'a econstr list * ('a -> case) -> ('a, values) t
  | Pair : ('a, values) t * ('b, values) t -> ('a * 'b, values) t
  | Triple :
      ('a, values) t * ('b, values) t * ('c, values) t
      -> ('a * 'b * 'c, values) t
  | Fdecl : ('a, 'k) t Fdecl.t -> ('a, 'k) t
  | Either :
      (* Invariant: field names must be different *)
      ('a, fields) t
      * ('b, fields) t
      -> (('a, 'b) Either.t, fields) t
  | Record : ('a, fields) t -> ('a, values) t

and ('a, 'arg) constr =
  { (* TODO allow consturctors without an argument *)
    name : string
  ; arg : ('arg, values) t
  ; inj : 'arg -> 'a
  }

and 'a econstr = Constr : ('a, 'arg) constr -> 'a econstr

and case = Case : 'arg * ('a, 'arg) constr -> case

and 'a field =
  | Required : ('a, values) t -> 'a field
  | Optional : ('a, values) t -> 'a option field

and 'k ret =
  | Values : values ret
  | Fields : Fields.t -> fields ret

type 'a value = ('a, values) t

let case a c = Case (a, c)

let constr name arg inj = { name; arg; inj }

let econstr c = Constr c

let both x y = Both (x, y)

let list x = List x

let sum x y = Sum (x, y)

let pair x y = Pair (x, y)

let triple x y z = Triple (x, y, z)

let discard_values ((a, x) : _ * values ret) =
  match (x : values ret) with
  | Values -> a

let string =
  Iso
    ( Sexp
    , (function
      | Atom s -> s
      | List _ as list ->
        raise_of_sexp ~payload:[ ("list", list) ]
          "string: expected atom. receieved list")
    , fun s -> Atom s )

let int =
  Iso
    ( Sexp
    , (function
      | List _ as list ->
        raise_of_sexp ~payload:[ ("list", list) ]
          "int: expected atom. received list"
      | Atom s -> (
        match Int.of_string s with
        | None -> raise_of_sexp "unable to read int"
        | Some i -> i))
    , fun s -> Atom (Int.to_string s) )

let unit =
  Iso
    ( Sexp
    , (function
      | List [] -> ()
      | _ -> raise_of_sexp "expected empty list")
    , fun () -> List [] )

let option x =
  let none = constr "None" unit (fun () -> None) in
  let some = constr "Some" x (fun x -> Some x) in
  sum [ econstr none; econstr some ] (function
    | None -> case () none
    | Some s -> case s some)

let char =
  Iso
    ( Sexp
    , (function
      | Atom s ->
        if String.length s = 1 then
          s.[0]
        else
          raise_of_sexp "expected only a single character"
      | List _ -> raise_of_sexp "expected a string of length 1")
    , fun c -> Atom (String.make 1 c) )

let to_sexp : 'a. ('a, values) t -> 'a -> Sexp.t =
 fun t a ->
  let rec loop : type a k. (a, k) t -> a -> k =
   fun t a ->
    match t with
    | Sexp -> a
    | Version (t, _) -> loop t a
    | Fdecl t -> loop (Fdecl.get t) a
    | List t -> List (List.map a ~f:(loop t))
    | Pair (x, y) ->
      let a, b = a in
      List [ loop x a; loop y b ]
    | Triple (x, y, z) ->
      let a, b, c = a in
      List [ loop x a; loop y b; loop z c ]
    | Record r ->
      let fields = loop r a in
      Fields.to_sexp fields
    | Field (name, spec) -> (
      match spec with
      | Required t -> Fields.of_field name (loop t a)
      | Optional t -> (
        match a with
        | None -> Fields.empty
        | Some a -> Fields.of_field name (loop t a)))
    | Iso (t, _, from) -> loop t (from a)
    | Both (x, y) ->
      let x = loop x (fst a) in
      let y = loop y (snd a) in
      Fields.merge x y
    | Either (x, y) -> (
      match a with
      | Left a -> loop x a
      | Right a -> loop y a)
    | Sum (_, constr) ->
      let (Case (a, constr)) = constr a in
      let arg = loop constr.arg a in
      Sexp.List [ Atom constr.name; arg ]
    | Enum choices -> (
      match
        List.find_map choices ~f:(fun (s, a') ->
            if Poly.equal a a' then
              Some s
            else
              None)
      with
      | Some v -> Atom v
      | None ->
        let open Dyn.Encoder in
        Code_error.raise "enum does not include this value"
          [ ("valid values", list (fun (x, _) -> string x) choices) ])
  in
  loop t a

let check_version ~version ~since ~until _ctx =
  if
    version < since
    ||
    match until with
    | None -> false
    | Some until -> version > until
  then
    raise_version_error ?until ~since "invalid version"

let of_sexp : 'a. ('a, values) t -> version:int * int -> Sexp.t -> 'a =
 fun t ~version sexp ->
  let rec loop : type a k. (a, k) t -> k -> a * k ret =
    fun (type a k) (t : (a, k) t) (ctx : k) : (a * k ret) ->
     match t with
     | Sexp -> (ctx, Values)
     | Version (t, { since; until }) ->
       check_version ~version ~since ~until ctx;
       loop t ctx
     | Fdecl t -> loop (Fdecl.get t) ctx
     | List t -> (
       match ctx with
       | List xs -> (List.map xs ~f:(fun x -> discard_values (loop t x)), Values)
       | Atom _ -> raise_of_sexp "expected list")
     | Pair (x, y) -> (
       match ctx with
       | List [ a; b ] ->
         let a, Values = loop x a in
         let b, Values = loop y b in
         ((a, b), Values)
       | _ -> raise_of_sexp "expected field entry")
     | Triple (x, y, z) -> (
       match ctx with
       | List [ a; b; c ] ->
         let a, Values = loop x a in
         let b, Values = loop y b in
         let c, Values = loop z c in
         ((a, b, c), Values)
       | _ -> raise_of_sexp "expected field entry")
     | Record (r : (a, fields) t) ->
       let (fields : Fields.t) = Fields.of_sexp ctx in
       let a, Fields f = loop r fields in
       Fields.check_empty f;
       (a, Values)
     | Field (name, spec) -> (
       match spec with
       | Required v ->
         let field, rest = Fields.required ctx name in
         let t, Values = loop v field in
         (t, Fields rest)
       | Optional v ->
         let field, rest = Fields.optional ctx name in
         let t =
           match field with
           | None -> None
           | Some f ->
             let a, Values = loop v f in
             Some a
         in
         (t, Fields rest))
     | Either (x, y) -> (
       try
         (* TODO share computation somehow *)
         let a, x = loop x ctx in
         (Left a, x)
       with
       | Of_sexp _ ->
         let a, y = loop y ctx in
         (Right a, y))
     | Iso (t, f, _) ->
       let a, k = loop t ctx in
       (f a, k)
     | Both (x, y) ->
       let a, Fields k = loop x ctx in
       let b, k = loop y k in
       ((a, b), k)
     | Sum (constrs, _) -> (
       match ctx with
       | List [ Atom head; args ] -> (
         match
           List.find_map constrs ~f:(fun (Constr c) ->
               if head = c.name then
                 Some
                   (let a, k = loop c.arg args in
                    (c.inj a, k))
               else
                 None)
         with
         | None -> raise_of_sexp "invalid constructor name"
         | Some p -> p)
       | _ -> raise_of_sexp "expected constructor")
     | Enum choices -> (
       match ctx with
       | List _ -> raise_of_sexp "expected list"
       | Atom a -> (
         match List.assoc choices a with
         | None -> raise_of_sexp "unable to read enum"
         | Some s -> (s, Values)))
  in
  discard_values (loop t sexp)

let of_sexp conv ~version sexp =
  match of_sexp conv ~version sexp with
  | s -> Ok s
  | exception Of_sexp e -> Error e

let record r = Record r

let either x y = Either (x, y)

let iso a t f = Iso (a, t, f)

let version ?until t ~since = Version (t, { until; since })

let field name spec = Field (name, spec)

let enum choices = Enum choices

let three a b c =
  iso
    (Both (a, Both (b, c)))
    (fun (x, (y, z)) -> (x, y, z))
    (fun (x, y, z) -> (x, (y, z)))

let four a b c d =
  iso
    (both (both a b) (both c d))
    (fun ((w, x), (y, z)) -> (w, x, y, z))
    (fun (w, x, y, z) -> ((w, x), (y, z)))

let five a b c d e =
  iso
    (both (both a b) (three c d e))
    (fun ((a, b), (c, d, e)) -> (a, b, c, d, e))
    (fun (a, b, c, d, e) -> ((a, b), (c, d, e)))

let six a b c d e f =
  iso
    (both (three a b c) (three d e f))
    (fun ((a, b, c), (d, e, f)) -> (a, b, c, d, e, f))
    (fun (a, b, c, d, e, f) -> ((a, b, c), (d, e, f)))

let seven a b c d e f g =
  iso
    (both (three a b c) (four d e f g))
    (fun ((a, b, c), (d, e, f, g)) -> (a, b, c, d, e, f, g))
    (fun (a, b, c, d, e, f, g) -> ((a, b, c), (d, e, f, g)))

let eight a b c d e f g h =
  iso
    (both (four a b c d) (four e f g h))
    (fun ((a, b, c, d), (e, f, g, h)) -> (a, b, c, d, e, f, g, h))
    (fun (a, b, c, d, e, f, g, h) -> ((a, b, c, d), (e, f, g, h)))

let sexp = Sexp

let required x = Required x

let optional x = Optional x

let fdecl x = Fdecl x
