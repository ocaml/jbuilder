open Stdune

type ast = Ast.t =
  | Atom of Loc.t * Atom.t
  | Quoted_string of Loc.t * string
  | Template of Template.t
  | List of Loc.t * ast list

type hint =
  { on : string
  ; candidates : string list
  }

module Name = struct
  module T = struct
    type t = string

    let compare a b =
      let alen = String.length a
      and blen = String.length b in
      match Int.compare alen blen with
      | Eq -> String.compare a b
      | ne -> ne

    let to_dyn = Dyn.Encoder.string
  end

  include T
  module Map = Map.Make (T)
end

module Fields : sig
  module Unparsed : sig
    type t = private
      { values : Ast.t list
      ; entry : Ast.t
      ; prev : t option (* Previous occurrence of this field *)
      }
  end

  type t = private
    { unparsed : Unparsed.t Name.Map.t
    ; known : string list
    }

  val of_values : Ast.t list -> t

  val add_known : t -> string -> t

  val consume : t -> string -> t

  val unparsed_ast : t -> Ast.t list

  val junk_unparsed : t -> t

  val leftover_fields : t -> string list -> t
end = struct
  module Unparsed = struct
    type t =
      { values : Ast.t list
      ; entry : Ast.t
      ; prev : t option (* Previous occurrence of this field *)
      }
  end

  type t =
    { unparsed : Unparsed.t Name.Map.t
    ; known : string list
    }

  let leftover_fields t fields =
    { unparsed = Name.Map.empty; known = t.known @ fields }

  let junk_unparsed t = { t with unparsed = Name.Map.empty }

  let of_values sexps =
    let unparsed =
      List.fold_left sexps ~init:Name.Map.empty ~f:(fun acc sexp ->
          match sexp with
          | List (_, name_sexp :: values) -> (
            match name_sexp with
            | Atom (_, A name) ->
              Name.Map.set acc name
                { Unparsed.values; entry = sexp; prev = Name.Map.find acc name }
            | List (loc, _)
            | Quoted_string (loc, _)
            | Template { loc; _ } ->
              User_error.raise ~loc [ Pp.text "Atom expected" ])
          | _ ->
            User_error.raise ~loc:(Ast.loc sexp)
              [ Pp.text "S-expression of the form (<name> <values>...) expected"
              ])
    in
    { unparsed; known = [] }

  let consume state name =
    { unparsed = Name.Map.remove state.unparsed name
    ; known = name :: state.known
    }

  let add_known state name = { state with known = name :: state.known }

  let unparsed_ast { unparsed; _ } =
    let rec loop acc = function
      | [] -> acc
      | x :: xs -> (
        match x.Unparsed.prev with
        | None -> loop (x.entry :: acc) xs
        | Some p -> loop (x.entry :: acc) (p :: xs))
    in
    loop [] (Name.Map.values unparsed)
    |> List.sort ~compare:(fun a b ->
           Int.compare (Ast.loc a).start.pos_cnum (Ast.loc b).start.pos_cnum)
end

type fields = Fields.t

type values = Ast.t list

(* Arguments are:

   - the location of the whole list - the first atom when parsing a constructor
   or a field - the universal map holding the user context *)
type 'kind context =
  | Values : Loc.t * string option * Univ_map.t -> values context
  | Fields : Loc.t * string option * Univ_map.t -> Fields.t context

type ('a, 'kind) parser = 'kind context -> 'kind -> 'a * 'kind

type 'a t = ('a, values) parser

type 'a fields_parser = ('a, Fields.t) parser

let return x _ctx state = (x, state)

let ( >>= ) t f ctx state =
  let x, state = t ctx state in
  f x ctx state

let ( >>| ) t f ctx state =
  let x, state = t ctx state in
  (f x, state)

let ( >>> ) a b ctx state =
  let (), state = a ctx state in
  b ctx state

let ( let* ) = ( >>= )

let ( let+ ) = ( >>| )

let ( and+ ) a b ctx state =
  let a, state = a ctx state in
  let b, state = b ctx state in
  ((a, b), state)

let map t ~f = t >>| f

let try_ t f ctx state =
  try t ctx state with
  | exn -> f exn ctx state

let get_user_context : type k. k context -> Univ_map.t = function
  | Values (_, _, uc) -> uc
  | Fields (_, _, uc) -> uc

let get key ctx state = (Univ_map.find (get_user_context ctx) key, state)

let get_all ctx state = (get_user_context ctx, state)

let set : type a b k. a Univ_map.Key.t -> a -> (b, k) parser -> (b, k) parser =
 fun key v t ctx state ->
  match ctx with
  | Values (loc, cstr, uc) ->
    t (Values (loc, cstr, Univ_map.set uc key v)) state
  | Fields (loc, cstr, uc) ->
    t (Fields (loc, cstr, Univ_map.set uc key v)) state

let update_var :
    type a b k.
       a Univ_map.Key.t
    -> f:(a option -> a option)
    -> (b, k) parser
    -> (b, k) parser =
 fun key ~f t ctx state ->
  match ctx with
  | Values (loc, cstr, uc) ->
    t (Values (loc, cstr, Univ_map.update uc key ~f)) state
  | Fields (loc, cstr, uc) ->
    t (Fields (loc, cstr, Univ_map.update uc key ~f)) state

let set_many : type a k. Univ_map.t -> (a, k) parser -> (a, k) parser =
 fun map t ctx state ->
  match ctx with
  | Values (loc, cstr, uc) ->
    t (Values (loc, cstr, Univ_map.superpose uc map)) state
  | Fields (loc, cstr, uc) ->
    t (Fields (loc, cstr, Univ_map.superpose uc map)) state

let loc : type k. k context -> k -> Loc.t * k =
 fun ctx state ->
  match ctx with
  | Values (loc, _, _) -> (loc, state)
  | Fields (loc, _, _) -> (loc, state)

let eos : type k. k context -> k -> bool * k =
 fun ctx state ->
  match ctx with
  | Values _ -> (state = [], state)
  | Fields _ -> (Name.Map.is_empty state.unparsed, state)

let repeat : 'a t -> 'a list t =
  let rec loop t acc ctx l =
    match l with
    | [] -> (List.rev acc, [])
    | _ ->
      let x, l = t ctx l in
      loop t (x :: acc) ctx l
  in
  fun t ctx state -> loop t [] ctx state

let repeat1 p =
  let+ x = p
  and+ xs = repeat p in
  x :: xs

let result : type a k. k context -> a * k -> a =
 fun ctx (v, state) ->
  match ctx with
  | Values (_, cstr, _) -> (
    match state with
    | [] -> v
    | sexp :: _ -> (
      match cstr with
      | None ->
        User_error.raise ~loc:(Ast.loc sexp) [ Pp.text "This value is unused" ]
      | Some s ->
        User_error.raise ~loc:(Ast.loc sexp)
          [ Pp.textf "Too many argument for %s" s ]))
  | Fields _ -> (
    match Name.Map.choose state.unparsed with
    | None -> v
    | Some (name, { entry; _ }) ->
      let name_loc =
        match entry with
        | List (_, s :: _) -> Ast.loc s
        | _ -> assert false
      in
      User_error.raise ~loc:name_loc
        ~hints:(User_message.did_you_mean name ~candidates:state.known)
        [ Pp.textf "Unknown field %s" name ])

let parse t context sexp =
  let ctx = Values (Ast.loc sexp, None, context) in
  result ctx (t ctx [ sexp ])

let set_input : type k. ast list -> (unit, k) parser =
 fun sexps context _ ->
  match context with
  | Values _ -> ((), sexps)
  | Fields _ -> ((), Fields.of_values sexps)

let capture ctx state =
  let f t = result ctx (t ctx state) in
  (f, [])

let lazy_ t =
  let+ f = capture in
  lazy (f t)

let end_of_list (Values (loc, cstr, _)) =
  match cstr with
  | None ->
    let loc = { loc with start = loc.stop } in
    User_error.raise ~loc [ Pp.text "Premature end of list" ]
  | Some s -> User_error.raise ~loc [ Pp.textf "Not enough arguments for %s" s ]
  [@@inline never]

let next f ctx sexps =
  match sexps with
  | [] -> end_of_list ctx
  | sexp :: sexps -> (f sexp, sexps)
  [@@inline always]

let next_with_user_context f ctx sexps =
  match sexps with
  | [] -> end_of_list ctx
  | sexp :: sexps -> (f (get_user_context ctx) sexp, sexps)
  [@@inline always]

let peek _ctx sexps =
  match sexps with
  | [] -> (None, sexps)
  | sexp :: _ -> (Some sexp, sexps)
  [@@inline always]

let peek_exn ctx sexps =
  match sexps with
  | [] -> end_of_list ctx
  | sexp :: _ -> (sexp, sexps)
  [@@inline always]

let junk = next ignore

let junk_everything : type k. (unit, k) parser =
 fun ctx state ->
  match ctx with
  | Values _ -> ((), [])
  | Fields _ -> ((), Fields.junk_unparsed state)

let keyword kwd =
  next (function
    | Atom (_, A s) when s = kwd -> ()
    | sexp ->
      User_error.raise ~loc:(Ast.loc sexp) [ Pp.textf "'%s' expected" kwd ])

let atom_matching f ~desc =
  next (fun sexp ->
      match
        match sexp with
        | Atom (_, A s) -> f s
        | _ -> None
      with
      | Some x -> x
      | None ->
        User_error.raise ~loc:(Ast.loc sexp) [ Pp.textf "%s expected" desc ])

let until_keyword kwd ~before ~after =
  let rec loop acc =
    peek >>= function
    | None -> return (List.rev acc, None)
    | Some (Atom (_, A s)) when s = kwd ->
      junk >>> after >>= fun x -> return (List.rev acc, Some x)
    | _ -> before >>= fun x -> loop (x :: acc)
  in
  loop []

let plain_string f =
  next (function
    | Atom (loc, A s)
    | Quoted_string (loc, s) ->
      f ~loc s
    | Template { loc; _ }
    | List (loc, _) ->
      User_error.raise ~loc [ Pp.text "Atom or quoted string expected" ])

let filename =
  plain_string (fun ~loc s ->
      match s with
      | "."
      | ".." ->
        User_error.raise ~loc
          [ Pp.textf "'.' and '..' are not valid filenames" ]
      | fn -> fn)

let relative_file =
  plain_string (fun ~loc fn ->
      if Filename.is_relative fn then
        fn
      else
        User_error.raise ~loc [ Pp.textf "relative filename expected" ])

let enter t =
  next_with_user_context (fun uc sexp ->
      match sexp with
      | List (loc, l) ->
        let ctx = Values (loc, None, uc) in
        result ctx (t ctx l)
      | sexp -> User_error.raise ~loc:(Ast.loc sexp) [ Pp.text "List expected" ])

let either =
  (* Before you read this code, close your eyes and internalise the fact that
     this code is temporary. It is a temporary state as part of a larger work to
     turn [Decoder.t] into a pure applicative. Once this is done, this function
     will be implemented in a better way and with a much cleaner semantic. *)
  let approximate_how_much_input_a_failing_branch_consumed
      (exn : Exn_with_backtrace.t) =
    Printexc.raw_backtrace_length exn.backtrace
  in
  let compare_input_consumed exn1 exn2 =
    Int.compare
      (approximate_how_much_input_a_failing_branch_consumed exn1)
      (approximate_how_much_input_a_failing_branch_consumed exn2)
  in
  fun a b ctx state ->
    try (a >>| Either.left) ctx state with
    | exn_a -> (
      let exn_a = Exn_with_backtrace.capture exn_a in
      try (b >>| Either.right) ctx state with
      | exn_b ->
        let exn_b = Exn_with_backtrace.capture exn_b in
        Exn_with_backtrace.reraise
          (match compare_input_consumed exn_a exn_b with
          | Gt -> exn_a
          | Eq
          | Lt ->
            exn_b))

let ( <|> ) x y =
  let+ res = either x y in
  match res with
  | Left x -> x
  | Right x -> x

let fix f =
  let rec p = lazy (f r)
  and r ast = (Lazy.force p) ast in
  r

let loc_between_states : type k. k context -> k -> k -> Loc.t =
 fun ctx state1 state2 ->
  match ctx with
  | Values _ -> (
    match state1 with
    | sexp :: rest when rest == state2 ->
      (* common case *)
      Ast.loc sexp
    | [] ->
      let (Values (loc, _, _)) = ctx in
      { loc with start = loc.stop }
    | sexp :: rest ->
      let loc = Ast.loc sexp in
      let rec search last l =
        if l == state2 then
          { loc with stop = (Ast.loc last).stop }
        else
          match l with
          | [] ->
            let (Values (loc, _, _)) = ctx in
            { (Ast.loc sexp) with stop = loc.stop }
          | sexp :: rest -> search sexp rest
      in
      search sexp rest)
  | Fields _ -> (
    let parsed =
      Name.Map.merge state1.unparsed state2.unparsed
        ~f:(fun _key before after ->
          match (before, after) with
          | Some _, None -> before
          | _ -> None)
    in
    match
      Name.Map.values parsed
      |> List.map ~f:(fun f -> Ast.loc f.Fields.Unparsed.entry)
      |> List.sort ~compare:(fun a b ->
             Int.compare a.Loc.start.pos_cnum b.start.pos_cnum)
    with
    | [] ->
      let (Fields (loc, _, _)) = ctx in
      loc
    | first :: l ->
      let last = List.fold_left l ~init:first ~f:(fun _ x -> x) in
      { first with stop = last.stop })

let located t ctx state1 =
  let x, state2 = t ctx state1 in
  ((loc_between_states ctx state1 state2, x), state2)

let raw = next Fun.id

let basic_loc desc f =
  next (function
    | Template { loc; _ }
    | List (loc, _)
    | Quoted_string (loc, _) ->
      User_error.raise ~loc [ Pp.textf "%s expected" desc ]
    | Atom (loc, s) -> (
      match f ~loc (Atom.to_string s) with
      | None -> User_error.raise ~loc [ Pp.textf "%s expected" desc ]
      | Some x -> x))

let basic desc f = basic_loc desc (fun ~loc:_ -> f)

let string = plain_string (fun ~loc:_ x -> x)

let int = basic "Integer" Int.of_string

let float = basic "Float" Float.of_string

let pair a b =
  enter
    ( a >>= fun a ->
      b >>= fun b -> return (a, b) )

let triple a b c =
  enter
    ( a >>= fun a ->
      b >>= fun b ->
      c >>= fun c -> return (a, b, c) )

let unit_number_generic ~of_string ~mul name suffixes =
  let unit_number_of_string ~loc s =
    let possible_suffixes () =
      String.concat ~sep:", " (List.map ~f:fst suffixes)
    in
    let n, suffix =
      let f c = not (Char.is_digit c) in
      match String.findi s ~f with
      | None ->
        User_error.raise ~loc
          [ Pp.textf "missing suffix, use one of %s" (possible_suffixes ()) ]
      | Some i -> String.split_n s i
    in
    let factor =
      match List.assoc suffixes suffix with
      | Some f -> f
      | None ->
        User_error.raise ~loc
          [ Pp.textf "invalid suffix, use one of %s" (possible_suffixes ()) ]
    in
    Option.map ~f:(mul factor) (of_string n)
  in
  basic_loc name unit_number_of_string

let unit_number = unit_number_generic ~of_string:Int.of_string ~mul:( * )

let unit_number_int64 =
  (* This can go into a separate module [stdune/int64.ml]. *)
  let of_string s = Option.try_with (fun () -> Int64.of_string s) in
  unit_number_generic ~of_string ~mul:Int64.mul

let duration = unit_number "Duration" [ ("s", 1); ("m", 60); ("h", 60 * 60) ]

(* CR-someday amokhov: Add KiB, MiB, GiB. *)
let bytes_unit =
  unit_number_int64 "Byte amount"
    [ ("B", 1L)
    ; ("kB", 1000L)
    ; ("KB", 1000L)
    ; ("MB", 1000_000L)
    ; ("GB", 1000_000_000L)
    ]

let maybe t = t >>| Option.some <|> return None

let find_cstr cstrs loc name ctx values =
  match List.assoc cstrs name with
  | Some t -> result ctx (t ctx values)
  | None ->
    User_error.raise ~loc
      ~hints:
        (User_message.did_you_mean name ~candidates:(List.map cstrs ~f:fst))
      [ Pp.textf "Unknown constructor %s" name ]

let sum ?(force_parens = false) cstrs =
  next_with_user_context (fun uc sexp ->
      match sexp with
      | Atom (loc, A s) when not force_parens ->
        find_cstr cstrs loc s (Values (loc, Some s, uc)) []
      | Atom (loc, _)
      | Template { loc; _ }
      | Quoted_string (loc, _)
      | List (loc, []) ->
        User_error.raise ~loc
          [ Pp.textf "S-expression of the form %s expected"
              (if force_parens then
                "(<atom> ...)"
              else
                "(<atom> ...) or <atom>")
          ]
      | List (loc, name :: args) -> (
        match name with
        | Quoted_string (loc, _)
        | List (loc, _)
        | Template { loc; _ } ->
          User_error.raise ~loc [ Pp.text "Atom expected" ]
        | Atom (s_loc, A s) ->
          find_cstr cstrs s_loc s (Values (loc, Some s, uc)) args))

let enum cstrs =
  next (function
    | Quoted_string (loc, _)
    | Template { loc; _ }
    | List (loc, _) ->
      User_error.raise ~loc [ Pp.text "Atom expected" ]
    | Atom (loc, A s) -> (
      match List.assoc cstrs s with
      | Some value -> value
      | None ->
        User_error.raise ~loc
          [ Pp.textf "Unknown value %s" s ]
          ~hints:
            (User_message.did_you_mean s ~candidates:(List.map cstrs ~f:fst))))

let bool = enum [ ("true", true); ("false", false) ]

let map_validate t ~f ctx state1 =
  let x, state2 = t ctx state1 in
  match f x with
  | Result.Ok x -> (x, state2)
  | Error (msg : User_message.t) ->
    let msg =
      match msg.loc with
      | Some _ -> msg
      | None -> { msg with loc = Some (loc_between_states ctx state1 state2) }
    in
    raise (User_error.E (msg, []))

(** TODO: Improve consistency of error messages, e.g. use %S consistently for
    field names: see [field_missing] and [field_present_too_many_times]. *)
let field_missing loc name =
  User_error.raise ~loc [ Pp.textf "field %s missing" name ]
  [@@inline never]

let field_present_too_many_times _ name entries =
  match entries with
  | _ :: second :: _ ->
    User_error.raise ~loc:(Ast.loc second)
      [ Pp.textf "Field %S is present too many times" name ]
  | _ -> assert false

let multiple_occurrences ?(on_dup = field_present_too_many_times) uc name last =
  let rec collect acc (x : Fields.Unparsed.t) =
    let acc = x.entry :: acc in
    match x.prev with
    | None -> acc
    | Some prev -> collect acc prev
  in
  on_dup uc name (collect [] last)
  [@@inline never]

let find_single ?on_dup uc (state : Fields.t) name =
  let res = Name.Map.find state.unparsed name in
  (match res with
  | Some ({ prev = Some _; _ } as last) ->
    multiple_occurrences uc name last ?on_dup
  | _ -> ());
  res

let field name ?default ?on_dup t (Fields (loc, _, uc)) state =
  match find_single uc state name ?on_dup with
  | Some { values; entry; _ } ->
    let ctx = Values (Ast.loc entry, Some name, uc) in
    let x = result ctx (t ctx values) in
    (x, Fields.consume state name)
  | None -> (
    match default with
    | Some v -> (v, Fields.add_known state name)
    | None -> field_missing loc name)

let field_o name ?on_dup t (Fields (_, _, uc)) state =
  match find_single uc state name ?on_dup with
  | Some { values; entry; _ } ->
    let ctx = Values (Ast.loc entry, Some name, uc) in
    let x = result ctx (t ctx values) in
    (Some x, Fields.consume state name)
  | None -> (None, Fields.add_known state name)

let field_b_gen field_gen ?check ?on_dup name =
  field_gen name ?on_dup
    (let* () = Option.value check ~default:(return ()) in
     eos >>= function
     | true -> return true
     | _ -> bool)

let field_b = field_b_gen (field ~default:false)

let field_o_b = field_b_gen field_o

let multi_field name t (Fields (_, _, uc)) (state : Fields.t) =
  let rec loop acc (field : Fields.Unparsed.t option) =
    match field with
    | None -> acc
    | Some { values; prev; entry } ->
      let ctx = Values (Ast.loc entry, Some name, uc) in
      let x = result ctx (t ctx values) in
      loop (x :: acc) prev
  in
  let res = loop [] (Name.Map.find state.unparsed name) in
  (res, Fields.consume state name)

let fields t (Values (loc, cstr, uc)) sexps =
  let ctx = Fields (loc, cstr, uc) in
  let x = result ctx (t ctx (Fields.of_values sexps)) in
  (x, [])

let leftover_fields_generic t more_fields (Fields (loc, cstr, uc)) state =
  let x =
    let ctx = Values (loc, cstr, uc) in
    result ctx (repeat t ctx (Fields.unparsed_ast state))
  in
  (x, Fields.leftover_fields state more_fields)

let leftover_fields ctx (state : Fields.t) =
  leftover_fields_generic raw (Name.Map.keys state.unparsed) ctx state

let leftover_fields_as_sums cstrs =
  leftover_fields_generic (sum cstrs) (List.map cstrs ~f:fst)

type kind =
  | Values of Loc.t * string option
  | Fields of Loc.t * string option

let kind : type k. k context -> k -> kind * k =
 fun ctx state ->
  match ctx with
  | Values (loc, cstr, _) -> (Values (loc, cstr), state)
  | Fields (loc, cstr, _) -> (Fields (loc, cstr), state)

let traverse l ~f ctx state =
  Tuple.T2.swap
    (List.fold_map ~init:state l ~f:(fun state x ->
         Tuple.T2.swap (f x ctx state)))

let all = traverse ~f:(fun x -> x)

let fields_missing_need_exactly_one loc names =
  User_error.raise ~loc
    [ Pp.textf "fields %s are all missing (exactly one is needed)"
        (String.concat ~sep:", " names)
    ]
  [@@inline never]

let fields_mutual_exclusion_violation loc names =
  User_error.raise ~loc
    [ Pp.textf "fields %s are mutually exclusive"
        (String.concat ~sep:", " names)
    ]
  [@@inline never]

let fields_mutually_exclusive ?on_dup ?default fields
    ((Fields (loc, _, _) : _ context) as ctx) state =
  let res, state =
    traverse
      ~f:(fun (name, parser) ->
        field_o name ?on_dup parser >>| fun res -> (name, res))
      fields ctx state
  in
  match
    List.filter_map res ~f:(function
      | name, Some x -> Some (name, x)
      | _, None -> None)
  with
  | [] -> (
    let names = List.map fields ~f:fst in
    match default with
    | None -> fields_missing_need_exactly_one loc names
    | Some default -> (default, state))
  | [ (_name, res) ] -> (res, state)
  | _ :: _ :: _ as results ->
    let names = List.map ~f:fst results in
    fields_mutual_exclusion_violation loc names



let hack keyword : (_, values) parser -> (_, values) parser -> (_, values) parser = fun left right (Values (_loc,_,_) as ctx) input  ->
  match input with
  | (Ast.List (_loc, (Atom (_,Atom.A h))::tl)) :: asttl when String.equal h keyword ->
      begin
        match right ctx tl with
        | (ans,[]) -> (ans, asttl)
        | (_,_) -> failwith "not all data is parsed"
      end
  | other -> left ctx other
