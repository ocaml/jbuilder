open! Stdune
open! Import

type part =
  | Text of string
  | Pform of Dune_lang.Template.Pform.t * Pform.t
  (* [Error _] is for percent forms that failed to parse with lang dune < 3.0 *)
  | Error of Dune_lang.Template.Pform.t * User_message.t

type t =
  { quoted : bool
  ; parts : part list
  ; loc : Loc.t
  }

let compare_no_loc t1 t2 =
  match Bool.compare t1.quoted t2.quoted with
  | (Ordering.Lt | Gt) as a -> a
  | Eq ->
    List.compare t1.parts t2.parts ~compare:(fun a b ->
        match (a, b) with
        | Text a, Text b -> String.compare a b
        | Pform (_, a), Pform (_, b) -> Pform.compare a b
        | Error (_, a), Error (_, b) -> Poly.compare a b
        | Text _, _ -> Lt
        | _, Text _ -> Gt
        | Pform _, _ -> Lt
        | _, Pform _ -> Gt)

let equal_no_loc t1 t2 = Ordering.is_eq (compare_no_loc t1 t2)

let make_text ?(quoted = false) loc s = { quoted; loc; parts = [ Text s ] }

let make_pform ?(quoted = false) loc pform =
  let source =
    match Pform.encode_to_latest_dune_lang_version pform with
    | Success { name; payload } ->
      { Dune_lang.Template.Pform.loc; name; payload }
    | Pform_was_deleted -> assert false
  in
  { quoted; loc; parts = [ Pform (source, pform) ] }

let literal ~quoted ~loc s = { parts = [ Text s ]; quoted; loc }

let decoding_env_key =
  Univ_map.Key.create ~name:"pform decoding environment" Pform.Env.to_dyn

let set_decoding_env env = Dune_lang.Decoder.set decoding_env_key env

let add_user_vars_to_decoding_env vars =
  Dune_lang.Decoder.update_var decoding_env_key ~f:(function
    | None -> Code_error.raise "Decoding env not set" []
    | Some env -> Some (Pform.Env.add_user_vars env vars))

let decode_manually f =
  let open Dune_lang.Decoder in
  let+ env = get decoding_env_key
  and+ x = raw in
  let env =
    match env with
    | Some env -> env
    | None ->
      Code_error.raise ~loc:(Dune_lang.Ast.loc x)
        "pform decoding environment not set" []
  in
  match x with
  | Atom (loc, A s) -> literal ~quoted:false ~loc s
  | Quoted_string (loc, s) -> literal ~quoted:true ~loc s
  | List (loc, _) -> User_error.raise ~loc [ Pp.text "Unexpected list" ]
  | Template { quoted; loc; parts } ->
    { quoted
    ; loc
    ; parts =
        List.map parts ~f:(function
          | Dune_lang.Template.Text s -> Text s
          | Pform v -> (
            match f env v with
            | pform -> Pform (v, pform)
            | exception User_error.E msg
              when Pform.Env.syntax_version env < (3, 0) ->
              (* Before dune 3.0, unknown variable errors were delayed *)
              Error (v, msg)))
    }

let decode = decode_manually Pform.Env.parse

let loc t = t.loc

let virt_pform ?quoted pos pform =
  let loc = Loc.of_pos pos in
  make_pform ?quoted loc pform

let virt_text pos s =
  let loc = Loc.of_pos pos in
  { parts = [ Text s ]; loc; quoted = true }

let concat_rev = function
  | [] -> ""
  | [ s ] -> s
  | l -> String.concat (List.rev l) ~sep:""

module Mode = struct
  type _ t =
    | Single : Value.t t
    | Many : Value.t list t

  let string : type a. a t -> string -> a =
   fun t s ->
    match t with
    | Single -> Value.String s
    | Many -> [ Value.String s ]

  let value : type a. a t -> Value.t list -> a option =
   fun t s ->
    match (t, s) with
    | Many, s -> Some s
    | Single, [ s ] -> Some s
    | Single, _ -> None
end

let invalid_multivalue source _pform x =
  User_error.raise ~loc:source.Dune_lang.Template.Pform.loc
    [ Pp.textf
        "%s %s expands to %d values, however a single value is expected here. \
         Please quote this atom."
        (String.capitalize_ascii
           (Dune_lang.Template.Pform.describe_kind source))
        (Dune_lang.Template.Pform.describe source)
        (List.length x)
    ]

type known_suffix =
  | Full of string
  | Partial of
      { source_pform : Dune_lang.Template.Pform.t
      ; suffix : string
      }

let known_suffix =
  let rec go t acc =
    match t with
    | Text s :: rest -> go rest (s :: acc)
    | [] -> Full (String.concat ~sep:"" acc)
    | (Pform (p, _) | Error (p, _)) :: _ ->
      Partial { source_pform = p; suffix = String.concat ~sep:"" acc }
  in
  fun t -> go (List.rev t.parts) []

type known_prefix =
  | Full of string
  | Partial of
      { prefix : string
      ; source_pform : Dune_lang.Template.Pform.t
      }

let known_prefix =
  let rec go t acc =
    match t with
    | Text s :: rest -> go rest (s :: acc)
    | [] -> Full (String.concat ~sep:"" (List.rev acc))
    | (Pform (p, _) | Error (p, _)) :: _ ->
      Partial
        { prefix = String.concat ~sep:"" (List.rev acc); source_pform = p }
  in
  fun t -> go t.parts []

let fold_pforms =
  let rec loop parts acc f =
    match parts with
    | [] -> acc
    | (Text _ | Error _) :: parts -> loop parts acc f
    | Pform (p, v) :: parts -> loop parts (f ~source:p v acc) f
  in
  fun t ~init ~f -> loop t.parts init f

type 'a expander = source:Dune_lang.Template.Pform.t -> Pform.t -> 'a

type yes_no_unknown =
  | Yes
  | No
  | Unknown of { source_pform : Dune_lang.Template.Pform.t }

let is_suffix t ~suffix:want =
  match known_suffix t with
  | Full s ->
    if String.is_suffix ~suffix:want s then
      Yes
    else
      No
  | Partial { suffix = have; source_pform } ->
    if String.is_suffix ~suffix:want have then
      Yes
    else if String.is_suffix ~suffix:have want then
      Unknown { source_pform }
    else
      No

let is_prefix t ~prefix:want =
  match known_prefix t with
  | Full s ->
    if String.is_prefix ~prefix:want s then
      Yes
    else
      No
  | Partial { prefix = have; source_pform } ->
    if String.is_prefix ~prefix:want have then
      Yes
    else if String.is_prefix ~prefix:have want then
      Unknown { source_pform }
    else
      No

module Private = struct
  module Partial = struct
    type nonrec 'a t =
      | Expanded of 'a
      | Unexpanded of t

    let map t ~f =
      match t with
      | Expanded t -> Expanded (f t)
      | Unexpanded t -> Unexpanded t

    let is_suffix t ~suffix =
      match t with
      | Expanded s ->
        if String.is_suffix ~suffix s then
          Yes
        else
          No
      | Unexpanded t -> is_suffix t ~suffix

    let is_prefix t ~prefix =
      match t with
      | Expanded s ->
        if String.is_prefix ~prefix s then
          Yes
        else
          No
      | Unexpanded t -> is_prefix t ~prefix
  end
end

open Private

module type S = sig
  type 'a app

  val expand :
       t
    -> mode:'a Mode.t
    -> dir:Path.t
    -> f:Value.t list option app expander
    -> 'a app

  val partial_expand :
       t
    -> mode:'a Mode.t
    -> dir:Path.t
    -> f:Value.t list option app expander
    -> 'a Partial.t app
end

module Make (A : Applicative_intf.S1) = struct
  (* We parameterize expansion over an applicative to be able to accumulate
     dependencies as we expand. *)
  module App = Applicative.Make (A)

  let partial_expand :
        'a.    t -> mode:'a Mode.t -> dir:Path.t
        -> f:Value.t list option A.t expander -> 'a Partial.t A.t =
   fun t ~mode ~dir ~f ->
    match t.parts with
    (* Optimizations for some common cases *)
    | [] -> A.return (Partial.Expanded (Mode.string mode ""))
    | [ Text s ] -> A.return (Partial.Expanded (Mode.string mode s))
    | [ Pform (source, p) ] when not t.quoted -> (
      let open App.O in
      let+ exp = f ~source p in
      match exp with
      | None -> Partial.Unexpanded t
      | Some e ->
        Expanded
          (match Mode.value mode e with
          | None -> invalid_multivalue source p e
          | Some s -> s))
    | _ ->
      let expanded_parts =
        List.map t.parts ~f:(fun part ->
            match part with
            | Text _
            | Error _ ->
              App.return part
            | Pform (source, p) -> (
              let open App.O in
              let+ exp = f ~source p in
              match exp with
              | Some (([] | _ :: _ :: _) as e) when not t.quoted ->
                invalid_multivalue source p e
              | Some t -> Text (Value.L.concat ~dir t)
              | None -> part))
      in
      let open App.O in
      let+ expanded = App.all expanded_parts in
      let commit_text acc_text acc =
        let s = concat_rev acc_text in
        if s = "" then
          acc
        else
          Text s :: acc
      in
      (* This pass merges all consecutive [Text] constructors *)
      let rec loop acc_text acc items =
        match items with
        | [] -> (
          match acc with
          | [] -> Partial.Expanded (Mode.string mode (concat_rev acc_text))
          | _ ->
            Unexpanded { t with parts = List.rev (commit_text acc_text acc) })
        | Text s :: items -> loop (s :: acc_text) acc items
        | it :: items -> loop [] (it :: commit_text acc_text acc) items
      in
      loop [] [] expanded

  let expand t ~mode ~dir ~f =
    let open App.O in
    let+ exp = partial_expand t ~mode ~dir ~f in
    match exp with
    | Partial.Expanded s -> s
    | Unexpanded t ->
      List.iter t.parts ~f:(function
        | Error (_, msg) -> raise (User_error.E msg)
        | _ -> ());
      assert false
end

include Make (Applicative.Id)

let is_pform t pform =
  match t.parts with
  | [ Pform (_, pform') ] -> Pform.compare pform pform' = Eq
  | _ -> false

let text_only t =
  match t.parts with
  | [ Text s ] -> Some s
  | _ -> None

let has_pforms t = Option.is_none (text_only t)

let encode t =
  match text_only t with
  | Some s -> Dune_lang.atom_or_quoted_string s
  | None ->
    Dune_lang.Template
      { loc = t.loc
      ; quoted = t.quoted
      ; parts =
          List.map t.parts ~f:(function
            | Text s -> Dune_lang.Template.Text s
            | Error (_, msg) -> raise (User_error.E msg)
            | Pform (source, pform) -> (
              match Pform.encode_to_latest_dune_lang_version pform with
              | Pform_was_deleted ->
                User_error.raise ~loc:source.loc
                  [ Pp.textf
                      "%s was deleted in the latest version of the dune \
                       language. It cannot appear here. "
                      (Dune_lang.Template.Pform.describe source)
                  ]
              | Success { name; payload } ->
                Pform { loc = source.loc; name; payload }))
      }

let to_dyn t = Dune_lang.to_dyn (encode t)

let remove_locs { quoted; loc = _; parts } =
  { quoted
  ; loc = Loc.none
  ; parts =
      List.map parts ~f:(function
        | Text _ as p -> p
        | Error (source, msg) ->
          Error ({ source with loc = Loc.none }, { msg with loc = None })
        | Pform (source, p) -> Pform ({ source with loc = Loc.none }, p))
  }

module Partial = struct
  include Private.Partial

  let to_dyn f =
    let open Dyn.Encoder in
    function
    | Expanded x -> constr "Expander" [ f x ]
    | Unexpanded t -> constr "Unexpanded" [ to_dyn t ]

  let elim t ~exp ~unexp =
    match t with
    | Expanded e -> exp e
    | Unexpanded t -> unexp t

  let expanded t = Expanded t
end
