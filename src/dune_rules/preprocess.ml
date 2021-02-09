open! Dune_engine
open Stdune
open Dune_lang.Decoder

module Pps_and_flags = struct
  let decode =
    let+ loc = loc
    and+ l, flags =
      until_keyword "--" ~before:String_with_vars.decode
        ~after:(repeat String_with_vars.decode)
    and+ syntax_version = Dune_lang.Syntax.get_exn Stanza.syntax in
    let pps, more_flags =
      List.partition_map l ~f:(fun s ->
          match String_with_vars.is_prefix ~prefix:"-" s with
          | Yes -> Right s
          | No
          | Unknown _ -> (
            let loc = String_with_vars.loc s in
            match String_with_vars.text_only s with
            | None ->
              User_error.raise ~loc
                [ Pp.text "No variables allowed in ppx library names" ]
            | Some txt -> Left (loc, Lib_name.parse_string_exn (loc, txt))))
    in
    let all_flags = more_flags @ Option.value flags ~default:[] in
    if syntax_version < (1, 10) then
      List.iter
        ~f:(fun flag ->
          if String_with_vars.has_pforms flag then
            Dune_lang.Syntax.Error.since
              (String_with_vars.loc flag)
              Stanza.syntax (1, 10) ~what:"Using variables in pps flags")
        all_flags;
    if pps = [] then
      User_error.raise ~loc
        [ Pp.text "You must specify at least one ppx rewriter." ];
    (pps, all_flags)
end

module Pps = struct
  type 'a t =
    { loc : Loc.t
    ; pps : 'a list
    ; flags : String_with_vars.t list
    ; staged : bool
    }

  let compare_no_locs compare_pps
      { loc = _; pps = pps1; flags = flags1; staged = s1 }
      { loc = _; pps = pps2; flags = flags2; staged = s2 } =
    match Bool.compare s1 s2 with
    | (Lt | Gt) as t -> t
    | Eq -> (
      match
        List.compare flags1 flags2 ~compare:String_with_vars.compare_no_loc
      with
      | (Lt | Gt) as t -> t
      | Eq -> List.compare pps1 pps2 ~compare:compare_pps)
end

type 'a t =
  | No_preprocessing
  | Action of Loc.t * Action_dune_lang.t
  | Pps of 'a Pps.t
  | Future_syntax of Loc.t

let map t ~f =
  match t with
  | Pps t -> Pps { t with pps = List.map t.pps ~f }
  | (No_preprocessing | Action _ | Future_syntax _) as t -> t

let filter_map t ~f =
  match t with
  | Pps t ->
    let pps = List.filter_map t.pps ~f in
    if pps = [] then
      No_preprocessing
    else
      Pps { t with pps }
  | (No_preprocessing | Action _ | Future_syntax _) as t -> t

module Without_instrumentation = struct
  type t = Loc.t * Lib_name.t

  let compare_no_locs (_, x) (_, y) = Lib_name.compare x y
end

module With_instrumentation = struct
  type t =
    | Ordinary of Without_instrumentation.t
    | Instrumentation_backend of (Loc.t * Lib_name.t)
end

let decode =
  sum
    [ ("no_preprocessing", return No_preprocessing)
    ; ( "action"
      , let+ loc, x =
          located
            (update_var String_with_vars.decoding_env_key
               ~f:(fun env ->
                 let env = Option.value_exn env in
                 Some (Pform.Env.lt_renamed_input_file env))
               Action_dune_lang.decode)
        in
        Action (loc, x) )
    ; ( "pps"
      , let+ loc = loc
        and+ pps, flags = Pps_and_flags.decode in
        Pps { loc; pps; flags; staged = false } )
    ; ( "staged_pps"
      , let+ () = Dune_lang.Syntax.since Stanza.syntax (1, 1)
        and+ loc = loc
        and+ pps, flags = Pps_and_flags.decode in
        Pps { loc; pps; flags; staged = true } )
    ; ( "future_syntax"
      , let+ () = Dune_lang.Syntax.since Stanza.syntax (1, 8)
        and+ loc = loc in
        Future_syntax loc )
    ]

let loc = function
  | No_preprocessing -> None
  | Action (loc, _)
  | Pps { loc; _ }
  | Future_syntax loc ->
    Some loc

let pps = function
  | Pps { pps; _ } -> pps
  | _ -> []

module Without_future_syntax = struct
  type 'a t =
    | No_preprocessing
    | Action of Loc.t * Action_dune_lang.t
    | Pps of 'a Pps.t
end

module Pp_flag_consumer = struct
  (* Compiler allows the output of [-pp] to be a binary AST. Merlin requires
     that to be a text file instead. *)
  type t =
    | Compiler
    | Merlin
end

let remove_future_syntax (t : 'a t) ~(for_ : Pp_flag_consumer.t) v :
    'a Without_future_syntax.t =
  match t with
  | No_preprocessing -> No_preprocessing
  | Action (loc, action) -> Action (loc, action)
  | Pps pps -> Pps pps
  | Future_syntax loc ->
    if Ocaml_version.supports_let_syntax v then
      No_preprocessing
    else
      Action
        ( loc
        , Run
            ( String_with_vars.make_pform loc (Macro (Bin, "ocaml-syntax-shims"))
            , (match for_ with
              | Compiler -> [ String_with_vars.make_text loc "-dump-ast" ]
              | Merlin ->
                (* We generate a text file instead of AST. That gives you less
                   precise locations, but at least Merlin doesn't fail outright.

                   In general this hack should be applied to all -pp commands
                   that might produce an AST, not just to Future_syntax. But
                   doing so means we need to change dune language so the user
                   can provide two versions of the command.

                   Hopefully this will be fixed in merlin before that becomes a
                   necessity. *)
                [])
              @ [ String_with_vars.make_pform loc (Var Input_file) ] ) )

module Per_module = struct
  module Per_module = Module_name.Per_item

  type 'a preprocess = 'a t

  type 'a t = 'a preprocess Per_module.t

  let decode = Per_module.decode decode ~default:No_preprocessing

  let no_preprocessing () = Per_module.for_all No_preprocessing

  let find module_name t = Per_module.get t module_name

  let default () = Per_module.for_all No_preprocessing

  let pps t =
    Per_module.fold t ~init:Lib_name.Map.empty ~f:(fun pp acc ->
        List.fold_left (pps pp) ~init:acc ~f:(fun acc (loc, pp) ->
            Lib_name.Map.set acc pp loc))
    |> Lib_name.Map.foldi ~init:[] ~f:(fun pp loc acc -> (loc, pp) :: acc)

  (* Any dummy module name works here *)
  let dummy_name = Module_name.of_string "A"

  let single_preprocess t =
    if Per_module.is_constant t then
      Per_module.get t dummy_name
    else
      No_preprocessing

  let add_instrumentation t ~loc ~flags:flags' libname =
    Per_module.map t ~f:(fun pp ->
        match pp with
        | No_preprocessing ->
          let pps = [ With_instrumentation.Instrumentation_backend libname ] in
          let staged = false in
          Pps { loc; pps; flags = flags'; staged }
        | Pps { loc; pps; flags; staged } ->
          let pps =
            With_instrumentation.Instrumentation_backend libname :: pps
          in
          Pps { loc; pps; flags = flags @ flags'; staged }
        | Action (loc, _)
        | Future_syntax loc ->
          User_error.raise ~loc
            [ Pp.text
                "Preprocessing with actions and future syntax cannot be used \
                 in conjunction with (instrumentation ...)"
            ])

  let without_instrumentation t =
    let f = function
      | With_instrumentation.Ordinary libname -> Some libname
      | With_instrumentation.Instrumentation_backend _ -> None
    in
    Per_module.map t ~f:(filter_map ~f)

  let with_instrumentation t ~instrumentation_backend =
    let f = function
      | With_instrumentation.Ordinary libname -> Some libname
      | With_instrumentation.Instrumentation_backend libname ->
        instrumentation_backend libname
    in
    Per_module.map t ~f:(filter_map ~f)
end
