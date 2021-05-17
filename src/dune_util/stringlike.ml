open Stdune

module Make (S : Stringlike_intf.S_base) = struct
  include S

  let of_string s : t =
    match S.of_string_opt s with
    | Some s -> s
    | None ->
      Code_error.raise
        ("Invalid " ^ S.module_ ^ ".t")
        [ ("s", Dyn.Encoder.string s) ]

  let error_message s = Printf.sprintf "%S is an invalid %s." s S.description

  let user_error (loc, s) =
    let hints =
      match S.hint_valid with
      | None -> []
      | Some f -> [ Pp.textf "%s would be a correct %s" (f s) S.description ]
    in
    let valid_desc =
      match S.description_of_valid_string with
      | None -> []
      | Some m -> [ m ]
    in
    User_error.make ~loc ~hints (Pp.text (error_message s) :: valid_desc)

  let of_string_user_error (loc, s) =
    match of_string_opt s with
    | Some s -> Ok s
    | None -> Error (user_error (loc, s))

  let parse_string_exn (loc, s) =
    match of_string_user_error (loc, s) with
    | Ok s -> s
    | Error err -> raise (User_error.E (err, User_error.Annotations.none))

  let conv =
    ( (fun s ->
        match of_string_opt s with
        | Some x -> Ok x
        | None -> Error (`Msg (error_message s)))
    , fun fmt t -> Format.pp_print_string fmt (to_string t) )

  let decode =
    let open Dune_lang.Decoder in
    map_validate (located string) ~f:of_string_user_error

  let decode_loc =
    let open Dune_lang.Decoder in
    map_validate (located string) ~f:(fun ((loc, _) as s) ->
        let open Result.O in
        let+ t = of_string_user_error s in
        (loc, t))

  let encode t = Dune_lang.Encoder.(string (to_string t))

  let to_dyn t = Dyn.Encoder.string (to_string t)
end
