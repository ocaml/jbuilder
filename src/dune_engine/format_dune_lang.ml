open! Stdune
open! Import
open Pp.O

type dune_file =
  | OCaml_syntax of Loc.t
  | Sexps of Dune_lang.Cst.t list

let parse_lexbuf lb =
  if Dune_lexer.is_script lb then
    OCaml_syntax (Loc.of_lexbuf lb)
  else
    Sexps (Dune_lang.Parser.parse lb ~mode:Cst)

let parse_file path_opt =
  match path_opt with
  | Some path -> Io.with_lexbuf_from_file path ~f:parse_lexbuf
  | None -> parse_lexbuf @@ Lexing.from_channel stdin

let can_be_displayed_wrapped =
  List.for_all ~f:(fun (c : Dune_lang.Cst.t) ->
      match c with
      | Atom _
      | Quoted_string _
      | Template _
      | List (_, [])
      | List (_, [ _ ]) ->
        true
      | List _
      | Comment _ ->
        false)

let pp_simple t =
  Dune_lang.Cst.abstract t |> Option.value_exn |> Dune_lang.Ast.remove_locs
  |> Dune_lang.pp

let print_wrapped_list ~version x =
  let inner = Pp.concat_map ~sep:Pp.space ~f:pp_simple x in
  if version < (2, 8) then
    Pp.char '(' ++ Pp.hovbox ~indent:1 inner ++ Pp.char ')'
  else
    Pp.hvbox ~indent:1 (Pp.char '(' ++ inner ++ Pp.char ')')

let pp_comment_line l = Pp.char ';' ++ Pp.verbatim l

let pp_comment loc (comment : Dune_lang.Cst.Comment.t) =
  match comment with
  | Lines ls -> Pp.vbox (Pp.concat_map ~sep:Pp.cut ~f:pp_comment_line ls)
  | Legacy ->
    User_error.raise ~loc
      [ Pp.text "Formatting is only supported with the dune syntax" ]

let pp_break attached =
  if attached then
    Pp.char ' '
  else
    Pp.cut

let pp_list_with_comments pp_sexp sexps =
  let rec go (l : Dune_lang.Cst.t list) =
    match l with
    | x :: Comment (loc, c) :: xs ->
      let attached = Loc.on_same_line (Dune_lang.Cst.loc x) loc in
      pp_sexp x ++ pp_break attached ++ pp_comment loc c ++ Pp.cut ++ go xs
    | Comment (loc, c) :: xs -> pp_comment loc c ++ Pp.cut ++ go xs
    | [ x ] -> pp_sexp x
    | x :: xs -> pp_sexp x ++ Pp.cut ++ go xs
    | [] -> Pp.nop
  in
  go sexps

let rec pp_sexp ~version : Dune_lang.Cst.t -> _ = function
  | (Atom _ | Quoted_string _ | Template _) as sexp -> pp_simple sexp
  | List (_, sexps) ->
    Pp.vbox ~indent:1
      (if can_be_displayed_wrapped sexps then
        print_wrapped_list ~version sexps
      else
        pp_sexp_list ~version sexps)
  | Comment (loc, c) -> pp_comment loc c

and pp_sexp_list ~version sexps =
  Pp.char '(' ++ pp_list_with_comments (pp_sexp ~version) sexps ++ Pp.char ')'

let pp_top_sexp ~version sexp = pp_sexp ~version sexp ++ Pp.char '\n'

let pp_top_sexps ~version =
  Pp.concat_map ~sep:Pp.newline ~f:(pp_top_sexp ~version)

let write_file ~version ~path sexps =
  let f oc =
    let fmt = Format.formatter_of_out_channel oc in
    Format.fprintf fmt "%a%!" Pp.to_fmt (pp_top_sexps ~version sexps)
  in
  Io.with_file_out ~binary:true path ~f

let format_file ~version ~input ~output =
  let with_output f =
    match output with
    | None -> f stdout
    | Some output -> Io.with_file_out output ~f
  in
  match parse_file input with
  | OCaml_syntax loc -> (
    match input with
    | Some path ->
      Io.with_file_in path ~f:(fun ic ->
          with_output (fun oc -> Io.copy_channels ic oc))
    | None -> User_error.raise ~loc [ Pp.text "OCaml syntax is not supported." ]
    )
  | Sexps sexps ->
    with_output (fun oc ->
        let oc = Format.formatter_of_out_channel oc in
        Format.fprintf oc "%a%!" Pp.to_fmt (pp_top_sexps ~version sexps))
