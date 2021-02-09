open Stdune

let error (loc : Loc.t) message = User_error.raise ~loc [ Pp.text message ]

(* To avoid writing two parsers, one for the Cst and one for the Ast, we write
   only one that work for both.

   The natural thing to do would be to have parser that produce [Cst.t] value
   and drop comment for the [Ast.t] one. However the most used parser is the one
   producing Ast one, so it is the one we want to go fast. As a result, we
   encode comment as special [Ast.t] values and decode them for the [Cst.t]
   parser.

   We could also do clever things with GADTs, but it will add type variables
   everywhere which is annoying. *)
let rec cst_of_encoded_ast (x : Ast.t) : Cst.t =
  match x with
  | Template t -> Template t
  | Quoted_string (loc, s) -> Quoted_string (loc, s)
  | List (loc, l) -> List (loc, List.map l ~f:cst_of_encoded_ast)
  | Atom (loc, (A s as atom)) -> (
    match s.[0] with
    | '\000' -> Comment (loc, Lines (String.drop s 1 |> String.split ~on:'\n'))
    | '\001' -> Comment (loc, Legacy)
    | _ -> Atom (loc, atom))

module Mode = struct
  type 'a t =
    | Single : Ast.t t
    | Many : Ast.t list t
    | Many_as_one : Ast.t t
    | Cst : Cst.t list t

  let with_comments : type a. a t -> bool = function
    | Single -> false
    | Many -> false
    | Many_as_one -> false
    | Cst -> true

  let make_result : type a. a t -> Lexing.lexbuf -> Ast.t list -> a =
   fun t lexbuf sexps ->
    match t with
    | Single -> (
      match sexps with
      | [ sexp ] -> sexp
      | [] -> error (Loc.of_lexbuf lexbuf) "no s-expression found in input"
      | _ :: sexp :: _ ->
        error (Ast.loc sexp) "too many s-expressions found in input")
    | Many -> sexps
    | Many_as_one -> (
      match sexps with
      | [] -> List (Loc.in_file (Path.of_string lexbuf.lex_curr_p.pos_fname), [])
      | x :: l ->
        let last = List.fold_left l ~init:x ~f:(fun _ x -> x) in
        let loc = { (Ast.loc x) with stop = (Ast.loc last).stop } in
        List (loc, x :: l))
    | Cst -> List.map sexps ~f:cst_of_encoded_ast
end

let rec loop with_comments depth lexer lexbuf acc =
  match (lexer ~with_comments lexbuf : Lexer.Token.t) with
  | Atom a ->
    let loc = Loc.of_lexbuf lexbuf in
    loop with_comments depth lexer lexbuf (Ast.Atom (loc, a) :: acc)
  | Quoted_string s ->
    let loc = Loc.of_lexbuf lexbuf in
    loop with_comments depth lexer lexbuf (Quoted_string (loc, s) :: acc)
  | Template t ->
    let loc = Loc.of_lexbuf lexbuf in
    loop with_comments depth lexer lexbuf (Template { t with loc } :: acc)
  | Lparen ->
    let start = Lexing.lexeme_start_p lexbuf in
    let sexps = loop with_comments (depth + 1) lexer lexbuf [] in
    let stop = Lexing.lexeme_end_p lexbuf in
    loop with_comments depth lexer lexbuf (List ({ start; stop }, sexps) :: acc)
  | Rparen ->
    if depth = 0 then
      error (Loc.of_lexbuf lexbuf)
        "right parenthesis without matching left parenthesis";
    List.rev acc
  | Sexp_comment ->
    let sexps =
      let loc = Loc.of_lexbuf lexbuf in
      match loop with_comments depth lexer lexbuf [] with
      | commented :: sexps ->
        if not with_comments then
          sexps
        else
          Atom (Ast.loc commented, Atom.of_string "\001") :: sexps
      | [] -> error loc "s-expression missing after #;"
    in
    List.rev_append acc sexps
  | Eof ->
    if depth > 0 then
      error (Loc.of_lexbuf lexbuf) "unclosed parenthesis at end of input";
    List.rev acc
  | Comment comment ->
    if not with_comments then
      loop false depth lexer lexbuf acc
    else
      let loc = Loc.of_lexbuf lexbuf in
      let encoded =
        match comment with
        | Lines lines -> "\000" ^ String.concat lines ~sep:"\n"
        | Legacy -> "\001"
      in
      loop with_comments depth lexer lexbuf
        (Atom (loc, Atom.of_string encoded) :: acc)

let parse ~mode ?(lexer = Lexer.token) lexbuf =
  let with_comments = Mode.with_comments mode in
  loop with_comments 0 lexer lexbuf [] |> Mode.make_result mode lexbuf

let parse_string ~fname ~mode ?lexer str =
  let lb = Lexbuf.from_string ~fname str in
  parse ~mode ?lexer lb

let load ?lexer path ~mode =
  Io.with_lexbuf_from_file path ~f:(parse ~mode ?lexer)

let insert_comments csts comments =
  (* To insert the comments, we tokenize the csts, reconciliate the token
     streams and parse the result again. This is not the fastest implementation,
     but at least it is simple. *)
  let compare (a, _) (b, _) =
    Int.compare a.Loc.start.pos_cnum b.Loc.start.pos_cnum
  in
  let rec reconciliate acc tokens1 tokens2 =
    match (tokens1, tokens2) with
    | [], l
    | l, [] ->
      List.rev_append acc l
    | tok1 :: rest1, tok2 :: rest2 -> (
      match compare tok1 tok2 with
      | Eq
      | Lt ->
        reconciliate (tok1 :: acc) rest1 tokens2
      | Gt -> reconciliate (tok2 :: acc) tokens1 rest2)
  in
  let tokens =
    reconciliate [] (Cst.tokenize csts)
      (List.sort comments ~compare
      |> List.map ~f:(fun (loc, comment) -> (loc, Lexer.Token.Comment comment))
      )
  in
  let tokens = ref tokens in
  let lexer ~with_comments:_ (lb : Lexing.lexbuf) =
    match !tokens with
    | [] ->
      lb.lex_curr_p <- lb.lex_start_p;
      Lexer.Token.Eof
    | ({ start; stop }, tok) :: rest ->
      tokens := rest;
      lb.lex_start_p <- start;
      lb.lex_curr_p <- stop;
      tok
  in
  parse (Lexing.from_string "") ~lexer ~mode:Cst
