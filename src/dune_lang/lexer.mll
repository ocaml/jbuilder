{
open! Stdune

open Stdune

module Token = struct
  module Comment = struct
    type t =
      | Lines of string list
      | Legacy

    let to_dyn =
      let open Dyn.Encoder in
      function
      | Legacy -> constr "Legacy" []
      | Lines l -> constr "Lines" [ list string l ]
  end

  type t =
    | Atom of Atom.t
    | Quoted_string of string
    | Lparen
    | Rparen
    | Sexp_comment
    | Eof
    | Template of Template.t
    | Comment of Comment.t
end

type t = with_comments:bool -> Lexing.lexbuf -> Token.t

let error ?(delta = 0) lexbuf message =
  let start = Lexing.lexeme_start_p lexbuf in
  let loc : Loc.t =
    { start = { start with pos_cnum = start.pos_cnum + delta }
    ; stop = Lexing.lexeme_end_p lexbuf
    }
  in
  User_error.raise ~loc [ Pp.text message ]

let invalid_dune_or_jbuild lexbuf =
  let start = Lexing.lexeme_start_p lexbuf in
  let fname = Filename.basename start.pos_fname in
  error lexbuf (sprintf "Invalid %s file" fname)

type escape_sequence =
  | Newline
  | Other

let eval_decimal_char c = Char.code c - Char.code '0'

let eval_decimal_escape c1 c2 c3 =
  (eval_decimal_char c1 * 100)
  + (eval_decimal_char c2 * 10)
  + eval_decimal_char c3

let eval_hex_char c =
  match c with
  | '0' .. '9' -> Char.code c - Char.code '0'
  | 'a' .. 'f' -> Char.code c - Char.code 'a' + 10
  | 'A' .. 'F' -> Char.code c - Char.code 'A' + 10
  | _ -> -1

let eval_hex_escape c1 c2 = (eval_hex_char c1 * 16) + eval_hex_char c2

type block_string_line_kind =
  | With_escape_sequences
  | Raw

module Template = struct
  include Template

  let dummy_loc =
    { Loc.
      start = Lexing.dummy_pos
    ; stop = Lexing.dummy_pos
    }

  let add_text parts s =
    match parts with
    | Template.Text s' :: parts -> Template.Text (s' ^ s) :: parts
    | _ -> Template.Text s :: parts

  let token parts ~quoted ~start (lexbuf : Lexing.lexbuf) =
    lexbuf.lex_start_p <- start;
    match parts with
    | [] | [Text ""] ->
      invalid_dune_or_jbuild lexbuf
    | [Text s] ->
      Token.Atom (Atom.of_string s)
    | _ ->
      Token.Template
        { quoted
        ; loc = dummy_loc
        ; parts = List.rev parts
        }

  module Buffer : sig
    val new_token : unit -> unit
    val get : unit -> Token.t
    val add_var : part -> unit
    val add_text_c : char -> unit
  end = struct
    type state =
      | String
      | Template of Template.part list

    let text_buf = Buffer.create 256

    let new_token () = Buffer.clear text_buf

    let take_buf () =
      let contents = Buffer.contents text_buf in
      Buffer.clear text_buf;
      contents

    let state = ref String

    let add_buf_to_parts parts =
      match take_buf () with
      | "" -> parts
      | t -> add_text parts t

    let get () =
      match !state with
      | String -> Token.Quoted_string (take_buf ())
      | Template parts ->
        state := String;
        begin match add_buf_to_parts parts with
        | [] -> assert false
        | [Text s] -> Quoted_string s
        | parts ->
          Token.Template
            { quoted = true
            ; loc = dummy_loc
            ; parts = List.rev parts
            }
        end

    let add_var v =
      match !state with
      | String ->
        state := Template (v :: add_buf_to_parts []);
      | Template parts ->
        let parts = add_buf_to_parts parts in
        state := Template (v::parts)

    let add_text_c = Buffer.add_char text_buf
  end
end
}

let comment_body = [^ '\n' '\r']*
let comment   = ';' comment_body
let newline   = '\r'? '\n'
let blank     = [' ' '\t' '\012']
let digit     = ['0'-'9']
let hexdigit  = ['0'-'9' 'a'-'f' 'A'-'F']

let atom_char = [^ ';' '(' ')' '"' '\000'-'\032' '\127'-'\255']
let varname_char = atom_char # [ ':' '%' '{' '}' ]

rule token with_comments = parse
  | newline
    { Lexing.new_line lexbuf; token with_comments lexbuf }
  | blank+
    { token with_comments lexbuf }
  | comment
    { if with_comments then
        comment_trail [String.drop (Lexing.lexeme lexbuf) 1] lexbuf
      else
        token with_comments lexbuf
    }
  | '('
    { Token.Lparen }
  | ')'
    { Rparen }
  | '"'
    { let start = Lexing.lexeme_start_p lexbuf in
      Template.Buffer.new_token ();
      let token = start_quoted_string lexbuf in
      lexbuf.lex_start_p <- start;
      token
    }
  | eof
    { Eof }
  | ""
    { atom [] (Lexing.lexeme_start_p lexbuf) lexbuf }

and comment_trail acc = parse
  | newline blank* ';' (comment_body as s)
    { comment_trail (s :: acc) lexbuf }
  | ""
    { Token.Comment (Lines (List.rev acc)) }

and atom acc start = parse
  | (atom_char # '%')+ as s
    { atom (Template.add_text acc s) start lexbuf }
  | "%{"
    { atom ((template_variable lexbuf) :: acc) start lexbuf }
  | "%"
    { atom (Template.add_text acc "%") start lexbuf }
  | ""
    { Template.token acc ~quoted:false ~start lexbuf }

and start_quoted_string = parse
  | "\\|"
    { block_string_start With_escape_sequences lexbuf }
  | "\\>"
    { block_string_start Raw lexbuf }
  | ""
    { quoted_string lexbuf }

and block_string_start kind = parse
  | newline
    { Lexing.new_line lexbuf;
      Template.Buffer.add_text_c '\n';
      block_string_after_newline lexbuf
    }
  | ' '
    { match kind with
      | With_escape_sequences -> block_string lexbuf
      | Raw -> raw_block_string lexbuf
    }
  | eof
    { Template.Buffer.get () }
  | _
    { error lexbuf "There must be at least one space after \"\\|"
    }

and block_string = parse
  | newline
    { Lexing.new_line lexbuf;
      Template.Buffer.add_text_c '\n';
      block_string_after_newline lexbuf
    }
  | '\\'
    { match escape_sequence lexbuf with
      | Newline -> block_string_after_newline lexbuf
      | Other   -> block_string               lexbuf
    }
  | "%{" {
      let var = template_variable lexbuf in
      Template.Buffer.add_var var;
      block_string lexbuf
    }
  | _ as c
    { Template.Buffer.add_text_c c;
      block_string lexbuf
    }
  | eof
    { Template.Buffer.get ()
    }

and block_string_after_newline = parse
  | blank* "\"\\|"
    { block_string_start With_escape_sequences lexbuf }
  | blank* "\"\\>"
    { block_string_start Raw lexbuf }
  | ""
    { Template.Buffer.get ()
    }

and raw_block_string = parse
  | newline
    { Lexing.new_line lexbuf;
      Template.Buffer.add_text_c '\n';
      block_string_after_newline lexbuf
    }
  | _ as c
    { Template.Buffer.add_text_c c;
      raw_block_string lexbuf
    }
  | eof
    { Template.Buffer.get ()
    }

and quoted_string = parse
  | '"'
    { Template.Buffer.get () }
  | '\\'
    { match escape_sequence lexbuf with
      | Newline -> quoted_string_after_escaped_newline lexbuf
      | Other   -> quoted_string                       lexbuf
    }
  | "%{"
    { Template.Buffer.add_var (template_variable lexbuf);
      quoted_string lexbuf
    }
  | newline
    { Lexing.new_line lexbuf;
      Template.Buffer.add_text_c '\n';
      quoted_string lexbuf
    }
  | _ as c
    { Template.Buffer.add_text_c c;
      quoted_string lexbuf
    }
  | eof
    { error lexbuf "unterminated quoted string"
    }

and escape_sequence = parse
  | newline
    { Lexing.new_line lexbuf;
      Newline }
  | '%'
    { Template.Buffer.add_text_c '%';
      Other
    }
  | ['\\' '\'' '"' 'n' 't' 'b' 'r'] as c
    { let c =
        match c with
        | 'n' -> '\n'
        | 'r' -> '\r'
        | 'b' -> '\b'
        | 't' -> '\t'
        | _   -> c
      in
      Template.Buffer.add_text_c c;
      Other
    }
  | (digit as c1) (digit as c2) (digit as c3)
    { let v = eval_decimal_escape c1 c2 c3 in
      if v > 255 then
        error lexbuf "escape sequence in quoted string out of range"
          ~delta:(-1);
      Template.Buffer.add_text_c (Char.chr v);
      Other
    }
  | digit digit digit
    { error lexbuf "escape sequence in quoted string out of range" ~delta:(-1);
    }
  | digit*
    { error lexbuf "unterminated decimal escape sequence" ~delta:(-1);
    }
  | 'x' (hexdigit as c1) (hexdigit as c2)
    { let v = eval_hex_escape c1 c2 in
      Template.Buffer.add_text_c (Char.chr v);
      Other
    }
  | 'x' hexdigit*
    { error lexbuf "unterminated hexadecimal escape sequence" ~delta:(-1);
    }
  | _
    { error lexbuf "unknown escape sequence" ~delta:(-1);
    }
  | eof
    { error lexbuf "unterminated escape sequence" ~delta:(-1);
    }

and quoted_string_after_escaped_newline = parse
  | [' ' '\t']*
    { quoted_string lexbuf }

and template_variable = parse
  | (varname_char+ as name) (':' ((':' | varname_char)* as payload))? '}'
    { let payload =
        match payload with
        | Some "" -> error lexbuf "payload after : in variable cannot be empty"
        | p -> p
      in
      Template.Var
        { loc =
            { start = Lexing.lexeme_start_p lexbuf
            ; stop = Lexing.lexeme_end_p lexbuf
            }
        ; name
        ; payload
        }
  }
  | '}' | eof
    { error lexbuf "%{...} forms cannot be empty" }
  | _ { error lexbuf "This character is not allowed inside %{...} forms" }

{
  let token ~with_comments lexbuf = token with_comments lexbuf
}
