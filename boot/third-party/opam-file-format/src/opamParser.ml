(**************************************************************************)
(*                                                                        *)
(*    Copyright 2016-2018 OCamlPro                                        *)
(*                                                                        *)
(*  All rights reserved. This file is distributed under the terms of the  *)
(*  GNU Lesser General Public License version 2.1, with the special       *)
(*  exception on linking described in the file LICENSE.                   *)
(*                                                                        *)
(**************************************************************************)

(** Generic glue functions *)
let parse_from_string parse_fun str filename =
  let lexbuf = Lexing.from_string str in
  lexbuf.Lexing.lex_curr_p <-
    { lexbuf.Lexing.lex_curr_p with Lexing.pos_fname = filename };
  parse_fun OpamLexer.token lexbuf

let parse_from_channel parse_fun ic filename =
  let lexbuf = Lexing.from_channel ic in
  lexbuf.Lexing.lex_curr_p <-
    { lexbuf.Lexing.lex_curr_p with Lexing.pos_fname = filename };
  parse_fun OpamLexer.token lexbuf

let parse_from_file parse_fun filename =
  let ic = open_in_bin filename in
  try
    let r = parse_from_channel parse_fun ic filename in
    close_in ic;
    r
  with e -> close_in ic; raise e

(** raw parser entry points *)
let main = OpamBaseParser.main
let value = OpamBaseParser.value

(** file parsers *)
let main' filename lexer lexbuf = main lexer lexbuf filename
let string str filename = parse_from_string (main' filename) str filename
let channel ic filename = parse_from_channel (main' filename) ic filename
let file filename = parse_from_file (main' filename) filename

(** value parsers *)
let value_from_string = parse_from_string value
let value_from_channel = parse_from_channel value
let value_from_file = parse_from_file value
