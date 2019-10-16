open! Stdune

let () = Printexc.record_backtrace true

(* Test that all strings of length <= 3 such that [Dune_lang.Atom.is_valid s]
   are recignized as atoms by the parser *)

type syntax =
  | Dune
  | Jbuild

let string_of_syntax = function
  | Dune -> "dune"
  | Jbuild -> "jbuild"

let jbuild_atom_is_valid str =
  let len = String.length str in
  len > 0
  &&
  let rec loop ix =
    match str.[ix] with
    | '"'
    | '('
    | ')'
    | ';' ->
      true
    | '|' ->
      ix > 0
      &&
      let next = ix - 1 in
      str.[next] = '#' || loop next
    | '#' ->
      ix > 0
      &&
      let next = ix - 1 in
      str.[next] = '|' || loop next
    | ' '
    | '\t'
    | '\n'
    | '\012'
    | '\r' ->
      true
    | _ -> ix > 0 && loop (ix - 1)
  in
  not (loop (len - 1))

let () =
  [ (Dune, Dune_lang.Lexer.token, fun s -> Dune_lang.Atom.is_valid s)
  ; ( Jbuild
    , Jbuild_support.JbuildLexer.token
    , fun s -> jbuild_atom_is_valid s )
  ]
  |> List.iter ~f:(fun (syntax, lexer, validator) ->
         for len = 0 to 3 do
           let s = Bytes.create len in
           for i = 0 to (1 lsl (len * 8)) - 1 do
             if len > 0 then Bytes.set s 0 (Char.chr (i land 0xff));
             if len > 1 then Bytes.set s 1 (Char.chr ((i lsr 4) land 0xff));
             if len > 2 then Bytes.set s 2 (Char.chr ((i lsr 8) land 0xff));
             let s = Bytes.unsafe_to_string s in
             let parser_recognizes_as_atom =
               match
                 Dune_lang.Parser.parse_string ~lexer ~fname:"" ~mode:Single s
               with
               | exception _ -> false
               | Atom (_, A s') -> s = s'
               | _ -> false
             in
             let printed_as_atom =
               match Dune_lang.atom_or_quoted_string s with
               | Atom _ -> true
               | _ -> false
             in
             let valid_dune_atom = validator s in
             if valid_dune_atom <> parser_recognizes_as_atom then (
               Printf.eprintf
                 "Dune_lang.Atom.is_valid error:\n\
                  - syntax = %s\n\
                  - s = %S\n\
                  - Dune_lang.Atom.is_valid s = %B\n\
                  - parser_recognizes_as_atom = %B\n"
                 (string_of_syntax syntax) s valid_dune_atom
                 parser_recognizes_as_atom;
               exit 1
             );
             if printed_as_atom && not parser_recognizes_as_atom then (
               Printf.eprintf
                 "Dune_lang.Atom.atom_or_quoted_string error:\n\
                  - syntax = %s\n\
                  - s = %S\n\
                  - printed_as_atom = %B\n\
                  - parser_recognizes_as_atom = %B\n"
                 (string_of_syntax syntax) s printed_as_atom
                 parser_recognizes_as_atom;
               exit 1
             )
           done
         done)
