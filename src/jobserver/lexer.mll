{}

rule lex = parse
  | "--jobserver-auth=" ([^ ' ']+ as v) { Some v }
  | _ { lex lexbuf }
  | eof { None }

{}
