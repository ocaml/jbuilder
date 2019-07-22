open StdLabels
open Shims

let prog_name = Filename.basename Sys.executable_name

let dump_ast = ref false

(* Table from positions to custom operators at these positions *)
let custom_operators = Hashtbl.create 128

module Wrap_lexer = struct
  let save_loc = Location.curr

  let restore_loc (lexbuf : Lexing.lexbuf) (loc : Location.t) =
    lexbuf.lex_start_p <- loc.loc_start;
    lexbuf.lex_curr_p <- loc.loc_end

  let encode_op (tok : Parser.token) op =
    (match tok with
     | LET -> "let__"
     | AND -> "and__"
     | _ -> assert false) ^ op

  let pending = Queue.create ()
  let add (x : Parser.token * _) = Queue.push x pending

  let register_custom_operator tok op (loc1 : Location.t) (loc2 : Location.t) =
    let op = encode_op tok op in
    Hashtbl.add custom_operators loc1.loc_start
      ({ loc1 with loc_end = loc2.loc_end }, op)

  let wrap (lexer : Lexing.lexbuf -> Parser.token) lb =
    if not (Queue.is_empty pending) then begin
      let tok, loc = Queue.pop pending in
      restore_loc lb loc;
      tok
    end else
      match lexer lb with
      | LET | AND as tok ->
        let loc = save_loc lb in
        (match Let_trail.op lb with
         | None -> ()
         | Some op -> register_custom_operator tok op loc (save_loc lb));
        restore_loc lb loc;
        tok
      | LPAREN -> begin
          let loc1 = save_loc lb in
          let tok2 = lexer lb in
          let loc2 = save_loc lb in
          let tok, loc =
            match tok2 with
            | LET | AND -> begin
                match Let_trail.op lb with
                | None ->
                  add (tok2, loc2);
                  (Parser.LPAREN, loc1)
                | Some op ->
                  let loc3 = save_loc lb in
                  match lexer lb with
                  | RPAREN ->
                    LIDENT (encode_op tok2 op),
                    { loc2 with loc_end = loc3.loc_end }
                  | tok4 ->
                    let loc4 = save_loc lb in
                    add (tok2, loc2);
                    add (tok4, loc4);
                    register_custom_operator tok2 op loc2 loc3;
                    (LPAREN, loc1)
              end
            | _ ->
              add (tok2, loc2);
              (LPAREN, loc1)
          in
          restore_loc lb loc;
          tok
        end
      | tok ->
        tok

  let () =
    Lexer.set_preprocessor
      (fun () -> Queue.clear pending)
      wrap
end

module Map_ast = struct
  open Ast_mapper
  open Asttypes
  open Parsetree
  open Ast_helper

  let get_op vb =
    match Hashtbl.find custom_operators vb.pvb_loc.loc_start with
    | exception Not_found -> None
    | (loc, op) ->
      Some
        (Exp.ident ~loc
           { txt = Lident op
           ; loc
           })

  let mapper =
    let super = default_mapper in
    let expr self expr =
      let expr =
        match expr.pexp_desc with
        | Pexp_let (rf, (vb :: _ as vbs), body) -> begin
            match get_op vb with
            | None -> expr
            | Some op ->
              if rf = Recursive then
                Location.raise_errorf ~loc:expr.pexp_loc
                  "Custom 'let' operators cannot be recursive";
              let patts, exprs =
                List.map vbs ~f:(fun vb ->
                  let { pvb_pat = patt
                      ; pvb_expr = expr
                      ; pvb_attributes = attrs
                      ; pvb_loc = loc
                      } = vb in
                  (match attrs with
                   | [] -> ()
                   | ({ loc; _ }, _) :: _ ->
                     Location.raise_errorf ~loc
                       "This attribute will be discarded");
                  let op =
                    match get_op vb with
                    | Some op ->
                      Hashtbl.remove custom_operators vb.pvb_loc.loc_start;
                      op
                    | None ->
                      Location.raise_errorf ~loc
                        "Custom 'and' operator expected, \
                         got standard 'and' keyword"
                  in
                  (patt, (loc, op, expr)))
                |> List.split
              in
              let patt =
                List.fold_left (List.tl patts)
                  ~init:(List.hd patts)
                  ~f:(fun acc patt ->
                    let loc = patt.ppat_loc in
                    Pat.tuple ~loc [acc; patt])
              in
              let vars =
                List.mapi exprs ~f:(fun i _ ->
                  Printf.sprintf "__future_syntax__%d__" i)
              in
              let pvars =
                List.map2 vars patts ~f:(fun v p ->
                  let loc = { p.ppat_loc with loc_ghost = true } in
                  Pat.var ~loc { txt = v; loc })
              in
              let evars =
                List.map2 vars exprs ~f:(fun v (_, _, e) ->
                  let loc = { e.pexp_loc with loc_ghost = true } in
                  Exp.ident ~loc { txt = Lident v; loc })
              in
              let expr =
                List.fold_left2 (List.tl evars) (List.tl exprs)
                  ~init:(List.hd evars)
                  ~f:(fun acc var (loc, op, _) ->
                    Exp.apply ~loc op
                      [ nolabel, acc
                      ; nolabel, var
                      ])
              in
              let body =
                let loc = expr.pexp_loc in
                Exp.apply ~loc op
                  [ nolabel, expr
                  ; nolabel, Exp.fun_ ~loc nolabel None patt body
                  ]
              in
              List.fold_right2 pvars exprs ~init:body
                ~f:(fun var (loc, _, expr) acc ->
                  Exp.let_ Nonrecursive ~loc
                    [Vb.mk ~loc var expr] acc)
          end
        | _ -> expr
      in
      super.expr self expr
    in
    { super with expr }

  let map f ast =
    let ast = f mapper ast in
    let fail _ (loc, _) =
      Location.raise_errorf ~loc
        "Invalid use of custom 'let' or 'and' operator"
    in
    Hashtbl.iter fail custom_operators;
    ast

  let structure = mapper.structure mapper
  let signature = mapper.signature mapper
end

let process_file fn ~magic ~parse ~print ~map ~mk_ext =
  let lexbuf = Lexing.from_channel (open_in_bin fn) in
  Location.init lexbuf fn;
  Location.input_lexbuf := Some lexbuf;
  let ast =
    try
      map (parse lexbuf)
    with exn ->
    match error_of_exn exn with
    | Some error ->
      if !dump_ast then
        [ mk_ext ?loc:None ?attrs:None
            (Ast_mapper.extension_of_error error) ]
      else begin
        Location.report_error Format.err_formatter error;
        exit 1
      end
    | None -> raise exn
  in
  if !dump_ast then begin
    set_binary_mode_out stdout true;
    output_string stdout magic;
    output_value stdout fn;
    output_value stdout ast;
    flush stdout
  end else
    Format.printf "%a@?" print ast

let process_file fn =
  let ext =
    match String.rindex fn '.' with
    | exception Not_found -> ""
    | i ->
      String.sub fn ~pos:i ~len:(String.length fn - i)
  in
  match ext with
  | ".ml" ->
    process_file fn
      ~magic:Config.ast_impl_magic_number
      ~parse:Parse.implementation
      ~print:Pprintast.structure
      ~map:Map_ast.structure
      ~mk_ext:Ast_helper.Str.extension
  | ".mli" ->
    process_file fn
      ~magic:Config.ast_intf_magic_number
      ~parse:Parse.interface
      ~print:Pprintast.signature
      ~map:Map_ast.signature
      ~mk_ext:Ast_helper.Sig.extension
  | _ ->
    Printf.eprintf "%s: Don't know what to do with %s.\n%!"
      prog_name fn;
    exit 2

let () =
  let args =
    Arg.align
      [ "-dump-ast", Arg.Set dump_ast,
        " Output a binary AST rather than a pretty-printed source file"
      ]
  in
  let usage = Printf.sprintf "Usage: %s [-dump-ast] FILES" prog_name in
  Arg.parse args process_file usage
