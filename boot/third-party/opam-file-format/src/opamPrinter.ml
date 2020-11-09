(**************************************************************************)
(*                                                                        *)
(*    Copyright 2012-2020 OCamlPro                                        *)
(*    Copyright 2012 INRIA                                                *)
(*                                                                        *)
(*  All rights reserved. This file is distributed under the terms of the  *)
(*  GNU Lesser General Public License version 2.1, with the special       *)
(*  exception on linking described in the file LICENSE.                   *)
(*                                                                        *)
(**************************************************************************)

open OpamParserTypes

let relop = function
  | `Eq  -> "="
  | `Neq -> "!="
  | `Geq -> ">="
  | `Gt  -> ">"
  | `Leq -> "<="
  | `Lt  -> "<"
  | `Sem -> "~"

let logop = function
  | `And -> "&"
  | `Or -> "|"

let pfxop = function
  | `Not -> "!"
  | `Defined -> "?"

let env_update_op = function
  | Eq -> "="
  | PlusEq -> "+="
  | EqPlus -> "=+"
  | EqPlusEq -> "=+="
  | ColonEq -> ":="
  | EqColon -> "=:"

let escape_string ?(triple=false) s =
  let len = String.length s in
  let buf = Buffer.create (len * 2) in
  for i = 0 to len -1 do
    let c = s.[i] in
    (match c with
     | '"'
       when not triple
         || (i < len - 2 && s.[i+1] = '"' && s.[i+2] = '"')
         || i = len - 1 ->
       Buffer.add_char buf '\\'
     | '\\' -> Buffer.add_char buf '\\'
     | _ -> ());
    Buffer.add_char buf c
  done;
  Buffer.contents buf

let rec format_value fmt = function
  | Relop (_,op,l,r) ->
    Format.fprintf fmt "@[<h>%a %s@ %a@]"
      format_value l (relop op) format_value r
  | Logop (_,op,l,r) ->
    Format.fprintf fmt "@[<hv>%a %s@ %a@]"
      format_value l (logop op) format_value r
  | Pfxop (_,op,r) ->
    Format.fprintf fmt "@[<h>%s%a@]" (pfxop op) format_value r
  | Prefix_relop (_,op,r) ->
    Format.fprintf fmt "@[<h>%s@ %a@]"
      (relop op) format_value r
  | Ident (_,s)     -> Format.fprintf fmt "%s" s
  | Int (_,i)       -> Format.fprintf fmt "%d" i
  | Bool (_,b)      -> Format.fprintf fmt "%b" b
  | String (_,s)    ->
    if String.contains s '\n'
    then Format.fprintf fmt "\"\"\"%s%s\"\"\""
        (if s.[0] = '\n' then "" else "\\\n")
        (escape_string ~triple:true s)
    else Format.fprintf fmt "\"%s\"" (escape_string s)
  | List (_, l) ->
    Format.fprintf fmt "@[<hv>[@;<0 2>@[<hv>%a@]@,]@]" format_values l
  | Group (_,g)     -> Format.fprintf fmt "@[<hv>(%a)@]" format_values g
  | Option(_,v,l)   -> Format.fprintf fmt "@[<hov 2>%a@ {@[<hv>%a@]}@]"
                         format_value v format_values l
  | Env_binding (_,id,op,v) ->
    Format.fprintf fmt "@[<h>%a %s@ %a@]"
      format_value id (env_update_op op) format_value v

and format_values fmt = function
  | [] -> ()
  | [v] -> format_value fmt v
  | v::r ->
    format_value fmt v;
    Format.pp_print_space fmt ();
    format_values fmt r

let value v =
  format_value Format.str_formatter v; Format.flush_str_formatter ()

let value_list vl =
  Format.fprintf Format.str_formatter "@[<hv>%a@]" format_values vl;
  Format.flush_str_formatter ()

let rec format_item fmt = function
  | Variable (_, _, List (_,[])) -> ()
  | Variable (_, _, List (_,[List(_,[])])) -> ()
  | Variable (_, i, List (_,l)) ->
    if List.exists
        (function List _ | Option (_,_,_::_) -> true | _ -> false)
        l
    then Format.fprintf fmt "@[<v>%s: [@;<0 2>@[<v>%a@]@,]@]"
        i format_values l
    else Format.fprintf fmt "@[<hv>%s: [@;<0 2>@[<hv>%a@]@,]@]"
        i format_values l
  | Variable (_, i, (String (_,s) as v)) when String.contains s '\n' ->
    Format.fprintf fmt "@[<hov 0>%s: %a@]" i format_value v
  | Variable (_, i, v) ->
    Format.fprintf fmt "@[<hov 2>%s:@ %a@]" i format_value v
  | Section (_,s) ->
    Format.fprintf fmt "@[<v 0>%s %s{@;<0 2>@[<v>%a@]@,}@]"
      s.section_kind
      (match s.section_name with
       | Some s -> Printf.sprintf "\"%s\" " (escape_string s)
       | None -> "")
      format_items s.section_items
and format_items fmt is =
  Format.pp_open_vbox fmt 0;
  (match is with
   | [] -> ()
   | i::r ->
     format_item fmt i;
     List.iter (fun i -> Format.pp_print_cut fmt (); format_item fmt i) r);
  Format.pp_close_box fmt ()

let format_opamfile fmt f =
  format_items fmt f.file_contents;
  Format.pp_print_newline fmt ()

let items l =
  format_items Format.str_formatter l; Format.flush_str_formatter ()

let opamfile f =
  items f.file_contents

let rec value_equals v1 v2 = match v1, v2 with
  | Bool (_, b1), Bool (_, b2) -> b1 = b2
  | Int (_, i1), Int (_, i2) -> i1 = i2
  | String (_, s1), String (_, s2) -> s1 = s2
  | Relop (_, r1, va1, vb1), Relop (_, r2, va2, vb2) ->
    r1 = r2 && value_equals va1 va2 && value_equals vb1 vb2
  | Prefix_relop (_, r1, v1), Prefix_relop (_, r2, v2) ->
    r1 = r2 && value_equals v1 v2
  | Logop (_, l1, va1, vb1), Logop (_, l2, va2, vb2) ->
    l1 = l2 && value_equals va1 va2 && value_equals vb1 vb2
  | Pfxop (_, p1, v1), Pfxop (_, p2, v2) ->
    p1 = p2 && value_equals v1 v2
  | Ident (_, s1), Ident (_, s2) ->
    s1 = s2
  | List (_, vl1), List (_, vl2) ->
    (try List.for_all2 value_equals vl1 vl2 with Invalid_argument _ -> false)
  | Group (_, vl1), Group (_, vl2) ->
    (try List.for_all2 value_equals vl1 vl2 with Invalid_argument _ -> false)
  | Option (_, v1, vl1), Option (_, v2, vl2) ->
    value_equals v1 v2 &&
    (try List.for_all2 value_equals vl1 vl2 with Invalid_argument _ -> false)
  | Env_binding (_, v1, op1, vx1), Env_binding (_, v2, op2, vx2) ->
    op1 = op2 && value_equals v1 v2 && value_equals vx1 vx2
  | _ -> false

let rec opamfile_item_equals i1 i2 = match i1, i2 with
  | Variable (_, n1, v1), Variable (_, n2, v2) ->
    n1 = n2 && value_equals v1 v2
  | Section (_, s1), Section (_, s2) ->
    s1.section_kind = s2.section_kind &&
    s1.section_name = s2.section_name &&
    (try List.for_all2 opamfile_item_equals s1.section_items s2.section_items
     with Invalid_argument _ -> false)
  | _ -> false

module Normalise = struct
  let escape_string s =
    let len = String.length s in
    let buf = Buffer.create (len * 2) in
    Buffer.add_char buf '"';
    for i = 0 to len -1 do
      match s.[i] with
      | '\\' | '"' as c -> Buffer.add_char buf '\\'; Buffer.add_char buf c
      | '\n' -> Buffer.add_string buf "\\n"
      | c -> Buffer.add_char buf c
    done;
    Buffer.add_char buf '"';
    Buffer.contents buf

  let rec value = function
    | Relop (_,op,l,r) ->
      String.concat " " [value l; relop op; value r]
    | Logop (_,op,l,r) ->
      String.concat " " [value l; logop op; value r]
    | Pfxop (_,op,r) ->
      String.concat " " [pfxop op; value r]
    | Prefix_relop (_,op,r) ->
      String.concat " " [relop op; value r]
    | Ident (_,s) -> s
    | Int (_,i) -> string_of_int i
    | Bool (_,b) -> string_of_bool b
    | String (_,s) -> escape_string s
    | List (_, l) -> Printf.sprintf "[%s]" (String.concat " " (List.map value l))
    | Group (_,g) -> Printf.sprintf "(%s)" (String.concat " " (List.map value g))
    | Option(_,v,l) ->
      Printf.sprintf "%s {%s}" (value v) (String.concat " " (List.map value l))
    | Env_binding (_,id,op,v) ->
      String.concat " "
        [value id; env_update_op op; value v]

  let rec item = function
    | Variable (_, _, List (_,([]|[List(_,[])]))) -> ""
    | Variable (_, i, List (_,l)) ->
      Printf.sprintf "%s: [%s]" i (String.concat " " (List.map value l))
    | Variable (_, i, v) -> String.concat ": " [i; value v]
    | Section (_,s) ->
      Printf.sprintf "%s %s{\n%s\n}"
        s.section_kind
        (match s.section_name with
         | Some s -> escape_string s ^ " "
         | None -> "")
        (String.concat "\n" (List.map item s.section_items))

  let item_order a b = match a,b with
    | Section _, Variable _ -> 1
    | Variable _, Section _ -> -1
    | Variable (_,i,_), Variable (_,j,_) -> String.compare i j
    | Section (_,s), Section (_,t) ->
      let r = String.compare s.section_kind t.section_kind in
      if r <> 0 then r
      else compare s.section_name t.section_name

  let items its =
    let its = List.sort item_order its in
    String.concat "\n" (List.map item its) ^ "\n"

  let opamfile f = items f.file_contents
end

module Preserved = struct
  let items txt orig f =
    let pos_index =
      let lines_index =
        let rec aux acc s =
          let until =
            try Some (String.index_from s (List.hd acc) '\n')
            with Not_found -> None
          in
          match until with
          | Some until -> aux (until+1 :: acc) s
          | None -> Array.of_list (List.rev acc)
        in
        aux [0] txt
      in
      fun (_file, li, col) -> lines_index.(li - 1) + col
    in
    let get_substring start_pos rest =
      let start = pos_index start_pos in
      let stop = match rest with
        | (Section (pos,_) | Variable (pos,_,_)) :: _ -> pos_index pos
        | [] -> String.length txt
      in
      if stop < start then raise Exit
      else String.sub txt start (stop - start)
    in
    let list_take f l =
      let rec aux acc = function
        | [] -> None, List.rev acc
        | x::r ->
          if f x then Some x, List.rev_append acc r
          else aux (x::acc) r
      in
      aux [] l
    in
    let is_variable name = function
      | Variable (_, name1, _v1) -> name = name1
      | _ -> false
    in
    let is_section kind name = function
      | Section (_, {section_kind; section_name; _}) ->
        kind = section_kind && name = section_name
      | _ -> false
    in
    let rec aux acc f = function
      | Variable (pos, name, v) :: r ->
        (match list_take (is_variable name) f with
         | Some (Variable (_, _, v1)), f when value_equals v v1 ->
           aux (get_substring pos r :: acc) f r
         | Some item, f ->
           aux ((items [item] ^ "\n") :: acc) f r
         | None, f ->
           aux acc f r)
      | Section (pos, {section_kind; section_name; _}) as sec :: r ->
        (match list_take (is_section section_kind section_name) f with
         | Some s, f when opamfile_item_equals sec s ->
           aux (get_substring pos r :: acc) f r
         | Some item, f ->
           aux ((items [item] ^ "\n") :: acc) f r
         | None, f -> aux acc f r)
      | [] ->
        let remaining = match f with
          | [] -> []
          | f -> [items f ^ "\n"]
        in
        List.rev_append acc remaining
    in
    let header = [get_substring ("",1,0) orig] in
    String.concat "" (aux header f orig)

  let opamfile ?format_from f =
    let orig_file = match format_from with
      | Some name -> name
      | None -> f.file_name
    in
    let txt =
      let b = Buffer.create 4096 in
      let ic = open_in orig_file in
      try while true do Buffer.add_channel b ic 4096 done; assert false with
      | End_of_file -> close_in ic; Buffer.contents b
      | e -> close_in ic; raise e
    in
    let orig = OpamParser.string txt orig_file in
    items txt orig.file_contents f.file_contents

end
