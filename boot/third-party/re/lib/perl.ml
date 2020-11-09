(*
   RE - A regular expression library

   Copyright (C) 2001 Jerome Vouillon
   email: Jerome.Vouillon@pps.jussieu.fr

   This library is free software; you can redistribute it and/or
   modify it under the terms of the GNU Lesser General Public
   License as published by the Free Software Foundation, with
   linking exception; either version 2.1 of the License, or (at
   your option) any later version.

   This library is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   Lesser General Public License for more details.

   You should have received a copy of the GNU Lesser General Public
   License along with this library; if not, write to the Free Software
   Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA
*)

module Re = Core

exception Parse_error
exception Not_supported

let posix_class_of_string = function
  | "alpha"  -> Re.alpha
  | "alnum"  -> Re.alnum
  | "ascii"  -> Re.ascii
  | "blank"  -> Re.blank
  | "cntrl"  -> Re.cntrl
  | "digit"  -> Re.digit
  | "lower"  -> Re.lower
  | "print"  -> Re.print
  | "space"  -> Re.space
  | "upper"  -> Re.upper
  | "word"   -> Re.wordc
  | "punct"  -> Re.punct
  | "graph"  -> Re.graph
  | "xdigit" -> Re.xdigit
  | class_   -> invalid_arg ("Invalid pcre class: " ^ class_)

let posix_class_strings =
  [ "alpha" ; "alnum" ; "ascii"
  ; "blank" ; "cntrl" ; "digit"
  ; "lower" ; "print" ; "space"
  ; "upper" ; "word"  ; "punct"
  ; "graph" ; "xdigit" ]

let parse multiline dollar_endonly dotall ungreedy s =
  let i = ref 0 in
  let l = String.length s in
  let eos () = !i = l in
  let test c = not (eos ()) && s.[!i] = c in
  let accept c = let r = test c in if r then incr i; r in
  let accept_s s' =
    let len = String.length s' in
    try
      for j = 0 to len - 1 do
        try if s'.[j] <> s.[!i + j] then raise Exit
        with _ -> raise Exit
      done;
      i := !i + len;
      true
    with Exit -> false in
  let get () = let r = s.[!i] in incr i; r in
  let unget () = decr i in
  let greedy_mod r =
    let gr = accept '?' in
    let gr = if ungreedy then not gr else gr in
    if gr then Re.non_greedy r else Re.greedy r
  in
  let rec regexp () = regexp' (branch ())
  and regexp' left =
    if accept '|' then regexp' (Re.alt [left; branch ()]) else left
  and branch () = branch' []
  and branch' left =
    if eos () || test '|' || test ')' then Re.seq (List.rev left)
    else branch' (piece () :: left)
  and piece () =
    let r = atom () in
    if accept '*' then greedy_mod (Re.rep r) else
    if accept '+' then greedy_mod (Re.rep1 r) else
    if accept '?' then greedy_mod (Re.opt r) else
    if accept '{' then
      match integer () with
        Some i ->
          let j = if accept ',' then integer () else Some i in
          if not (accept '}') then raise Parse_error;
          begin match j with
            Some j when j < i -> raise Parse_error | _ -> ()
          end;
          greedy_mod (Re.repn r i j)
      | None ->
          unget (); r
    else
      r
  and atom () =
    if accept '.' then begin
      if dotall then Re.any else Re.notnl
    end else if accept '(' then begin
      if accept '?' then begin
        if accept ':' then begin
          let r = regexp () in
          if not (accept ')') then raise Parse_error;
          r
        end else if accept '#' then begin
          comment ()
        end else
          raise Parse_error
      end else begin
        let r = regexp () in
        if not (accept ')') then raise Parse_error;
        Re.group r
      end
    end else
    if accept '^' then begin
      if multiline then Re.bol else Re.bos
    end else if accept '$' then begin
      if multiline then Re.eol else if dollar_endonly then Re.leol else Re.eos
    end else if accept '[' then begin
      if accept '^' then
        Re.compl (bracket [])
      else
        Re.alt (bracket [])
    end else if accept '\\' then begin
(* XXX
   - Back-references
   - \cx (control-x), \e, \f, \n, \r, \t, \xhh, \ddd
*)
      if eos () then raise Parse_error;
      match get () with
        'w' ->
          Re.alt [Re.alnum; Re.char '_']
      | 'W' ->
          Re.compl [Re.alnum; Re.char '_']
      | 's' ->
          Re.space
      | 'S' ->
          Re.compl [Re.space]
      | 'd' ->
          Re.digit
      | 'D' ->
          Re.compl [Re.digit]
      | 'b' ->
          Re.alt [Re.bow; Re.eow]
      | 'B' ->
          Re.not_boundary
      | 'A' ->
          Re.bos
      | 'Z' ->
          Re.leol
      | 'z' ->
          Re.eos
      | 'G' ->
          Re.start
      | 'a'..'z' | 'A'..'Z' ->
          raise Parse_error
      | '0'..'9' ->
          raise Not_supported
      | c ->
          Re.char c
    end else begin
      if eos () then raise Parse_error;
      match get () with
        '*' | '+' | '?' | '{' | '\\' -> raise Parse_error
      |                 c            -> Re.char c
    end
  and integer () =
    if eos () then None else
    match get () with
      '0'..'9' as d -> integer' (Char.code d - Char.code '0')
    |     _        -> unget (); None
  and integer' i =
    if eos () then Some i else
    match get () with
      '0'..'9' as d ->
        let i' = 10 * i + (Char.code d - Char.code '0') in
        if i' < i then raise Parse_error;
        integer' i'
    | _ ->
        unget (); Some i
  and bracket s =
    if s <> [] && accept ']' then s else begin
      match char () with
      | `Char c ->
        if accept '-' then begin
          if accept ']' then Re.char c :: Re.char '-' :: s else begin
            match char () with
              `Char c' ->
              bracket (Re.rg c c' :: s)
            | `Set st' ->
              bracket (Re.char c :: Re.char '-' :: st' :: s)
          end
        end else
          bracket (Re.char c :: s)
      | `Set st -> bracket (st :: s)
    end
  and char () =
    if eos () then raise Parse_error;
    let c = get () in
    if c = '[' then begin
      if accept '=' then raise Not_supported;
      if accept ':' then
        let compl = accept '^' in
        let cls =
          try List.find accept_s posix_class_strings
          with Not_found -> raise Parse_error in
        if not (accept_s ":]") then raise Parse_error;
        let re =
          let posix_class = posix_class_of_string cls in
          if compl then Re.compl [posix_class] else posix_class in
        `Set (re)
      else if accept '.' then begin
        if eos () then raise Parse_error;
        let c = get () in
        if not (accept '.') then raise Not_supported;
        if not (accept ']') then raise Parse_error;
        `Char c
      end else
        `Char c
    end else if c = '\\' then begin
      if eos () then raise Parse_error;
      let c = get () in
(* XXX
   \127, ...
*)
      match c with
        'b' -> `Char '\008'
      | 'n' -> `Char '\n' (*XXX*)
      | 'r' -> `Char '\r' (*XXX*)
      | 't' -> `Char '\t' (*XXX*)
      | 'w' -> `Set (Re.alt [Re.alnum; Re.char '_'])
      | 'W' -> `Set (Re.compl [Re.alnum; Re.char '_'])
      | 's' -> `Set (Re.space)
      | 'S' -> `Set (Re.compl [Re.space])
      | 'd' -> `Set (Re.digit)
      | 'D' -> `Set (Re.compl [Re.digit])
      | 'a'..'z' | 'A'..'Z' ->
          raise Parse_error
      | '0'..'9' ->
          raise Not_supported
      | _ ->
          `Char c
    end else
      `Char c
  and comment () =
    if eos () then raise Parse_error;
    if accept ')' then Re.epsilon else begin incr i; comment () end
  in
  let res = regexp () in
  if not (eos ()) then raise Parse_error;
  res

type opt =
  [ `Ungreedy | `Dotall | `Dollar_endonly
  | `Multiline | `Anchored | `Caseless ]

let re  ?(opts = []) s =
  let r =
    parse
      (List.memq `Multiline opts) (List.memq `Dollar_endonly opts)
      (List.memq `Dotall opts) (List.memq `Ungreedy opts)
      s
  in
  let r = if List.memq `Anchored opts then Re.seq [Re.start; r] else r in
  let r = if List.memq `Caseless opts then Re.no_case r else r in
  r

let compile = Re.compile
let compile_pat ?(opts = []) s = compile (re ~opts s)
