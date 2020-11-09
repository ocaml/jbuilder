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

let parse s =
  let i = ref 0 in
  let l = String.length s in
  let eos () = !i = l in
  let test c = not (eos ()) && s.[!i] = c in
  let test2 c c' = !i + 1 < l && s.[!i] = c && s.[!i + 1] = c' in
  let accept c = let r = test c in if r then incr i; r in
  let accept2 c c' = let r = test2 c c' in if r then i := !i + 2; r in
  let get () = let r = s.[!i] in incr i; r in

  let rec regexp () = regexp' (branch ())
  and regexp' left =
    if accept2 '\\' '|' then regexp' (Re.alt [left; branch ()]) else left
  and branch () = branch' []
  and branch' left =
    if eos () || test2 '\\' '|' || test2 '\\' ')' then Re.seq (List.rev left)
    else branch' (piece () :: left)
  and piece () =
    let r = atom () in
    if accept '*' then Re.rep r else
    if accept '+' then Re.rep1 r else
    if accept '?' then Re.opt r else
    r
  and atom () =
    if accept '.' then begin
      Re.notnl
    end else if accept '^' then begin
      Re.bol
    end else if accept '$' then begin
      Re.eol
    end else if accept '[' then begin
      if accept '^' then
        Re.compl (bracket [])
      else
        Re.alt (bracket [])
    end else if accept '\\' then begin
      if accept '(' then begin
        let r = regexp () in
        if not (accept2 '\\' ')') then raise Parse_error;
        Re.group r
      end else if accept '`' then
        Re.bos
      else if accept '\'' then
        Re.eos
      else if accept '=' then
        Re.start
      else if accept 'b' then
        Re.alt [Re.bow; Re.eow]
      else if accept 'B' then
        Re.not_boundary
      else if accept '<' then
        Re.bow
      else if accept '>' then
        Re.eow
      else if accept 'w' then
        Re.alt [Re.alnum; Re.char '_']
      else if accept 'W' then
        Re.compl [Re.alnum; Re.char '_']
      else begin
        if eos () then raise Parse_error;
        match get () with
          '*' | '+' | '?' | '[' | ']' | '.' | '^' | '$' | '\\' as c ->
            Re.char c
        | '0' .. '9' ->
            raise Not_supported
        | _ ->
            raise Parse_error
      end
    end else begin
      if eos () then raise Parse_error;
      match get () with
        '*' | '+' | '?' -> raise Parse_error
      |        c        -> Re.char c
    end
  and bracket s =
    if s <> [] && accept ']' then s else begin
      let c = char () in
      if accept '-' then begin
        if accept ']' then Re.char c :: Re.char '-' :: s else begin
          let c' = char () in
          bracket (Re.rg c c' :: s)
        end
      end else
        bracket (Re.char c :: s)
    end
  and char () =
    if eos () then raise Parse_error;
    get ()
  in
  let res = regexp () in
  if not (eos ()) then raise Parse_error;
  res

let re ?(case = true) s = let r = parse s in if case then r else Re.no_case r

let compile = Re.compile
let compile_pat ?(case = true) s = compile (re ~case s)
