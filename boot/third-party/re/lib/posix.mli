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

(**
References:
  - {{: http://www.opengroup.org/onlinepubs/007908799/xbd/re.html} re}
  - {{: http://www.opengroup.org/onlinepubs/007908799/xsh/regcomp.html} regcomp}

Example of how to use this module (to parse some IRC logs):

{[
type msg = {
  time:string;
  author:string;
  content:string;
}

let re = Core.compile (Re_posix.re "([^:].*:[^:]*:[^:]{2})<.([^>]+)> (.+)$")

(* parse a line *)
let match_line line =
  try
    let substrings = Core.exec re line in
    let groups = Core.get_all substrings in
    (* groups can be obtained directly by index within [substrings] *)
    Some {time=groups.(1); author=groups.(2); content=groups.(3)}
  with Not_found ->
    None (* regex didn't match *)
]}
*)

(** XXX Character classes *)

exception Parse_error
exception Not_supported
(** Errors that can be raised during the parsing of the regular expression *)

type opt = [`ICase | `NoSub | `Newline]

val re : ?opts:(opt list) -> string -> Core.t
(** Parsing of a Posix extended regular expression *)

val compile : Core.t -> Core.re
(** Regular expression compilation *)

val compile_pat : ?opts:(opt list) -> string -> Core.re
(** [compile r] is defined as [Core.compile (Core.longest r)] *)

(*
Deviation from the standard / ambiguities in the standard
---------------------------------------------------------
We tested the behavior of the Linux library (glibc) and the Solaris
library.

(1) An expression [efg] should be parsed as [(ef)g].
    All implementations parse it as [e(fg)].
(2) When matching the pattern "((a)|b)*" against the string "ab",
    the sub-expression "((a)|b)" should match "b", and the
    sub-expression "(a)" should not match anything.
    In both implementation, the sub-expression "(a)" matches "a".
(3) When matching the pattern "(aa?)*" against the string "aaa", it is
    not clear whether the final match of the sub-expression "(aa?)"  is
    the last "a" (all matches of the sub-expression are successively
    maximized), or "aa" (the final match is maximized).
    Both implementations implements the first case.
(4) When matching the pattern "((a?)|b)*" against the string "ab",
    the sub-expression "((a?)|b)" should match the empty string at the
    end of the string (it is better to match the empty string than to
    match nothing).
    In both implementations, this sub-expression matches "b".
    (Strangely, in the Linux implementation, the sub-expression "(a?)"
     correctly matches the empty string at the end of the string)

This library behaves the same way as the other libraries for all
points, except for (2) and (4) where it follows the standard.

The behavior of this library in theses four cases may change in future
releases.
*)
