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

(** Shell-style regular expressions *)

exception Parse_error

val glob :
  ?anchored:bool ->
  ?pathname:bool ->
  ?period:bool ->
  ?expand_braces:bool ->
  ?double_asterisk:bool ->
  string ->
  Core.t
(** Implements the semantics of shells patterns. The returned regular
    expression is unanchored by default.

    Character '*' matches any sequence of characters and character
    '?' matches a single character.
    A sequence '[...]' matches any one of the enclosed characters.
    A sequence '[^...]' or '[!...]' matches any character *but* the enclosed characters.
    A backslash escapes the following character.  The last character of the string cannot
    be a backslash.

    [anchored] controls whether the regular expression will only match entire
    strings. Defaults to false.

    [pathname]: If this flag is set, match a slash in string only with a slash in pattern
    and not by an asterisk ('*') or a question mark ('?') metacharacter, nor by a bracket
    expression ('[]') containing a slash. Defaults to true.

    [period]: If this flag is set, a leading period in string has to be matched exactly by
    a period in pattern. A period is considered to be leading if it is the first
    character in string, or if both [pathname] is set and the period immediately follows a
    slash. Defaults to true.

    If [expand_braces] is true, braced sets will expand into multiple globs,
    e.g. a\{x,y\}b\{1,2\} matches axb1, axb2, ayb1, ayb2.  As specified for bash, brace
    expansion is purely textual and can be nested. Defaults to false.

    [double_asterisk]: If this flag is set, double asterisks ('**') will match slash
    characters, even if [pathname] is set. The [period] flag still applies. Default to
    true. *)

val glob' : ?anchored:bool -> bool -> string -> Core.t
(** Same, but allows to choose whether dots at the beginning of a
    file name need to be explicitly matched (true) or not (false)

    @deprecated Use [glob ~period].
*)

val globx : ?anchored:bool -> string -> Core.t
(** This version of [glob] also recognizes the pattern \{..,..\}

    @deprecated Prefer [glob ~expand_braces:true].
*)

val globx' : ?anchored:bool -> bool -> string -> Core.t
(** This version of [glob'] also recognizes the pattern \{..,..\}

    @deprecated Prefer [glob ~expand_braces:true ~period].
*)
