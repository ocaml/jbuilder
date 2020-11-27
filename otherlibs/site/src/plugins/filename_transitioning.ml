(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*           Xavier Leroy and Damien Doligez, INRIA Rocquencourt          *)
(*                                                                        *)
(*   Copyright 1996 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

let generic_quote quotequote s =
  let l = String.length s in
  let b = Buffer.create (l + 20) in
  Buffer.add_char b '\'';
  for i = 0 to l - 1 do
    if s.[i] = '\'' then
      Buffer.add_string b quotequote
    else
      Buffer.add_char b s.[i]
  done;
  Buffer.add_char b '\'';
  Buffer.contents b

module type SYSDEPS = sig
  val null : string

  val quote_command :
       string
    -> ?stdin:string
    -> ?stdout:string
    -> ?stderr:string
    -> string list
    -> string
end

module Unix : SYSDEPS = struct
  let null = "/dev/null"

  let quote = generic_quote "'\\''"

  let quote_command cmd ?stdin ?stdout ?stderr args =
    String.concat " " (List.map quote (cmd :: args))
    ^ ( match stdin with
      | None -> ""
      | Some f -> " <" ^ quote f )
    ^ ( match stdout with
      | None -> ""
      | Some f -> " >" ^ quote f )
    ^
    match stderr with
    | None -> ""
    | Some f ->
      if stderr = stdout then
        " 2>&1"
      else
        " 2>" ^ quote f
end

module Win32 : SYSDEPS = struct
  let null = "NUL"

  let quote s =
    let l = String.length s in
    let b = Buffer.create (l + 20) in
    Buffer.add_char b '\"';
    let rec loop i =
      if i = l then
        Buffer.add_char b '\"'
      else
        match s.[i] with
        | '\"' -> loop_bs 0 i
        | '\\' -> loop_bs 0 i
        | c ->
          Buffer.add_char b c;
          loop (i + 1)
    and loop_bs n i =
      if i = l then (
        Buffer.add_char b '\"';
        add_bs n
      ) else
        match s.[i] with
        | '\"' ->
          add_bs ((2 * n) + 1);
          Buffer.add_char b '\"';
          loop (i + 1)
        | '\\' -> loop_bs (n + 1) (i + 1)
        | _ ->
          add_bs n;
          loop i
    and add_bs n =
      for _j = 1 to n do
        Buffer.add_char b '\\'
      done
    in
    loop 0;
    Buffer.contents b

  (* Quoting commands for execution by cmd.exe is difficult. 1- Each argument is
     first quoted using the "quote" function above, to protect it against the
     processing performed by the C runtime system, then cmd.exe's special
     characters are escaped with '^', using the "quote_cmd" function below. For
     more details, see
     https://blogs.msdn.microsoft.com/twistylittlepassagesallalike/2011/04/23 2-
     The command and the redirection files, if any, must be double-quoted in
     case they contain spaces. This quoting is interpreted by cmd.exe, not by
     the C runtime system, hence the "quote" function above cannot be used. The
     two characters we don't know how to quote inside a double-quoted cmd.exe
     string are double-quote and percent. We just fail if the command name or
     the redirection file names contain a double quote (not allowed in Windows
     file names, anyway) or a percent. See function "quote_cmd_filename" below.
     3- The whole string passed to Sys.command is then enclosed in double
     quotes, which are immediately stripped by cmd.exe. Otherwise, some of the
     double quotes from step 2 above can be misparsed. See e.g.
     https://stackoverflow.com/a/9965141 *)
  let quote_cmd s =
    let b = Buffer.create (String.length s + 20) in
    String.iter
      (fun c ->
        match c with
        | '('
        | ')'
        | '!'
        | '^'
        | '%'
        | '\"'
        | '<'
        | '>'
        | '&'
        | '|' ->
          Buffer.add_char b '^';
          Buffer.add_char b c
        | _ -> Buffer.add_char b c)
      s;
    Buffer.contents b

  let quote_cmd_filename f =
    if String.contains f '\"' || String.contains f '%' then
      failwith ("Filename.quote_command: bad file name " ^ f)
    else if String.contains f ' ' then
      "\"" ^ f ^ "\""
    else
      f

  (* Redirections in cmd.exe: see https://ss64.com/nt/syntax-redirection.html
     and
     https://docs.microsoft.com/en-us/previous-versions/windows/it-pro/windows-xp/bb490982(v=technet.10) *)
  let quote_command cmd ?stdin ?stdout ?stderr args =
    String.concat ""
      [ "\""
      ; quote_cmd_filename cmd
      ; " "
      ; quote_cmd (String.concat " " (List.map quote args))
      ; ( match stdin with
        | None -> ""
        | Some f -> " <" ^ quote_cmd_filename f )
      ; ( match stdout with
        | None -> ""
        | Some f -> " >" ^ quote_cmd_filename f )
      ; ( match stderr with
        | None -> ""
        | Some f ->
          if stderr = stdout then
            " 2>&1"
          else
            " 2>" ^ quote_cmd_filename f )
      ; "\""
      ]
end

module Cygwin : SYSDEPS = struct
  let null = "/dev/null"

  let quote_command = Unix.quote_command
end

module Sysdeps =
( val match Sys.os_type with
      | "Win32" -> (module Win32 : SYSDEPS)
      | "Cygwin" -> (module Cygwin : SYSDEPS)
      | _ -> (module Unix : SYSDEPS) )

include Sysdeps
