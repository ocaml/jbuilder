(*
 * $Id: pcre_match.ml,v 1.1.1.1 2001/09/21 15:54:12 vouillon Exp $
 * http://www.bagley.org/~doug/shootout/
 * from: Markus Mottl
 *)

open Re_pcre

let rex =
  regexp ~flags:[`EXTENDED]
    "(?: ^ | [^\d\(])     # must be preceeded by non-digit
     (\(\d\d\d\)|\d\d\d)  # match 1: area code
     [ ]                  # area code followed by one space
     \d\d\d               # prefix of 3 digits
     [ -]                 # separator is either space or dash
     \d\d\d\d             # last 4 digits
     (?: \D|$)            # must be followed by a non-digit (or EOL)"

let phones =
  let lines = ref [] in
  foreach_line (fun line -> lines := line :: !lines);
  List.rev !lines

let check_phone irflags ar cnt must_print line =
  try
    unsafe_pcre_exec irflags rex 0 line 4 ar;
    let num = String.copy "(...) ...-...." and pos = Array.unsafe_get ar 2 in
    let ofs = if String.unsafe_get line pos = '(' then 1 else 0 in
    let pos = pos + ofs in
    String.unsafe_blit line pos num 1 3;
    let pos = pos + ofs + 4 in
    String.unsafe_blit line pos num 6 3;
    String.unsafe_blit line (pos + 4) num 10 4;
    if must_print then Printf.printf "%d: %s\n" !cnt num;
    incr cnt
  with Not_found -> ()

let n = if Array.length Sys.argv > 1 then int_of_string Sys.argv.(1) else 1;;
for i = 2 to n do
  List.iter (check_phone (rflags []) (Array.create 6 0) (ref 1) false) phones
done;
List.iter (check_phone (rflags []) (Array.create 6 0) (ref 1) true) phones
