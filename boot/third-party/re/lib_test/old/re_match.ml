(*
 * $Id: re_match.ml,v 1.2 2001/10/03 15:04:59 vouillon Exp $
 * http://www.bagley.org/~doug/shootout/
 * from: Markus Mottl
 *)

let rex =
  let three_digits = Re.seq [Re.digit; Re.digit; Re.digit] in
  Re.compile
    (Re.seq
       [(* Must be preceeded by a non-digit *)
        Re.alt [Re.bol; Re.compl [Re.digit; Re.char '(']];
        (* Area code *)
        Re.group (Re.alt [Re.seq [Re.char '('; three_digits; Re.char ')'];
                          three_digits]);
        (* One space *)
        Re.char ' ';
        (* Prefix of 3 digits *)
        three_digits;
        (* Separator: either a space or a dash *)
        Re.set " -";
        (* Last for digits *)
        three_digits; Re.digit;
        (* Must be followed by a non-digit (or EOL) *)
        Re.alt [Re.eol; Re.compl [Re.digit]]])

let foreach_line ?(ic = stdin) f =
  try while true do f (input_line ic) done with End_of_file -> ()

let phones =
  let lines = ref [] in
let ic = open_in "Input" in
  foreach_line ~ic (fun line -> lines := line :: !lines);
close_in ic;
  List.rev !lines

let check_phone cnt must_print line =
  try
    let matches = Re.exec rex line in
    let num = "(...) ...-...." in
    let (pos, _) = Re.Group.offset matches 1 in
    let ofs = if line.[pos] = '(' then 1 else 0 in
    let pos = pos + ofs in
    String.blit line pos num 1 3;
    let pos = pos + ofs + 4 in
    String.blit line pos num 6 3;
    String.blit line (pos + 4) num 10 4;
    if must_print then Printf.printf "%d: %s\n" !cnt num;
    incr cnt
  with Not_found -> ()

let n = if Array.length Sys.argv > 1 then int_of_string Sys.argv.(1) else 1;;
for _i = 2 to n do
  List.iter (check_phone (ref 1) false) phones
done;
List.iter (check_phone (ref 1) true) phones
