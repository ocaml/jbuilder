include Loc0

module O = Comparable.Make (struct
  type nonrec t = t

  let compare = Poly.compare

  let to_dyn = to_dyn
end)

include O

let in_file p =
  let pos = none_pos (Path.to_string p) in
  { start = pos; stop = pos }

let in_dir = in_file

let drop_position (t : t) =
  let pos = none_pos t.start.pos_fname in
  { start = pos; stop = pos }

let of_lexbuf lexbuf : t =
  { start = Lexing.lexeme_start_p lexbuf; stop = Lexing.lexeme_end_p lexbuf }

let equal_position
    { Lexing.pos_fname = f_a; pos_lnum = l_a; pos_bol = b_a; pos_cnum = c_a }
    { Lexing.pos_fname = f_b; pos_lnum = l_b; pos_bol = b_b; pos_cnum = c_b } =
  f_a = f_b && l_a = l_b && b_a = b_b && c_a = c_b

let equal { start = start_a; stop = stop_a } { start = start_b; stop = stop_b }
    =
  equal_position start_a start_b && equal_position stop_a stop_b

let of_pos (fname, lnum, cnum, enum) =
  let pos : Lexing.position =
    { pos_fname = fname; pos_lnum = lnum; pos_cnum = cnum; pos_bol = 0 }
  in
  { start = pos; stop = { pos with pos_cnum = enum } }

let is_none = equal none

let to_file_colon_line t =
  Printf.sprintf "%s:%d" t.start.pos_fname t.start.pos_lnum

let to_dyn_hum t : Dyn.t = String (to_file_colon_line t)

let pp_file_colon_line t = Pp.verbatim (to_file_colon_line t)

let pp_left_pad n s =
  let needed_spaces = n - String.length s in
  Pp.verbatim
    (if needed_spaces > 0 then
      String.make needed_spaces ' ' ^ s
    else
      s)

let pp_line padding_width (lnum, l) =
  let open Pp.O in
  pp_left_pad padding_width lnum
  ++ Pp.verbatim " | " ++ Pp.verbatim l ++ Pp.newline

type tag = Loc

let pp_file_excerpt ~context_lines ~max_lines_to_print_in_full { start; stop } :
    tag Pp.t =
  let start_c = start.pos_cnum - start.pos_bol in
  let stop_c = stop.pos_cnum - start.pos_bol in
  let file = start.pos_fname in
  let pp_file_excerpt () =
    let line_num = start.pos_lnum in
    let line_num_str = string_of_int line_num in
    let padding_width = String.length line_num_str in
    let open Result.O in
    let* line =
      Result.try_with (fun () -> Io.String_path.file_line file line_num)
    in
    if stop_c <= String.length line then
      let len = stop_c - start_c in
      let open Pp.O in
      Ok
        (pp_line padding_width (line_num_str, line)
        ++ pp_left_pad (stop_c + padding_width + 3) (String.make len '^')
        ++ Pp.newline)
    else
      let get_padding lines =
        let lnum, _ = Option.value_exn (List.last lines) in
        String.length lnum
      in
      let print_ellipsis padding_width =
        (* We add 2 to the width of max line to account for the extra space and
           the `|` character at the end of a line number *)
        let line = String.make (padding_width + 2) '.' in
        Pp.verbatim line
      in
      let print_lines lines padding_width =
        Pp.concat_map lines ~f:(pp_line padding_width)
      in
      let file_lines ~start ~stop =
        Result.try_with (fun () -> Io.String_path.file_lines file ~start ~stop)
      in
      let num_lines = stop.pos_lnum - start.pos_lnum in
      if num_lines <= max_lines_to_print_in_full then
        let+ lines = file_lines ~start:start.pos_lnum ~stop:stop.pos_lnum in
        print_lines lines (get_padding lines)
      else
        (* We need to send the padding width from the last four lines so the two
           blocks of lines align if they have different number of digits in
           their line numbers *)
        let* first_shown_lines =
          file_lines ~start:start.pos_lnum ~stop:(start.pos_lnum + context_lines)
        in
        let+ last_shown_lines =
          file_lines ~start:(stop.pos_lnum - context_lines) ~stop:stop.pos_lnum
        in
        let padding_width = get_padding last_shown_lines in
        let open Pp.O in
        print_lines first_shown_lines padding_width
        ++ print_ellipsis padding_width
        ++ print_lines last_shown_lines padding_width
  in
  let whole_file = start_c = 0 && stop_c = 0 in
  if whole_file then
    Pp.nop
  else
    match
      let open Result.O in
      let* exists =
        Result.try_with (fun () -> Sys.file_exists start.pos_fname)
      in
      if exists then
        pp_file_excerpt ()
      else
        Result.Ok Pp.nop
    with
    | Ok pp -> pp
    | Error exn ->
      let backtrace = Printexc.get_backtrace () in
      Format.eprintf "Raised when trying to print location contents of %s@.%a@."
        file
        (Exn.pp_uncaught ~backtrace)
        exn;
      Pp.nop

let pp ({ start; stop } as loc) =
  let start_c = start.pos_cnum - start.pos_bol in
  let stop_c = stop.pos_cnum - start.pos_bol in
  let open Pp.O in
  Pp.tag Loc
    (Pp.verbatim
       (Printf.sprintf "File \"%s\", line %d, characters %d-%d:" start.pos_fname
          start.pos_lnum start_c stop_c))
  ++ Pp.newline
  ++ pp_file_excerpt ~context_lines:2 ~max_lines_to_print_in_full:10 loc

let on_same_line loc1 loc2 =
  let start1 = loc1.start in
  let start2 = loc2.start in
  let same_file = String.equal start1.pos_fname start2.pos_fname in
  let same_line = Int.equal start1.pos_lnum start2.pos_lnum in
  same_file && same_line

let span begin_ end_ = { begin_ with stop = end_.stop }

let rec render ppf pp =
  Pp.to_fmt_with_tags ppf pp ~tag_handler:(fun ppf Loc pp ->
      Format.fprintf ppf "@{<loc>%a@}" render pp)
