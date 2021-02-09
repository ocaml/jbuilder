module Style = struct
  type t = string

  let fg_black = "30"

  let fg_red = "31"

  let fg_green = "32"

  let fg_yellow = "33"

  let fg_blue = "34"

  let fg_magenta = "35"

  let fg_cyan = "36"

  let fg_white = "37"

  let fg_default = "39"

  let fg_bright_black = "90"

  let fg_bright_red = "91"

  let fg_bright_green = "92"

  let fg_bright_yellow = "93"

  let fg_bright_blue = "94"

  let fg_bright_magenta = "95"

  let fg_bright_cyan = "96"

  let fg_bright_white = "97"

  let bg_black = "40"

  let bg_red = "41"

  let bg_green = "42"

  let bg_yellow = "43"

  let bg_blue = "44"

  let bg_magenta = "45"

  let bg_cyan = "46"

  let bg_white = "47"

  let bg_default = "49"

  let bg_bright_black = "100"

  let bg_bright_red = "101"

  let bg_bright_green = "102"

  let bg_bright_yellow = "103"

  let bg_bright_blue = "104"

  let bg_bright_magenta = "105"

  let bg_bright_cyan = "106"

  let bg_bright_white = "107"

  let bold = "1"

  let dim = "2"

  let underlined = "4"

  let escape_sequence l =
    let l = "0" :: l in
    Printf.sprintf "\027[%sm" (String.concat l ~sep:";")

  let escape_sequence_no_reset l =
    Printf.sprintf "\027[%sm" (String.concat l ~sep:";")
end

let term_supports_color =
  lazy
    (match Stdlib.Sys.getenv "TERM" with
    | exception Not_found -> false
    | "dumb" -> false
    | _ -> true)

let stdout_supports_color =
  lazy (Lazy.force term_supports_color && Unix.isatty Unix.stdout)

let stderr_supports_color =
  lazy (Lazy.force term_supports_color && Unix.isatty Unix.stderr)

let rec tag_handler current_styles ppf styles pp =
  Format.pp_print_as ppf 0 (Style.escape_sequence_no_reset styles);
  Pp.to_fmt_with_tags ppf pp
    ~tag_handler:(tag_handler (current_styles @ styles));
  Format.pp_print_as ppf 0 (Style.escape_sequence current_styles)

let make_printer supports_color ppf =
  let f =
    lazy
      (if Lazy.force supports_color then
        Pp.to_fmt_with_tags ppf ~tag_handler:(tag_handler [])
      else
        Pp.to_fmt ppf)
  in
  Staged.stage (fun pp ->
      Lazy.force f pp;
      Format.pp_print_flush ppf ())

let print =
  Staged.unstage (make_printer stdout_supports_color Format.std_formatter)

let prerr =
  Staged.unstage (make_printer stderr_supports_color Format.err_formatter)

let strip str =
  let len = String.length str in
  let buf = Buffer.create len in
  let rec loop i =
    if i = len then
      Buffer.contents buf
    else
      match str.[i] with
      | '\027' -> skip (i + 1)
      | c ->
        Buffer.add_char buf c;
        loop (i + 1)
  and skip i =
    if i = len then
      Buffer.contents buf
    else
      match str.[i] with
      | 'm' -> loop (i + 1)
      | _ -> skip (i + 1)
  in
  loop 0

let parse_line str styles =
  let len = String.length str in
  let add_chunk acc ~styles ~pos ~len =
    if len = 0 then
      acc
    else
      let s = Pp.verbatim (String.sub str ~pos ~len) in
      let s =
        match styles with
        | [] -> s
        | _ -> Pp.tag styles s
      in
      Pp.seq acc s
  in
  let rec loop styles i acc =
    match String.index_from str i '\027' with
    | None -> (styles, add_chunk acc ~styles ~pos:i ~len:(len - i))
    | Some seq_start -> (
      let acc = add_chunk acc ~styles ~pos:i ~len:(seq_start - i) in
      (* Skip the "\027[" *)
      let seq_start = seq_start + 2 in
      if seq_start >= len || str.[seq_start - 1] <> '[' then
        (styles, acc)
      else
        match String.index_from str seq_start 'm' with
        | None -> (styles, acc)
        | Some seq_end ->
          let styles =
            if seq_start = seq_end then
              (* Some commands output "\027[m", which seems to be interpreted
                 the same as "\027[0m" by terminals *)
              []
            else
              String.sub str ~pos:seq_start ~len:(seq_end - seq_start)
              |> String.split ~on:';'
              |> List.fold_left ~init:(List.rev styles) ~f:(fun styles s ->
                     match s with
                     | "0" -> []
                     | _ -> s :: styles)
              |> List.rev
          in
          loop styles (seq_end + 1) acc)
  in
  loop styles 0 Pp.nop

let parse =
  let rec loop styles lines acc =
    match lines with
    | [] -> Pp.vbox (Pp.concat ~sep:Pp.cut (List.rev acc))
    | line :: lines ->
      let styles, pp = parse_line line styles in
      loop styles lines (pp :: acc)
  in
  fun str -> loop [] (String.split_lines str) []
