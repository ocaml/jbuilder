module List = struct
  include ListLabels

  let map ~f t = rev (rev_map ~f t)
end

module String = StringLabels

type +'a t =
  | Nop
  | Seq of 'a t * 'a t
  | Concat of 'a t * 'a t list
  | Box of int * 'a t
  | Vbox of int * 'a t
  | Hbox of 'a t
  | Hvbox of int * 'a t
  | Hovbox of int * 'a t
  | Verbatim of string
  | Char of char
  | Break of (string * int * string) * (string * int * string)
  | Newline
  | Text of string
  | Tag of 'a * 'a t

let rec map_tags t ~f =
  match t with
  | Nop -> Nop
  | Seq (a, b) -> Seq (map_tags a ~f, map_tags b ~f)
  | Concat (sep, l) -> Concat (map_tags sep ~f, List.map l ~f:(map_tags ~f))
  | Box (indent, t) -> Box (indent, map_tags t ~f)
  | Vbox (indent, t) -> Vbox (indent, map_tags t ~f)
  | Hbox t -> Hbox (map_tags t ~f)
  | Hvbox (indent, t) -> Hvbox (indent, map_tags t ~f)
  | Hovbox (indent, t) -> Hovbox (indent, map_tags t ~f)
  | (Verbatim _ | Char _ | Break _ | Newline | Text _) as t -> t
  | Tag (tag, t) -> Tag (f tag, map_tags t ~f)

let rec filter_map_tags t ~f =
  match t with
  | Nop -> Nop
  | Seq (a, b) -> Seq (filter_map_tags a ~f, filter_map_tags b ~f)
  | Concat (sep, l) ->
    Concat (filter_map_tags sep ~f, List.map l ~f:(filter_map_tags ~f))
  | Box (indent, t) -> Box (indent, filter_map_tags t ~f)
  | Vbox (indent, t) -> Vbox (indent, filter_map_tags t ~f)
  | Hbox t -> Hbox (filter_map_tags t ~f)
  | Hvbox (indent, t) -> Hvbox (indent, filter_map_tags t ~f)
  | Hovbox (indent, t) -> Hovbox (indent, filter_map_tags t ~f)
  | (Verbatim _ | Char _ | Break _ | Newline | Text _) as t -> t
  | Tag (tag, t) -> (
    let t = filter_map_tags t ~f in
    match f tag with
    | None -> t
    | Some tag -> Tag (tag, t) )

module Render = struct
  open Format

  let rec render ppf t ~tag_handler =
    match t with
    | Nop -> ()
    | Seq (a, b) ->
      render ppf ~tag_handler a;
      render ppf ~tag_handler b
    | Concat (_, []) -> ()
    | Concat (sep, x :: l) ->
      render ppf ~tag_handler x;
      List.iter l ~f:(fun x ->
          render ppf ~tag_handler sep;
          render ppf ~tag_handler x)
    | Box (indent, t) ->
      pp_open_box ppf indent;
      render ppf ~tag_handler t;
      pp_close_box ppf ()
    | Vbox (indent, t) ->
      pp_open_vbox ppf indent;
      render ppf ~tag_handler t;
      pp_close_box ppf ()
    | Hbox t ->
      pp_open_hbox ppf ();
      render ppf ~tag_handler t;
      pp_close_box ppf ()
    | Hvbox (indent, t) ->
      pp_open_hvbox ppf indent;
      render ppf ~tag_handler t;
      pp_close_box ppf ()
    | Hovbox (indent, t) ->
      pp_open_hovbox ppf indent;
      render ppf ~tag_handler t;
      pp_close_box ppf ()
    | Verbatim x -> pp_print_string ppf x
    | Char x -> pp_print_char ppf x
    | Break (fits, breaks) -> pp_print_custom_break ppf ~fits ~breaks
    | Newline -> pp_force_newline ppf ()
    | Text s -> pp_print_text ppf s
    | Tag (tag, t) -> tag_handler ppf tag t
end

let to_fmt_with_tags = Render.render

let rec to_fmt ppf t =
  Render.render ppf t ~tag_handler:(fun ppf _tag t -> to_fmt ppf t)

let nop = Nop

let seq a b = Seq (a, b)

let concat ?(sep = Nop) = function
  | [] -> Nop
  | [ x ] -> x
  | l -> Concat (sep, l)

let concat_map ?(sep = Nop) l ~f =
  match l with
  | [] -> Nop
  | [ x ] -> f x
  | l -> Concat (sep, List.map l ~f)

let concat_mapi ?(sep = Nop) l ~f =
  match l with
  | [] -> Nop
  | [ x ] -> f 0 x
  | l -> Concat (sep, List.mapi l ~f)

let box ?(indent = 0) t = Box (indent, t)

let vbox ?(indent = 0) t = Vbox (indent, t)

let hbox t = Hbox t

let hvbox ?(indent = 0) t = Hvbox (indent, t)

let hovbox ?(indent = 0) t = Hovbox (indent, t)

let verbatim x = Verbatim x

let char x = Char x

let custom_break ~fits ~breaks = Break (fits, breaks)

let break ~nspaces ~shift =
  custom_break ~fits:("", nspaces, "") ~breaks:("", shift, "")

let space = break ~nspaces:1 ~shift:0

let cut = break ~nspaces:0 ~shift:0

let newline = Newline

let text s = Text s

let textf fmt = Printf.ksprintf text fmt

let tag tag t = Tag (tag, t)

let enumerate l ~f =
  vbox
    (concat ~sep:cut
       (List.map l ~f:(fun x -> box ~indent:2 (seq (verbatim "- ") (f x)))))

let chain l ~f =
  vbox
    (concat ~sep:cut
       (List.mapi l ~f:(fun i x ->
            box ~indent:3
              (seq
                 (verbatim
                    ( if i = 0 then
                      "   "
                    else
                      "-> " ))
                 (f x)))))

module O = struct
  let ( ++ ) = seq
end
