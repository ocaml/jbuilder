open! Stdune

type t =
  | Atom of Atom.t
  | Quoted_string of string
  | List of t list
  | Template of Template.t

let atom_or_quoted_string s =
  if Atom.is_valid s then
    Atom (Atom.of_string s)
  else
    Quoted_string s

let atom s = Atom (Atom.of_string s)

let unsafe_atom_of_string s = atom s

let rec to_string t =
  match t with
  | Atom a -> Atom.print a
  | Quoted_string s -> Escape.quoted s
  | List l ->
    Printf.sprintf "(%s)" (List.map l ~f:to_string |> String.concat ~sep:" ")
  | Template t -> Template.to_string t

let rec pp = function
  | Atom s -> Pp.verbatim (Atom.print s)
  | Quoted_string s -> Pp.verbatim (Escape.quoted s)
  | List [] -> Pp.verbatim "()"
  | List l ->
    let open Pp.O in
    Pp.box ~indent:1
      (Pp.char '('
      ++ Pp.hvbox (Pp.concat_map l ~sep:Pp.space ~f:pp)
      ++ Pp.char ')')
  | Template t -> Template.pp t

module Deprecated = struct
  let pp ppf t = Pp.to_fmt ppf (pp t)

  let pp_print_quoted_string ppf s =
    if String.contains s '\n' then (
      match String.split s ~on:'\n' with
      | [] -> Format.pp_print_string ppf (Escape.quoted s)
      | first :: rest ->
        Format.fprintf ppf "@[<hv 1>\"@{<atom>%s" (Escape.escaped first);
        List.iter rest ~f:(fun s ->
            Format.fprintf ppf "@,\\n%s" (Escape.escaped s));
        Format.fprintf ppf "@}\"@]"
    ) else
      Format.pp_print_string ppf (Escape.quoted s)

  let rec pp_split_strings ppf = function
    | Atom s -> Format.pp_print_string ppf (Atom.print s)
    | Quoted_string s -> pp_print_quoted_string ppf s
    | List [] -> Format.pp_print_string ppf "()"
    | List (first :: rest) ->
      Format.pp_open_box ppf 1;
      Format.pp_print_string ppf "(";
      Format.pp_open_hvbox ppf 0;
      pp_split_strings ppf first;
      List.iter rest ~f:(fun sexp ->
          Format.pp_print_space ppf ();
          pp_split_strings ppf sexp);
      Format.pp_close_box ppf ();
      Format.pp_print_string ppf ")";
      Format.pp_close_box ppf ()
    | Template t -> Template.pp_split_strings ppf t

  type formatter_state =
    | In_atom
    | In_makefile_action
    | In_makefile_stuff

  let prepare_formatter ppf =
    let state = ref [] in
    Format.pp_set_mark_tags ppf true;
    let ofuncs = Format.pp_get_formatter_out_functions ppf () in
    let tfuncs =
      (Format.pp_get_formatter_tag_functions ppf () [@warning "-3"])
    in
    Format.pp_set_formatter_tag_functions ppf
      { tfuncs with
        mark_open_tag =
          (function
          | "atom" ->
            state := In_atom :: !state;
            ""
          | "makefile-action" ->
            state := In_makefile_action :: !state;
            ""
          | "makefile-stuff" ->
            state := In_makefile_stuff :: !state;
            ""
          | s -> tfuncs.mark_open_tag s)
      ; mark_close_tag =
          (function
          | "atom"
          | "makefile-action"
          | "makefile-stuff" ->
            state := List.tl !state;
            ""
          | s -> tfuncs.mark_close_tag s)
      } [@warning "-3"];
    Format.pp_set_formatter_out_functions ppf
      { ofuncs with
        out_newline =
          (fun () ->
            match !state with
            | [ In_atom; In_makefile_action ] -> ofuncs.out_string "\\\n\t" 0 3
            | [ In_atom ] -> ofuncs.out_string "\\\n" 0 2
            | [ In_makefile_action ] -> ofuncs.out_string " \\\n\t" 0 4
            | [ In_makefile_stuff ] -> ofuncs.out_string " \\\n" 0 3
            | [] -> ofuncs.out_string "\n" 0 1
            | _ -> assert false)
      ; out_spaces =
          (fun n ->
            ofuncs.out_spaces
              (match !state with
              | In_atom :: _ -> max 0 (n - 2)
              | _ -> n))
      }
end

let rec to_dyn =
  let open Dyn.Encoder in
  function
  | Atom (A a) -> string a
  | List s -> List (List.map s ~f:to_dyn)
  | Quoted_string s -> string s
  | Template t -> constr "template" [ string (Template.to_string t) ]
