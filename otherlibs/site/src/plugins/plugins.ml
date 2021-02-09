open Dune_site.Private_
module Data = Dune_site_plugins_data

let readdir dirs =
  List.concat
    (List.map
       (fun dir -> Array.to_list (Sys.readdir dir))
       (List.filter Sys.file_exists dirs))

module type S = sig
  val paths : string list

  val list : unit -> string list

  val load_all : unit -> unit

  val load : string -> unit
end

let rec check_predicates predicates =
  match (Sys.backend_type, predicates) with
  | _, [] -> true
  | Sys.Native, Meta_parser.Pos "byte" :: _ -> false
  | Sys.Bytecode, Meta_parser.Pos "native" :: _ -> false
  | Sys.Native, Meta_parser.Pos "native" :: predicates ->
    check_predicates predicates
  | Sys.Bytecode, Meta_parser.Pos "byte" :: predicates ->
    check_predicates predicates
  | Sys.Native, Meta_parser.Neg "native" :: _ -> false
  | Sys.Bytecode, Meta_parser.Neg "byte" :: _ -> false
  | Sys.Native, Meta_parser.Neg "byte" :: predicates ->
    check_predicates predicates
  | Sys.Bytecode, Meta_parser.Neg "native" :: predicates ->
    check_predicates predicates
  | _, Meta_parser.Pos pred :: predicates ->
    Data.findlib_predicates_set_by_dune pred && check_predicates predicates
  | _, Meta_parser.Neg pred :: predicates ->
    (not (Data.findlib_predicates_set_by_dune pred))
    && check_predicates predicates

let rec get_plugin plugins requires entries =
  match entries with
  | [] -> (List.rev plugins, List.rev requires)
  | Meta_parser.Comment _ :: entries -> get_plugin plugins requires entries
  | Package _ :: entries -> get_plugin plugins requires entries
  | Rule { var = "plugin"; predicates; action = Set; value } :: entries
    when check_predicates predicates ->
    get_plugin [ value ] requires entries
  | Rule { var = "plugin"; predicates; action = Add; value } :: entries
    when check_predicates predicates ->
    get_plugin (value :: plugins) requires entries
  | Rule { var = "requires"; predicates; action = Set; value } :: entries
    when check_predicates predicates ->
    get_plugin plugins [ value ] entries
  | Rule { var = "requires"; predicates; action = Add; value } :: entries
    when check_predicates predicates ->
    get_plugin plugins (value :: requires) entries
  | Rule _ :: entries -> get_plugin plugins requires entries

exception Library_not_found of string

let rec find_library ~suffix directory meta =
  let rec find_directory directory = function
    | [] -> directory
    | Meta_parser.Rule
        { var = "directory"; predicates = []; action = Set; value }
      :: _ -> (
      match directory with
      | None -> Some value
      | Some old -> Some (Filename.concat old value))
    | _ :: entries -> find_directory directory entries
  in
  match suffix with
  | [] -> (find_directory directory meta, meta)
  | pkg :: suffix ->
    let directory = find_directory directory meta in
    let rec aux pkg = function
      | [] -> raise (Library_not_found pkg)
      | Meta_parser.Package { name = Some name; entries } :: _
        when String.equal name pkg ->
        find_library ~suffix directory entries
      | _ :: entries -> aux pkg entries
    in
    aux pkg meta

let extract_words s ~is_word_char =
  let rec skip_blanks i =
    if i = String.length s then
      []
    else if is_word_char s.[i] then
      parse_word i (i + 1)
    else
      skip_blanks (i + 1)
  and parse_word i j =
    if j = String.length s then
      [ StringLabels.sub s ~pos:i ~len:(j - i) ]
    else if is_word_char s.[j] then
      parse_word i (j + 1)
    else
      StringLabels.sub s ~pos:i ~len:(j - i) :: skip_blanks (j + 1)
  in
  skip_blanks 0

let extract_comma_space_separated_words s =
  extract_words s ~is_word_char:(function
    | ','
    | ' '
    | '\t'
    | '\n' ->
      false
    | _ -> true)

let split_all l = List.concat (List.map extract_comma_space_separated_words l)

let find_plugin ~dir ~suffix meta =
  let directory, meta = find_library ~suffix None meta.Meta_parser.entries in
  let plugins, requires = get_plugin [] [] meta in
  let directory =
    match directory with
    | None -> dir
    | Some pkg_dir ->
      if pkg_dir.[0] = '+' || pkg_dir.[0] = '^' then
        Filename.concat
          (Lazy.force Helpers.stdlib)
          (String.sub pkg_dir 1 (String.length pkg_dir - 1))
      else if Filename.is_relative pkg_dir then
        Filename.concat dir pkg_dir
      else
        pkg_dir
  in
  let plugins = split_all plugins in
  let requires = split_all requires in
  (directory, plugins, requires)

let load file ~pkg =
  let entries =
    let ic = open_in file in
    try
      let lb = Lexing.from_channel ic in
      lb.lex_curr_p <-
        { pos_fname = file; pos_lnum = 1; pos_bol = 0; pos_cnum = 0 };
      let r = Meta_parser.Parse.entries lb 0 [] in
      close_in ic;
      r
    with
    | exn ->
      close_in ic;
      raise exn
  in
  { Meta_parser.name = Some pkg; entries }

let meta_fn = "META"

let lookup_and_load_one_dir ~dir ~pkg =
  let meta_file = Filename.concat dir meta_fn in
  if Sys.file_exists meta_file then
    Some (load meta_file ~pkg)
  else
    (* Alternative layout *)
    let dir = Filename.dirname dir in
    let meta_file = Filename.concat dir (meta_fn ^ "." ^ pkg) in
    if Sys.file_exists meta_file then
      Some (load meta_file ~pkg)
    else
      None

let split name =
  match String.split_on_char '.' name with
  | [] -> raise (Library_not_found name)
  | pkg :: rest -> (pkg, rest)

let lookup_and_summarize dirs name =
  let pkg, suffix = split name in
  let rec loop dirs =
    match dirs with
    | [] -> (
      List.assoc_opt pkg Data.builtin_library |> function
      | None -> raise (Library_not_found name)
      | Some meta -> find_plugin ~dir:(Lazy.force Helpers.stdlib) ~suffix meta)
    | dir :: dirs -> (
      let dir = Filename.concat dir pkg in
      match lookup_and_load_one_dir ~dir ~pkg with
      | None -> loop dirs
      | Some p -> find_plugin ~dir ~suffix p)
  in
  loop dirs

let loaded_libraries =
  lazy
    (let h = Hashtbl.create 10 in
     List.iter (fun s -> Hashtbl.add h s ()) Data.already_linked_libraries;
     h)

let load_gen ~load_requires dirs name =
  let loaded_libraries = Lazy.force loaded_libraries in
  if not (Hashtbl.mem loaded_libraries name) then (
    Hashtbl.add loaded_libraries name ();
    let directory, plugins, requires = lookup_and_summarize dirs name in
    List.iter load_requires requires;
    List.iter
      (fun p ->
        let file = Filename.concat directory p in
        Dynlink.loadfile file)
      plugins
  )

let rec load_requires name =
  load_gen ~load_requires (Lazy.force Helpers.ocamlpath) name

let load_plugin plugin_paths name = load_gen ~load_requires plugin_paths name

module Make (X : sig
  val paths : string list
end) : S = struct
  include X

  let list () = List.sort String.compare (readdir paths)

  let load name = load_plugin paths name

  let load_all () = List.iter load (list ())
end

let load = load_requires

let available name =
  Hashtbl.mem (Lazy.force loaded_libraries) name
  ||
  let ocamlpath = Lazy.force Helpers.ocamlpath in
  try
    ignore (lookup_and_summarize ocamlpath name);
    true
  with
  | _ ->
    (* CR - What exceptions are being swallowed here? *)
    false
