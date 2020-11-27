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

let rec get_plugin directory plugins requires entries =
  match entries with
  | [] -> (directory, List.rev plugins, List.rev requires)
  | Meta_parser.Comment _ :: entries ->
    get_plugin directory plugins requires entries
  | Package _ :: entries -> get_plugin directory plugins requires entries
  | Rule { var = "directory"; predicates = []; action = Set; value } :: entries
    ->
    get_plugin (Some value) plugins requires entries
  | Rule { var = "plugin"; predicates; action = Set; value } :: entries
    when check_predicates predicates ->
    get_plugin directory [ value ] requires entries
  | Rule { var = "plugin"; predicates; action = Add; value } :: entries
    when check_predicates predicates ->
    get_plugin directory (value :: plugins) requires entries
  | Rule { var = "requires"; predicates; action = Set; value } :: entries
    when check_predicates predicates ->
    get_plugin directory plugins [ value ] entries
  | Rule { var = "requires"; predicates; action = Add; value } :: entries
    when check_predicates predicates ->
    get_plugin directory plugins (value :: requires) entries
  | Rule _ :: entries -> get_plugin directory plugins requires entries

exception Library_not_found of string

let rec find_library ~rest meta =
  match rest with
  | [] -> meta
  | pkg :: rest ->
    let rec aux pkg = function
      | [] -> raise (Library_not_found pkg)
      | Meta_parser.Package { name = Some name; entries } :: _
        when String.equal name pkg ->
        find_library ~rest entries
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

let find_plugin ~dir ~rest meta =
  let directory, plugins, requires =
    get_plugin None [] [] (find_library ~rest meta.Meta_parser.entries)
  in
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
    with exn ->
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
  let pkg, rest = split name in
  let rec loop dirs =
    match dirs with
    | [] -> (
      List.assoc_opt pkg Data.builtin_library |> function
      | None -> raise (Library_not_found name)
      | Some meta -> find_plugin ~dir:(Lazy.force Helpers.stdlib) ~rest meta )
    | dir :: dirs -> (
      let dir = Filename.concat dir pkg in
      match lookup_and_load_one_dir ~dir ~pkg with
      | None -> loop dirs
      | Some p -> find_plugin ~dir ~rest p )
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

let load_plugin plugin_paths name =
  let _, plugins, requires = lookup_and_summarize plugin_paths name in
  assert (plugins = []);
  List.iter load_requires requires

module Make (X : sig
  val paths : string list
end) : S = struct
  include X

  let list () = readdir paths

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
  with _ -> (* CR - What exceptions are being swallowed here? *)
            false

let prng = lazy (Random.State.make_self_init ())

let temp_file_name temp_dir prefix suffix =
  let rnd = Random.State.bits (Lazy.force prng) land 0xFFFFFF in
  Filename.concat temp_dir (Printf.sprintf "%s%06x%s" prefix rnd suffix)

let mk_temp_dir ?(temp_dir = Filename.get_temp_dir_name ()) prefix suffix =
  let rec try_name counter =
    let name = temp_file_name temp_dir prefix suffix in
    try
      let cmd =
        Filename_transitioning.quote_command "mkdir" [ name ]
          ~stdin:Filename_transitioning.null ~stdout:Filename_transitioning.null
          ~stderr:Filename_transitioning.null
      in
      let r = Sys.command cmd in
      if r <> 0 then
        if counter >= 1000 then
          invalid_arg
            (Printf.sprintf "Can't create temporary directory in %s" temp_dir)
        else
          try_name (counter + 1)
      else
        name
    with Sys_error _ as e ->
      if counter >= 1000 then
        raise e
      else
        try_name (counter + 1)
  in
  try_name 0

let rm_rf dir =
  let cmd =
    match Sys.os_type with
    | "Win32" -> Filename_transitioning.quote_command "rd" [ "/s"; "/q"; dir ]
    | _ -> Filename_transitioning.quote_command "rm" [ "-rf"; dir ]
  in
  ignore (Sys.command cmd)

(* from Io *)
let input_lines =
  let rec loop ic acc =
    match input_line ic with
    | exception End_of_file -> List.rev acc
    | line -> loop ic (line :: acc)
  in
  fun ic -> loop ic []

let readfile file =
  let cin = open_in file in
  let l = input_lines cin in
  close_in cin;
  l

let ocaml_compiler =
  lazy
    (let get_env name = function
       | Some s -> s
       | None -> (
         try Sys.getenv name
         with Not_found ->
           invalid_arg "No ocaml compiler path provided by dune" )
     in
     match Sys.backend_type with
     | Native -> get_env "DUNE_OCAMLOPT" Dune_site_plugins_data.ocamlopt
     | Bytecode -> get_env "DUNE_OCAMLC" Dune_site_plugins_data.ocamlc
     | Other _ -> invalid_arg "load_script: unknown backend_type")

let load_script ?(open_ = []) ?warnings filename =
  let directories =
    Hashtbl.fold
      (fun name () acc ->
        let directory, _, _ =
          lookup_and_summarize (Lazy.force Helpers.ocamlpath) name
        in
        directory :: acc)
      (Lazy.force loaded_libraries)
      []
  in
  let tempdir = mk_temp_dir "dune_load_script" "" in
  try
    let options_filename = Filename.concat tempdir "options.txt" in
    let cout_options = open_out options_filename in
    let shared_file =
      Dynlink.adapt_filename (Filename.concat tempdir "script.cma")
    in
    List.iter (Printf.fprintf cout_options "-open\000%s\000") open_;
    List.iter (Printf.fprintf cout_options "-I\000%s\000") directories;
    ( match warnings with
    | None -> ()
    | Some warnings -> Printf.fprintf cout_options "-w\000%s\000" warnings );
    Printf.fprintf cout_options "-shared\000-o\000%s\000%s" shared_file filename;
    close_out cout_options;
    let ocaml_compiler =
      if Lazy.force Dune_site.Private_.Helpers.relocatable then
        let compiler_name =
          let exe =
            match Sys.os_type with
            | "Win32" -> ".exe"
            | _ -> ""
          in
          match Sys.backend_type with
          | Native -> "ocamlopt" ^ exe
          | Bytecode -> "ocamlc" ^ exe
          | Other _ -> invalid_arg "load_script: unknown backend_type"
        in
        let p = Lazy.force Dune_site.Private_.Helpers.relocatable_prefix in
        let p = Filename.concat p "bin" in
        let p = Filename.concat p compiler_name in
        if Sys.file_exists p then
          p
        else
          compiler_name
      else
        Lazy.force ocaml_compiler
    in
    let stdout = Filename.concat tempdir "stdout.log" in
    let stderr = Filename.concat tempdir "stderr.log" in
    let cmd =
      Filename_transitioning.quote_command ~stdin:Filename_transitioning.null
        ~stdout ~stderr ocaml_compiler
        [ "-args0"; options_filename ]
    in
    let r = Sys.command cmd in
    let stdout = readfile stdout in
    let stderr = readfile stderr in
    if r <> 0 then (
      rm_rf tempdir;
      (`Compilation_failed, stdout, stderr)
    ) else (
      Dynlink.loadfile shared_file;
      rm_rf tempdir;
      (`Ok, stdout, stderr)
    )
  with e ->
    rm_rf tempdir;
    raise e
