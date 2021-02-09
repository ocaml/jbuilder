open! Dune_engine
open! Stdune
open Import

module Merlin_conf = struct
  type t = Sexp.t

  let make_error msg = Sexp.(List [ List [ Atom "ERROR"; Atom msg ] ])

  let to_stdout (t : t) =
    Csexp.to_channel stdout t;
    flush stdout
end

module Commands = struct
  type t =
    | File of string
    | Halt
    | Unknown of string

  let read_input in_channel =
    match Csexp.input in_channel with
    | Ok sexp -> (
      let open Sexp in
      match sexp with
      | Atom "Halt" -> Halt
      | List [ Atom "File"; Atom path ] -> File path
      | sexp ->
        let msg = Printf.sprintf "Bad input: %s" (Sexp.to_string sexp) in
        Unknown msg)
    | Error _ -> Halt
end

(* [to_local p] makes absolute path [p] relative to the project's root and
   optionally removes the build context *)
let to_local abs_file_path =
  let error msg = Error msg in
  let path_opt =
    String.drop_prefix
      ~prefix:Path.(to_absolute_filename (of_local Local.root))
      abs_file_path
  in
  match path_opt with
  | Some path -> (
    try Ok (Filename.concat "." path |> Path.Local.of_string) with
    | User_error.E mess -> User_message.to_string mess |> error)
  | None ->
    Printf.sprintf "Path is not in dune workspace %s" abs_file_path |> error

let get_merlin_files_paths local_path =
  let workspace = Workspace.workspace () in
  let context =
    Option.value ~default:Context_name.default workspace.merlin_context
  in
  let ctx = Context_name.to_string context in
  let ctx_root = Path.Build.(relative root ctx) in
  let dir_path = Path.Build.(append_local ctx_root local_path) in
  let merlin_path =
    Path.Build.relative dir_path Merlin_ident.merlin_folder_name
  in
  let files =
    Result.value ~default:[] (Path.readdir_unsorted (Path.build merlin_path))
    |> List.fast_sort ~cmp:Stdlib.compare
  in
  List.map files ~f:(fun f -> Path.Build.relative merlin_path f |> Path.build)

let load_merlin_file local_path file =
  (* We search for an appropriate merlin configuration in the current directory
     and its parents *)
  let rec find_closest path =
    let filename = String.lowercase_ascii file in
    let file_paths = get_merlin_files_paths path in
    let result =
      List.find_map file_paths ~f:(fun file_path ->
          if Path.exists file_path then
            let open Option.O in
            let* config = Merlin.Processed.load_file file_path in
            Merlin.Processed.get config ~filename
          else
            None)
    in
    match result with
    | Some p -> Some p
    | None ->
      Option.bind ~f:find_closest
        (if Path.Local.is_root path then
          None
        else
          Path.Local.parent path)
  in
  let default =
    Merlin_conf.make_error "Project isn't built. (Try calling `dune build`.)"
  in
  Option.value (find_closest local_path) ~default

let print_merlin_conf file =
  let abs_root, file = Filename.(dirname file, basename file) in
  let answer =
    match to_local abs_root with
    | Ok p -> load_merlin_file p file
    | Error s -> Merlin_conf.make_error s
  in
  Merlin_conf.to_stdout answer

let dump s =
  match to_local s with
  | Ok path ->
    List.iter (get_merlin_files_paths path) ~f:Merlin.Processed.print_file
  | Error mess -> Printf.eprintf "%s\n%!" mess

let start () =
  let rec main () =
    match Commands.read_input stdin with
    | File path ->
      print_merlin_conf path;
      main ()
    | Unknown msg ->
      Merlin_conf.to_stdout (Merlin_conf.make_error msg);
      main ()
    | Halt -> exit 0
  in
  main ()
