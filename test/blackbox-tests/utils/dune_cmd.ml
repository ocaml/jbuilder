open Stdune
module Re = Dune_re

let commands = Table.create (module String) 10

let register name of_args run =
  Table.add_exn commands name (fun args ->
      let t = of_args args in
      run t)

module Stat = struct
  type data =
    | Hardlinks
    | Permissions
    | Size

  type t =
    { file : Path.t
    ; data : data
    }

  let data_of_string = function
    | "size" -> Size
    | "hardlinks" -> Hardlinks
    | "permissions" -> Permissions
    | s ->
      raise
        (Arg.Bad
           (sprintf
              "%s is invalid. hardlinks, permissions are only valid options" s))

  let pp_stats data (stats : Unix.stats) =
    match data with
    | Size -> Int.to_string stats.st_size
    | Hardlinks -> Int.to_string stats.st_nlink
    | Permissions -> sprintf "%o" stats.st_perm

  let name = "stat"

  let of_args = function
    | [ data; file ] ->
      let data = data_of_string data in
      let file = Path.of_filename_relative_to_initial_cwd file in
      { file; data }
    | _ -> raise (Arg.Bad (sprintf "2 arguments must be provided"))

  let run { file; data } =
    let stats = Path.stat file in
    print_endline (pp_stats data stats)

  let () = register name of_args run
end

module Wait_for_fs_clock_to_advance = struct
  let name = "wait-for-fs-clock-to-advance"

  let of_args = function
    | [] -> ()
    | _ -> raise (Arg.Bad ("Usage: dune_cmd " ^ name))

  let run () =
    let fn = "." ^ name ^ ".tmp" in
    let fstime () =
      Unix.close (Unix.openfile fn [ O_WRONLY; O_CREAT; O_TRUNC ] 0o644);
      let t = (Unix.stat fn).st_ctime in
      Unix.unlink fn;
      t
    in
    let t = fstime () in
    while fstime () <= t do
      Unix.sleepf 0.01
    done

  let () = register name of_args run
end

module Cat = struct
  type t = File of Path.t

  let name = "cat"

  let of_args = function
    | [ file ] -> File (Path.of_filename_relative_to_initial_cwd file)
    | _ -> raise (Arg.Bad "Usage: dune_arg cat <file>")

  let run (File p) = print_string (Io.read_file p)

  let () = register name of_args run
end

module Expand_lines = struct
  let name = "expand_lines"

  let of_args = function
    | [] -> ()
    | _ -> raise (Arg.Bad ("Usage: dune_arg " ^ name))

  let run () =
    let re = Re.compile (Re.str "\\n") in
    set_binary_mode_in stdin true;
    set_binary_mode_out stdout true;
    let rec loop () =
      match input_line stdin with
      | exception End_of_file -> ()
      | s ->
        print_endline (Re.replace_string ~all:true re s ~by:"\n");
        loop ()
    in
    loop ()

  let () = register name of_args run
end

module Sanitizer = struct
  module Configurator = Configurator.V1

  let make_ext_replace config =
    let tbl =
      List.filter_map [ "ext_exe"; "ext_dll"; "ext_asm"; "ext_lib"; "ext_obj" ]
        ~f:(fun var ->
          match Configurator.ocaml_config_var config var with
          | Some "" -> None
          | Some s -> Some (s, "$" ^ var)
          | None -> (
            match (var, Configurator.ocaml_config_var config "system") with
            | "ext_exe", Some "Win32" -> Some (".exe", var)
            | _ -> None))
    in
    let re =
      Re.(
        compile
          (seq
             [ diff any (char '/')
             ; alt (List.map tbl ~f:(fun (s, _) -> str s))
             ; eow
             ]))
    in
    let map = String.Map.of_list_reduce tbl ~f:(fun _ x -> x) in
    fun s ->
      Re.replace re s ~f:(fun g ->
          let s = Re.Group.get g 0 in
          sprintf "%c%s" s.[0] (String.Map.find_exn map (String.drop s 1)))

  let name = "sanitize"

  let of_args = function
    | [] -> ()
    | _ -> raise (Arg.Bad "Usage: dune_cmd sanitize takes no arguments")

  let run () =
    let config = Configurator.create "sanitizer" in
    let sanitize = make_ext_replace config in
    let rec loop () =
      match input_line stdin with
      | exception End_of_file -> ()
      | line ->
        print_endline (sanitize line);
        loop ()
    in
    loop ()

  let () = register name of_args run
end

module Count_lines = struct
  type t =
    | Stdin
    | File of Path.t

  let name = "count-lines"

  let count_lines ic =
    let rec loop n =
      match input_line ic with
      | exception End_of_file -> n
      | _line -> loop (n + 1)
    in
    loop 0

  let of_args = function
    | [] -> Stdin
    | [ file ] -> File (Path.of_filename_relative_to_initial_cwd file)
    | _ -> raise (Arg.Bad "Usage: dune_arg count-lines <file>")

  let run t =
    let n =
      match t with
      | Stdin -> count_lines stdin
      | File p -> Io.with_file_in p ~binary:false ~f:count_lines
    in
    Printf.printf "%d\n%!" n

  let () = register name of_args run
end

let () =
  let name, args =
    match Array.to_list Sys.argv with
    | _ :: name :: args -> (name, args)
    | [] -> assert false
    | [ _ ] ->
      Format.eprintf "No arguments passed@.%!";
      exit 1
  in
  match Table.find commands name with
  | None ->
    Format.eprintf "No command %S name found" name;
    exit 1
  | Some run -> run args
