#!/usr/bin/env ocaml

(* Usage: ocaml build.ml [cma|cmxa|cmxs|clean] *)

let root_dir = Sys.getcwd ()
let build_dir = "_build"
let src_dir = "src"

let base_ocaml_opts =
  [ "-g"; "-bin-annot";
    "-safe-string"; (* Remove once we require >= 4.06 *) ]

(* Logging *)

let strf = Printf.sprintf
let err fmt = Printf.kfprintf (fun oc -> flush oc; exit 1) stderr fmt
let log fmt = Printf.kfprintf (fun oc -> flush oc) stdout fmt

(* The running joke *)

let rev_cut ~sep s = match String.rindex s sep with
| exception Not_found -> None
| i -> String.(Some (sub s 0 i, sub s (i + 1) (length s - (i + 1))))

let cuts ~sep s =
  let rec loop acc = function
  | "" -> acc
  | s ->
      match rev_cut ~sep s with
      | None -> s :: acc
      | Some (l, r) -> loop (r :: acc) l
  in
  loop [] s

(* Read, write and collect files *)

let fpath ~dir f = String.concat "" [dir; "/"; f]

let string_of_file f =
  let ic = open_in_bin f in
  let len = in_channel_length ic in
  let buf = Bytes.create len in
  really_input ic buf 0 len;
  close_in ic;
  Bytes.unsafe_to_string buf

let string_to_file f s =
  let oc = open_out_bin f in
  output_string oc s;
  close_out oc

let cp src dst = string_to_file dst (string_of_file src)

let ml_srcs dir =
  let add_file dir acc f = match rev_cut ~sep:'.' f with
  | Some (m, e) when e = "ml" || e = "mli" -> f :: acc
  | Some _ | None -> acc
  in
  Array.fold_left (add_file dir) [] (Sys.readdir dir)

(* Finding and running commands *)

let find_cmd cmds =
  let test, null = match Sys.win32 with
  | true -> "where", " NUL"
  | false -> "type", "/dev/null"
  in
  let cmd c = Sys.command (strf "%s %s 1>%s 2>%s" test c null null) = 0 in
  try Some (List.find cmd cmds) with Not_found -> None

let err_cmd exit cmd = err "exited with %d: %s\n" exit cmd
let quote_cmd = match Sys.win32 with
| false -> fun cmd -> cmd
| true -> fun cmd -> strf "\"%s\"" cmd

let run_cmd args =
  let cmd = String.concat " " (List.map Filename.quote args) in
(*  log "[EXEC] %s\n" cmd; *)
  let exit = Sys.command (quote_cmd cmd) in
  if exit = 0 then () else err_cmd exit cmd

let read_cmd args =
  let stdout = Filename.temp_file (Filename.basename Sys.argv.(0)) "b00t" in
  at_exit (fun () -> try ignore (Sys.remove stdout) with _ -> ());
  let cmd = String.concat " " (List.map Filename.quote args) in
  let cmd = quote_cmd @@ strf "%s 1>%s" cmd (Filename.quote stdout) in
  let exit = Sys.command cmd in
  if exit = 0 then string_of_file stdout else err_cmd exit cmd

(* Create and delete directories *)

let mkdir dir =
  try match Sys.file_exists dir with
  | true -> ()
  | false -> run_cmd ["mkdir"; dir]
  with
  | Sys_error e -> err "%s: %s" dir e

let rmdir dir =
  try match Sys.file_exists dir with
  | false -> ()
  | true ->
      let rm f = Sys.remove (fpath ~dir f) in
      Array.iter rm (Sys.readdir dir);
      run_cmd ["rmdir"; dir]
  with
  | Sys_error e -> err "%s: %s" dir e

(* Lookup OCaml compilers and ocamldep *)

let really_find_cmd alts = match find_cmd alts with
| Some cmd -> cmd
| None -> err "No %s found in PATH\n" (List.hd @@ List.rev alts)

let ocamlc () = really_find_cmd ["ocamlc.opt"; "ocamlc"]
let ocamlopt () = really_find_cmd ["ocamlopt.opt"; "ocamlopt"]
let ocamldep () = really_find_cmd ["ocamldep.opt"; "ocamldep"]

(* Build *)

let sort_srcs srcs =
  let srcs = List.sort String.compare srcs in
  read_cmd (ocamldep () :: "-slash" :: "-sort" :: srcs)
  |> String.trim |> cuts ~sep:' '

let common srcs = base_ocaml_opts @ sort_srcs srcs

let build_cma srcs =
  run_cmd ([ocamlc ()] @ common srcs @ ["-a"; "-o"; "cmdliner.cma"])

let build_cmxa srcs =
  run_cmd ([ocamlopt ()] @ common srcs @ ["-a"; "-o"; "cmdliner.cmxa"])

let build_cmxs srcs =
  run_cmd ([ocamlopt ()] @ common srcs @ ["-shared"; "-o"; "cmdliner.cmxs"])

let clean () = rmdir build_dir

let in_build_dir f =
  let srcs = ml_srcs src_dir in
  let cp src = cp (fpath ~dir:src_dir src) (fpath ~dir:build_dir src) in
  mkdir build_dir;
  List.iter cp srcs;
  Sys.chdir build_dir; f srcs; Sys.chdir root_dir

let main () = match Array.to_list Sys.argv with
| _ :: [ "cma" ] -> in_build_dir build_cma
| _ :: [ "cmxa" ] -> in_build_dir build_cmxa
| _ :: [ "cmxs" ] -> in_build_dir build_cmxs
| _ :: [ "clean" ] -> clean ()
| [] | [_] -> err "Missing argument: cma, cmxa, cmxs or clean\n";
| cmd :: args ->
    err "%s: Unknown argument(s): %s\n" cmd @@ String.concat " " args

let () = main ()
