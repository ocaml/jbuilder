open StdLabels
open Printf

(* This program performs version checking of the compiler and switches to the
   secondary compiler if necessary. The script should execute in OCaml 4.02! *)

let min_supported_natively = (4, 08, 0)

let verbose, keep_generated_files, debug =
  let anon s = raise (Arg.Bad (sprintf "don't know what to do with %s\n" s)) in
  let verbose = ref false in
  let keep_generated_files = ref false in
  let debug = ref false in
  Arg.parse
    [ ("-j", Arg.Int ignore, "JOBS Concurrency")
    ; ("--verbose", Arg.Set verbose, " Set the display mode")
    ; ( "--keep-generated-files"
      , Arg.Set keep_generated_files
      , " Keep generated files" )
    ; ("--debug", Arg.Set debug, " Enable various debugging options")
    ; ( "--force-byte-compilation"
      , Arg.Unit ignore
      , " Force bytecode compilation even if ocamlopt is available" )
    ]
    anon "Usage: ocaml bootstrap.ml <options>\nOptions are:";
  (!verbose, !keep_generated_files, !debug)

let modules = [ "boot/libs"; "boot/duneboot" ]

let duneboot = ".duneboot"

let prog = duneboot ^ ".exe"

let () =
  at_exit (fun () ->
      Array.iter (Sys.readdir "boot") ~f:(fun fn ->
          let fn = Filename.concat "boot" fn in
          if Filename.check_suffix fn ".cmi" || Filename.check_suffix fn ".cmo"
          then
            Sys.remove fn));
  if not keep_generated_files then
    at_exit (fun () ->
        Array.iter (Sys.readdir ".") ~f:(fun fn ->
            if
              String.length fn >= String.length duneboot
              && String.sub fn 0 (String.length duneboot) = duneboot
            then
              Sys.remove fn))

let runf fmt =
  ksprintf
    (fun cmd ->
      prerr_endline cmd;
      Sys.command cmd)
    fmt

let exit_if_non_zero = function
  | 0 -> ()
  | n -> exit n

let read_file fn =
  let ic = open_in_bin fn in
  let s = really_input_string ic (in_channel_length ic) in
  close_in ic;
  s

let () =
  let v = Scanf.sscanf Sys.ocaml_version "%d.%d.%d" (fun a b c -> (a, b, c)) in
  let compiler, which =
    if v >= min_supported_natively then
      ("ocamlc", None)
    else
      let compiler = "ocamlfind -toolchain secondary ocamlc" in
      let output_fn = duneboot ^ ".ocamlfind-output" in
      let n = runf "%s 2>%s" compiler output_fn in
      let s = read_file output_fn in
      prerr_endline s;
      if n <> 0 || s <> "" then (
        Format.eprintf "@[%a@]@." Format.pp_print_text
          (let a, b, _ = min_supported_natively in
           sprintf
             "The ocamlfind's secondary toolchain does not seem to be \
              correctly installed.\n\
              Dune requires OCaml %d.%02d or later to compile.\n\
              Please either upgrade your compile or configure a secondary \
              OCaml compiler (in opam, this can be done by installing the \
              ocamlfind-secondary package)."
             a b);
        exit 2
      );
      (compiler, Some "--secondary")
  in
  exit_if_non_zero
    (runf "%s %s -w -24 -g -o %s -I boot unix.cma %s" compiler
       (* Make sure to produce a self-contained binary as dlls tend to cause
          issues *)
       (if v < (4, 10, 1) then
         "-custom"
       else
         "-output-complete-exe")
       prog
       (List.map modules ~f:(fun m -> m ^ ".ml") |> String.concat ~sep:" "));
  let args = Array.to_list (Array.sub Sys.argv 1 (Array.length Sys.argv - 1)) in
  let args =
    match which with
    | None -> args
    | Some x -> x :: args
  in
  let args = Filename.concat "." prog :: args in
  exit (runf "%s" (String.concat ~sep:" " args))
