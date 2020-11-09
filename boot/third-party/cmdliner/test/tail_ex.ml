(* Example from the documentation, this code is in public domain. *)

(* Implementation of the command, we just print the args. *)

type loc = bool * int
type verb = Verbose | Quiet
type follow = Name | Descriptor

let str = Printf.sprintf
let opt_str sv = function None -> "None" | Some v -> str "Some(%s)" (sv v)
let loc_str (rev, k) = if rev then str "%d" k else str "+%d" k
let follow_str = function Name -> "name" | Descriptor -> "descriptor"
let verb_str = function Verbose -> "verbose" | Quiet -> "quiet"

let tail lines follow verb pid files =
  Printf.printf "lines = %s\nfollow = %s\nverb = %s\npid = %s\nfiles = %s\n"
    (loc_str lines) (opt_str follow_str follow) (verb_str verb)
    (opt_str string_of_int pid) (String.concat ", " files)

(* Command line interface *)

open Cmdliner

let lines =
  let loc =
    let parse s =
      try
        if s <> "" && s.[0] <> '+' then Ok (true, int_of_string s) else
        Ok (false, int_of_string (String.sub s 1 (String.length s - 1)))
      with Failure _ -> Error (`Msg "unable to parse integer")
    in
    let print ppf p = Format.fprintf ppf "%s" (loc_str p) in
    Arg.conv ~docv:"N" (parse, print)
  in
  Arg.(value & opt loc (true, 10) & info ["n"; "lines"] ~docv:"N"
         ~doc:"Output the last $(docv) lines or use $(i,+)$(docv) to start
               output after the $(i,N)-1th line.")
let follow =
  let doc = "Output appended data as the file grows. $(docv) specifies how the
             file should be tracked, by its `name' or by its `descriptor'." in
  let follow = Arg.enum ["name", Name; "descriptor", Descriptor] in
  Arg.(value & opt (some follow) ~vopt:(Some Descriptor) None &
       info ["f"; "follow"] ~docv:"ID" ~doc)

let verb =
  let doc = "Never output headers giving file names." in
  let quiet = Quiet, Arg.info ["q"; "quiet"; "silent"] ~doc in
  let doc = "Always output headers giving file names." in
  let verbose = Verbose, Arg.info ["v"; "verbose"] ~doc in
  Arg.(last & vflag_all [Quiet] [quiet; verbose])

let pid =
  let doc = "With -f, terminate after process $(docv) dies." in
  Arg.(value & opt (some int) None & info ["pid"] ~docv:"PID" ~doc)

let files = Arg.(value & (pos_all non_dir_file []) & info [] ~docv:"FILE")

let cmd =
  let doc = "display the last part of a file" in
  let man = [
    `S Manpage.s_description;
    `P "$(tname) prints the last lines of each $(i,FILE) to standard output. If
        no file is specified reads standard input. The number of printed
        lines can be  specified with the $(b,-n) option.";
    `S Manpage.s_bugs;
    `P "Report them to <hehey at example.org>.";
    `S Manpage.s_see_also;
    `P "$(b,cat)(1), $(b,head)(1)" ]
  in
  Term.(const tail $ lines $ follow $ verb $ pid $ files),
  Term.info "tail" ~version:"%%VERSION%%" ~doc ~exits:Term.default_exits ~man

let () = Term.(exit @@ eval cmd)
