(* Example from the documentation, this code is in public domain. *)

(* Implementation of the command, we just print the args. *)

type prompt = Always | Once | Never
let prompt_str = function
| Always -> "always" | Once -> "once" | Never -> "never"

let rm prompt recurse files =
  Printf.printf "prompt = %s\nrecurse = %B\nfiles = %s\n"
    (prompt_str prompt) recurse (String.concat ", " files)

(* Command line interface *)

open Cmdliner

let files = Arg.(non_empty & pos_all file [] & info [] ~docv:"FILE")
let prompt =
  let doc = "Prompt before every removal." in
  let always = Always, Arg.info ["i"] ~doc in
  let doc = "Ignore nonexistent files and never prompt." in
  let never = Never, Arg.info ["f"; "force"] ~doc in
  let doc = "Prompt once before removing more than three files, or when
             removing recursively. Less intrusive than $(b,-i), while
             still giving protection against most mistakes."
  in
  let once = Once, Arg.info ["I"] ~doc in
  Arg.(last & vflag_all [Always] [always; never; once])

let recursive =
  let doc = "Remove directories and their contents recursively." in
  Arg.(value & flag & info ["r"; "R"; "recursive"] ~doc)

let cmd =
  let doc = "remove files or directories" in
  let man = [
    `S Manpage.s_description;
    `P "$(tname) removes each specified $(i,FILE). By default it does not
        remove directories, to also remove them and their contents, use the
        option $(b,--recursive) ($(b,-r) or $(b,-R)).";
    `P "To remove a file whose name starts with a `-', for example
        `-foo', use one of these commands:";
    `Pre "$(mname) -- -foo\n\
          $(mname) ./-foo";
    `P "$(tname) removes symbolic links, not the files referenced by the
        links.";
    `S Manpage.s_bugs; `P "Report bugs to <hehey at example.org>.";
    `S Manpage.s_see_also; `P "$(b,rmdir)(1), $(b,unlink)(2)" ]
  in
  Term.(const rm $ prompt $ recursive $ files),
  Term.info "rm" ~version:"%%VERSION%%" ~doc ~exits:Term.default_exits ~man

let () = Term.(exit @@ eval cmd)
