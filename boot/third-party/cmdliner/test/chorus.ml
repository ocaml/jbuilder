(* Example from the documentation, this code is in public domain. *)

(* Implementation of the command *)

let chorus count msg = for i = 1 to count do print_endline msg done

(* Command line interface *)

open Cmdliner

let count =
  let doc = "Repeat the message $(docv) times." in
  Arg.(value & opt int 10 & info ["c"; "count"] ~docv:"COUNT" ~doc)

let msg =
  let doc = "Overrides the default message to print." in
  let env = Arg.env_var "CHORUS_MSG" ~doc in
  let doc = "The message to print." in
  Arg.(value & pos 0 string "Revolt!" & info [] ~env ~docv:"MSG" ~doc)

let chorus_t = Term.(const chorus $ count $ msg)

let info =
  let doc = "print a customizable message repeatedly" in
  let man = [
    `S Manpage.s_bugs;
    `P "Email bug reports to <hehey at example.org>." ]
  in
  Term.info "chorus" ~version:"%%VERSION%%" ~doc ~exits:Term.default_exits ~man

let () = Term.exit @@ Term.eval (chorus_t, info)
