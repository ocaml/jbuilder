open Cmdliner

let pos r a1 a0 a2  =
  print_endline (String.concat "\n" ([a0; a1; a2; "--"] @ r))

let test_pos =
  let req p =
    let docv = Printf.sprintf "ARG%d" p in
    Arg.(required & pos p (some string) None & info [] ~docv)
  in
  let right = Arg.(non_empty & pos_right 2 string [] & info [] ~docv:"RIGHT") in
  Term.(const pos $ right $ req 1 $ req 0 $ req 2),
  Term.info "test_pos_req" ~doc:"Test pos req arguments"

let () = Term.(exit @@ eval test_pos)
