open Cmdliner

let pos l =
  print_endline (String.concat "\n" l)

let test_pos_left =
  let l = Arg.(value & pos_left 2 string [] & info [] ~docv:"LEFT") in
  Term.(const pos $ l),
  Term.info "test_pos" ~doc:"Test pos left"

let () = Term.(exit @@ eval test_pos_left)
