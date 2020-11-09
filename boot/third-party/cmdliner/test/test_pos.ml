open Cmdliner

let pos l t r =
  print_endline (String.concat "\n" (l @ ["--"; t; "--"] @ r))

let test_pos =
  let l = Arg.(value & pos_left 2 string [] & info [] ~docv:"LEFT") in
  let t = Arg.(value & pos 2 string "undefined" & info [] ~docv:"TWO") in
  let r = Arg.(value & pos_right 2 string [] & info [] ~docv:"RIGHT") in
  Term.(const pos $ l $ t $ r),
  Term.info "test_pos" ~doc:"Test pos arguments"

let () = Term.(exit @@ eval test_pos)
