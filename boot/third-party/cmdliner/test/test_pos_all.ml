open Cmdliner

let pos_all all = print_endline (String.concat "\n" all)

let test_pos_all =
  let docv = "THEARG" in
  let all = Arg.(value & pos_all string [] & info [] ~docv) in
  Term.(const pos_all $ all),
  Term.info "test_pos_all" ~doc:"Test pos all"

let () = Term.(exit @@ eval test_pos_all)
