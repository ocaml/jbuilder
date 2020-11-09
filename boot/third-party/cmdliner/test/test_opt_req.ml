open Cmdliner

let opt o = print_endline o

let test_opt =
  let req =
    Arg.(required & opt (some string) None & info ["r"; "req"] ~docv:"ARG")
  in
  Term.(const opt $ req),
  Term.info "test_opt_req"
    ~doc:"Test optional required arguments (don't do this)"

let () = Term.(exit @@ eval test_opt)
