open Cmdliner

let print_args ((), args) _other =
  print_endline (String.concat " " args)

let test_pos_left =
  let a = Arg.(value & flag & info ["a"; "aaa"]) in
  let b = Arg.(value & opt (some string) None & info ["b"; "bbb"]) in
  let c = Arg.(value & pos_all string [] & info []) in
  let main =
    let ignore_values _a _b _c = () in
    Term.(with_used_args (const ignore_values $ a $ b $ c))
  in
  let other = Arg.(value & flag & info ["other"]) in
  Term.(const print_args $ main $ other),
  Term.info "test_capture" ~doc:"Test pos left"

let () = Term.(exit @@ eval test_pos_left)
