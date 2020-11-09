open Cmdliner

let nop () = print_endline "It's the manual that is of interest."


let test_pos =
  Term.(const nop $ const ()),
  Term.info "test_pos"
    ~doc:"UTF-8 test: íöüóőúűéáăîâșț ÍÜÓŐÚŰÉÁĂÎÂȘȚ 雙峰駱駝"

let () = Term.(exit @@ eval test_pos)
