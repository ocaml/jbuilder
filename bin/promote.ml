open Stdune
open Import

let command =
  let doc = "Promote files from the last run" in
  let man =
    [ `S "DESCRIPTION"
    ; `P
        {|Considering all actions of the form $(b,(diff a b)) that failed
           in the last run of dune, $(b,dune promote) does the following:

           If $(b,a) is present in the source tree but $(b,b) isn't, $(b,b) is
           copied over to $(b,a) in the source tree. The idea behind this is that
           you might use $(b,(diff file.expected file.generated)) and then call
           $(b,dune promote) to promote the generated file.
         |}
    ; `Blocks Common.help_secs
    ]
  in
  let term =
    let+ common = Common.term
    and+ files =
      Arg.(value & pos_all Cmdliner.Arg.file [] & info [] ~docv:"FILE")
    in
    Common.set_common common ~targets:[];
    Promotion.promote_files_registered_in_last_run
      (match files with
      | [] -> All
      | _ ->
        let files =
          List.map files ~f:(fun fn ->
              Path.Source.of_string (Common.prefix_target common fn))
        in
        let on_missing fn =
          Format.eprintf "@{<warning>Warning@}: Nothing to promote for %s.@."
            (Path.Source.to_string_maybe_quoted fn)
        in
        These (files, on_missing))
  in
  (term, Term.info "promote" ~doc ~man)
