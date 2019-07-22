open Dune.Import
open Import

let info =
  Term.info "describe-project"
    ~doc:"Describe a project"
    ~man:
      [ `S "DESCRIPTION"
      ; `P "Describe the contents available in a project."
      ; `Blocks Common.help_secs
      ]

let only_project (conf:Dune.Dune_load.conf) =
  match conf.projects with
  | [x] -> x
  | projects ->
    Code_error.raise
      "Cannot determine project to describe"
      [ ( "projects"
        , List (List.map ~f:Dune_project.to_dyn projects)
        )
      ]

let format_arg =
  let open Dune.Describe_project in
  let values =
    [ ("text", Text)
    ; ("json", JSON)
    ; ("sexp", Sexp)
    ]
  in
  Arg.
    (value
     & opt (enum values) Text
     & info ["format"] ~docv:"FORMAT" ~doc:"Select the output format")

let term =
  let+ common = Common.term
  and+ context_name = Common.context_arg ~doc:"Run the command in this build context."
  and+ format = format_arg
  in
  Common.set_common common ~targets:[];
  let log = Log.create common in
  Scheduler.go ~log ~common  (fun () ->
    let open Fiber.O in
    let* workspace = Import.Main.scan_workspace ~log common in
    let conf = workspace.conf in
    let project = only_project conf in
    let context = Import.Main.find_context_exn workspace ~name:context_name in
    let+ dune_files = Dune.Dune_load.Dune_files.eval conf.dune_files ~context in
    Dune.Describe_project.describe project dune_files ~format
  )

let command = (term, info)
