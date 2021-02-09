open! Stdune
open Import

let doc = "Start a merlin configuration server"

let man =
  [ `S "DESCRIPTION"
  ; `P
      {|$(b,dune ocaml-merlin) starts a server that can be queried to get
      .merlin information. It is meant to be used by Merlin itself and does not
      provide a user-friendly output.|}
  ; `Blocks Common.help_secs
  ; Common.footer
  ]

let info = Term.info "ocaml-merlin" ~doc ~man

let term =
  let+ common = Common.term
  and+ dump_config =
    Arg.(
      value
      & opt (some string) None
      & info [ "dump-config" ]
          ~doc:
            "Prints the entire content of the merlin configuration for the \
             given folder in a user friendly form. This is for testing and \
             debugging purposes only and should not be considered as a stable \
             ouptut.")
  in
  Common.set_common common ~log_file:No_log_file ~targets:[];
  Scheduler.go ~common (fun () ->
      Dune_engine.File_tree.init ~recognize_jbuilder_projects:true
        ~ancestor_vcs:None;
      let x = Common.x common in
      let workspace_file =
        Common.workspace_file common |> Option.map ~f:Arg.Path.path
      in
      Dune_rules.Workspace.init ?x ?workspace_file ();
      (match dump_config with
      | Some s -> Dune_rules.Merlin_server.dump s
      | None -> Dune_rules.Merlin_server.start ())
      |> Fiber.return)

let command = (term, info)
