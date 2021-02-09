open! Stdune
open Import

let all : _ Term.Group.t list =
  let terms =
    [ Installed_libraries.command
    ; External_lib_deps.command
    ; Build_cmd.build
    ; Build_cmd.runtest
    ; command_alias Build_cmd.runtest "test"
    ; Clean.command
    ; Install_uninstall.install
    ; Install_uninstall.uninstall
    ; Exec.command
    ; Subst.command
    ; Print_rules.command
    ; Utop.command
    ; Init.command
    ; Promote.command
    ; Printenv.command
    ; Help.command
    ; Format_dune_file.command
    ; Compute.command
    ; Upgrade.command
    ; Caching.command
    ; Describe.command
    ; Top.command
    ; Ocaml_merlin.command
    ]
    |> List.map ~f:in_group
  in
  let groups = [ Ocaml.group ] in
  terms @ groups

let common_commands_synopsis =
  (* Short reminders for the most used and useful commands *)
  let commands =
    [ "build [--watch]"
    ; "runtest [--watch]"
    ; "exec NAME"
    ; "utop [DIR]"
    ; "install"
    ; "init project NAME [PATH] [--libs=l1,l2 --ppx=p1,p2 --inline-tests]"
    ]
  in
  let format_command c acc =
    `Noblank :: `P (Printf.sprintf "$(b,dune %s)" c) :: acc
  in
  [ `S "SYNOPSIS"
  ; `Blocks (List.fold_right ~init:[] ~f:format_command commands)
  ]

let default =
  let doc = "composable build system for OCaml" in
  let term =
    Term.ret
    @@ let+ _ = Common.term in
       `Help (`Pager, None)
  in
  ( term
  , Term.info "dune" ~doc
      ~version:
        (match Build_info.V1.version () with
        | None -> "n/a"
        | Some v -> Build_info.V1.Version.to_string v)
      ~man:
        [ `Blocks common_commands_synopsis
        ; `S "DESCRIPTION"
        ; `P
            {|Dune is a build system designed for OCaml projects only. It
              focuses on providing the user with a consistent experience and takes
              care of most of the low-level details of OCaml compilation. All you
              have to do is provide a description of your project and Dune will
              do the rest.
            |}
        ; `P
            {|The scheme it implements is inspired from the one used inside Jane
              Street and adapted to the open source world. It has matured over a
              long time and is used daily by hundreds of developers, which means
              that it is highly tested and productive.
            |}
        ; `Blocks Common.help_secs
        ; Common.examples
            [ ("Initialise a new project named `foo'", "dune init project foo")
            ; ("Build all targets in the current source tree", "dune build")
            ; ("Run the executable named `bar'", "dune exec bar")
            ; ("Run all tests in the current source tree", "dune runtest")
            ; ("Install all components defined in the project", "dune install")
            ; ("Remove all build artefacts", "dune clean")
            ]
        ] )

let () =
  Colors.setup_err_formatter_colors ();
  try
    match Term.Group.eval default all ~catch:false with
    | `Error _ -> exit 1
    | _ -> exit 0
  with
  | exn ->
    let exn = Exn_with_backtrace.capture exn in
    Dune_engine.Report_error.report exn;
    exit 1
