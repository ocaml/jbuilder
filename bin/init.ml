open Stdune
open Import
open Dune_rules.Dune_init

(** {1 Helper functions} *)

(** {2 Validation} *)

(* TODO(shonfeder): Remove when nested subcommands are available *)
let validate_component_options kind unsupported_options =
  let report_invalid_option = function
    | _, false -> () (* The option wasn't supplied *)
    | option_name, true ->
      User_error.raise
        [ Pp.textf "The `%s' component does not support the `--%s' option"
            (Kind.to_string kind) option_name
        ]
  in
  List.iter ~f:report_invalid_option unsupported_options

(** {2 Cmdliner Argument Converters} *)

let atom_parser s =
  match Dune_lang.Atom.parse s with
  | Some s -> Ok s
  | None -> Error (`Msg "expected a valid dune atom")

let atom_printer ppf a = Format.pp_print_string ppf (Dune_lang.Atom.to_string a)

let component_name_parser s =
  (* TODO refactor to use Lib_name.Local.conv *)
  let err_msg () =
    User_error.make
      [ Pp.textf "invalid component name `%s'" s
      ; Lib_name.Local.valid_format_doc
      ]
    |> User_message.to_string
    |> fun m -> `Msg m
  in
  let open Result.O in
  let* atom = atom_parser s in
  let* _ =
    match Lib_name.Local.of_string_opt s with
    | None -> Error (err_msg ())
    | Some s -> Ok s
  in
  Ok atom

let atom_conv = Arg.conv (atom_parser, atom_printer)

let component_name_conv = Arg.conv (component_name_parser, atom_printer)

let public_name_conv =
  let open Component.Options in
  let parser = function
    | "" -> Ok Use_name
    | s -> component_name_parser s |> Result.map ~f:(fun a -> Public_name a)
  in
  let printer ppf public_name =
    Format.pp_print_string ppf (public_name_to_string public_name)
  in
  Arg.conv (parser, printer)

(** {2 Status reporting} *)

let print_completion kind name =
  let open Pp.O in
  Dune_util.Console.print_user_message
    (User_message.make
       [ Pp.tag User_message.Style.Ok (Pp.verbatim "Success")
         ++ Pp.textf ": initialized %s component named " (Kind.to_string kind)
         ++ Pp.tag User_message.Style.Kwd
              (Pp.verbatim (Dune_lang.Atom.to_string name))
       ])

(** {1 CLI} *)

let doc = "Initialize dune components"

let man =
  [ `S "DESCRIPTION"
  ; `P
      {|$(b,dune init {library,executable,test,project} NAME [PATH]) initialize
         a new dune component of the specified kind, named $(b,NAME), with
         fields determined by the supplied options.|}
  ; `P
      {|Any prefix of the component kinds can be supplied, e.g., $(b,dune init
         proj myproject).|}
  ; `P
      {|If the optional $(b,PATH) is provided, the component will be created
         there. Otherwise, it is created in the current working directory.|}
  ; `P
      {|The command can be used to add stanzas to existing dune files as
         well as for creating new dune files and basic component templates.|}
  ; Common.examples
      [ ( {|Define an executable component named `myexe' in a dune file in the
            current directory
           |}
        , {|dune init exe myexe|} )
      ; ( {|Define a library component named `mylib' in a dune file in the ./src
            directory depending on the core and cmdliner libraries, the ppx_let
            and ppx_inline_test preprocessors, and declared as using inline
            tests"
           |}
        , {|dune init lib mylib src --libs core,cmdliner --ppx ppx_let,ppx_inline_test --inline-tests"|}
        )
      ; ( {|Define a library component named `mytest' in a dune file in the
            ./test directory that depends on `mylib'"
           |}
        , {|dune init test myexe test --libs mylib|} )
      ]
  ]

let info = Term.info "init" ~doc ~man

let term =
  let+ common_term = Common.term
  and+ kind =
    (* TODO(shonfeder): Replace with nested subcommand once we have support for
       that *)
    let docv = "INIT_KIND" in
    Arg.(required & pos 0 (some (enum Kind.commands)) None & info [] ~docv)
  and+ name =
    let docv = "NAME" in
    Arg.(required & pos 1 (some component_name_conv) None & info [] ~docv)
  and+ path =
    let docv = "PATH" in
    Arg.(value & pos 2 (some string) None & info [] ~docv)
  and+ libraries =
    let docv = "LIBRARIES" in
    let doc =
      "A comma separated list of libraries on which the component depends"
    in
    Arg.(value & opt (list atom_conv) [] & info [ "libs" ] ~docv ~doc)
  and+ pps =
    let docv = "PREPROCESSORS" in
    let doc =
      "A comma separated list of ppx preprocessors used by the component"
    in
    Arg.(value & opt (list atom_conv) [] & info [ "ppx" ] ~docv ~doc)
  and+ public =
    (* TODO(shonfeder): Move to subcommands {lib, exe} once implemented *)
    let docv = "PUBLIC_NAME" in
    let doc =
      "If called with an argument, make the component public under the given \
       PUBLIC_NAME. If supplied without an argument, use NAME."
    in
    Arg.(
      value
      & opt ~vopt:(Some Component.Options.Use_name) (some public_name_conv) None
      & info [ "public" ] ~docv ~doc)
  and+ inline_tests =
    (* TODO(shonfeder): Move to subcommand [lib] once implemented *)
    let docv = "USE_INLINE_TESTS" in
    let doc =
      "Whether to use inline tests. Only applicable for $(b,library) and \
       $(b,project) components."
    in
    Arg.(value & flag & info [ "inline-tests" ] ~docv ~doc)
  and+ template =
    let docv = "PROJECT_KIND" in
    let doc =
      "The kind of project to initialize. Valid options are $(b,e[xecutable]) \
       or $(b,l[ibrary]). Defaults to $(b,executable). Only applicable for \
       $(b,project) components."
    in
    Arg.(
      value
      & opt (some (enum Component.Options.Project.Template.commands)) None
      & info [ "kind" ] ~docv ~doc)
  and+ pkg =
    let docv = "PACKAGE_MANAGER" in
    let doc =
      "Which package manager to use. Valid options are $(b,o[pam]) or \
       $(b,e[sy]). Defaults to $(b,opam). Only applicable for $(b,project) \
       components."
    in
    Arg.(
      value
      & opt (some (enum Component.Options.Project.Pkg.commands)) None
      & info [ "pkg" ] ~docv ~doc)
  in
  Common.set_common common_term ~targets:[];
  let open Component in
  let context = Init_context.make path in
  let common : Options.Common.t = { name; libraries; pps } in
  let given_public = Option.is_some public in
  let given_pkg = Option.is_some pkg in
  let given_template = Option.is_some template in
  let pkg = Option.value pkg ~default:Options.Project.Pkg.Opam in
  let template = Option.value template ~default:Options.Project.Template.Exec in
  (* for the [kind] of initialization *)
  let check_unsupported_options = validate_component_options kind in
  ( match kind with
  | Kind.Executable ->
    check_unsupported_options
      [ ("inline-tests", inline_tests)
      ; ("kind", given_template)
      ; ("pkg", given_pkg)
      ];
    init @@ Executable { context; common; options = { public } }
  | Kind.Library ->
    check_unsupported_options [ ("kind", given_template); ("pkg", given_pkg) ];
    init @@ Library { context; common; options = { public; inline_tests } }
  | Kind.Project ->
    check_unsupported_options [ ("public", given_public) ];
    init
    @@ Project { context; common; options = { inline_tests; pkg; template } }
  | Kind.Test ->
    check_unsupported_options
      [ ("public", given_public)
      ; ("inline-tests", inline_tests)
      ; ("kind", given_template)
      ; ("pkg", given_pkg)
      ];
    init @@ Test { context; common; options = () } );
  print_completion kind name

let command = (term, info)
