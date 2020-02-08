open! Stdune
open Import

let run_build_command ~common ~targets =
  let once () =
    let open Fiber.O in
    let* setup = Main.setup common in
    do_build (targets setup)
  in
  if Common.watch common then
    let once () =
      Cached_digest.invalidate_cached_timestamps ();
      once ()
    in
    Scheduler.poll ~common ~once ~finally:Hooks.End_of_build.run ()
  else
    Scheduler.go ~common once;
  match Build_system.get_cache () with
  | Some { cache = (module Caching : Dune_cache.Caching); _ } ->
    (* Synchronously wait for the end of the connection with the cache daemon,
       ensuring all dedup messages have been queued. *)
    Caching.Cache.teardown Caching.cache;
    (* Hande all remaining dedup mesages. *)
    Scheduler.wait_for_dune_cache ()
  | None -> ()

let build_targets =
  let doc =
    "Build the given targets, or all installable targets if none are given."
  in
  let man =
    [ `S "DESCRIPTION"
    ; `P {|Targets starting with a $(b,@) are interpreted as aliases.|}
    ; `Blocks Common.help_secs
    ; Common.examples
        [ ("Build all targets in the current source tree", "dune build")
        ; ("Build targets in the `./foo/bar' directory", "dune build ./foo/bar")
        ; ( "Build the minimal set of targets required for tooling such as \
             Merlin (useful for quickly detecting errors)"
          , "dune build @check" )
        ; ( "Run all code formatting tools in-place"
          , "dune build --auto-promote @fmt" )
        ]
    ]
  in
  let name_ = Arg.info [] ~docv:"TARGET" in
  let term =
    let+ common = Common.term
    and+ targets = Arg.(value & pos_all dep [] name_) in
    let targets =
      match targets with
      | [] -> [ Common.default_target common ]
      | _ :: _ -> targets
    in
    Common.set_common common ~targets;
    let targets setup = Target.resolve_targets_exn common setup targets in
    run_build_command ~common ~targets
  in
  (term, Term.info "build" ~doc ~man)

let runtest =
  let doc = "Run tests." in
  let man =
    [ `S "DESCRIPTION"
    ; `P {|This is a short-hand for calling:|}
    ; `Pre {|  dune build @runtest|}
    ; `Blocks Common.help_secs
    ; Common.examples
        [ ( "Run all tests in the current source tree (including those that \
             passed on the last run)"
          , "dune runtest --force" )
        ; ( "Run tests sequentially without output buffering"
          , "dune runtest --no-buffer -j 1" )
        ]
    ]
  in
  let name_ = Arg.info [] ~docv:"DIR" in
  let term =
    let+ common = Common.term
    and+ dirs = Arg.(value & pos_all string [ "." ] name_) in
    Common.set_common common
      ~targets:
        (List.map dirs ~f:(fun s ->
             let dir = Path.Local.of_string s in
             Arg.Dep.alias_rec ~dir Dune.Alias.Name.runtest));
    let targets (setup : Main.build_system) =
      List.map dirs ~f:(fun dir ->
          let dir = Path.(relative root) (Common.prefix_target common dir) in
          Target.Alias
            (Alias.in_dir ~name:Dune.Alias.Name.runtest ~recursive:true
               ~contexts:setup.workspace.contexts dir))
    in
    run_build_command ~common ~targets
  in
  (term, Term.info "runtest" ~doc ~man)

let clean =
  let doc = "Clean the project." in
  let man =
    [ `S "DESCRIPTION"
    ; `P
        {|Removes files added by dune such as _build, <package>.install, and .merlin|}
    ; `Blocks Common.help_secs
    ]
  in
  let term =
    let+ common = Common.term in
    (* Pass [No_log_file] to prevent the log file from being created. Indeed, we
       are going to delete the whole build directory right after and that
       includes deleting the log file. Not only creating the log file would be
       useless but with some FS this also causes [dune clean] to fail (cf
       https://github.com/ocaml/dune/issues/2964). *)
    Common.set_common common ~targets:[] ~log_file:No_log_file;
    Build_system.files_in_source_tree_to_delete ()
    |> Path.Set.iter ~f:Path.unlink_no_err;
    Path.rm_rf Path.build_dir
  in
  (term, Term.info "clean" ~doc ~man)

let promote =
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
      ( match files with
      | [] -> All
      | _ ->
        let files =
          List.map files ~f:(fun fn ->
              Path.Source.of_string (Common.prefix_target common fn))
        in
        let on_missing fn =
          Format.eprintf "@{<warning>Warning@}: Nothing to promote for %a.@."
            Path.Source.pp fn
        in
        These (files, on_missing) )
  in
  (term, Term.info "promote" ~doc ~man)

(* Adapted from
   https://github.com/ocaml/opam/blob/fbbe93c3f67034da62d28c8666ec6b05e0a9b17c/src/client/opamArg.ml#L759 *)
let command_alias cmd name =
  let term, info = cmd in
  let orig = Term.name info in
  let doc = Printf.sprintf "An alias for $(b,%s)." orig in
  let man =
    [ `S "DESCRIPTION"
    ; `P
        (Printf.sprintf "$(mname)$(b, %s) is an alias for $(mname)$(b, %s)."
           name orig)
    ; `P (Printf.sprintf "See $(mname)$(b, %s --help) for details." orig)
    ; `Blocks Common.help_secs
    ]
  in
  (term, Term.info name ~docs:"COMMAND ALIASES" ~doc ~man)

let all =
  [ Installed_libraries.command
  ; External_lib_deps.command
  ; build_targets
  ; runtest
  ; command_alias runtest "test"
  ; clean
  ; Install_uninstall.install
  ; Install_uninstall.uninstall
  ; Exec.command
  ; Subst.command
  ; Print_rules.command
  ; Utop.command
  ; Init.command
  ; promote
  ; Printenv.command
  ; Help.command
  ; Format_dune_file.command
  ; Compute.command
  ; Upgrade.command
  ; Cache_daemon.command
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
        ( match Build_info.V1.version () with
        | None -> "n/a"
        | Some v -> Build_info.V1.Version.to_string v )
      ~man:
        [ `S "DESCRIPTION"
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
    match Term.eval_choice default all ~catch:false with
    | `Error _ -> exit 1
    | _ -> exit 0
  with
  | Fiber.Never -> exit 1
  | exn ->
    let exn = Exn_with_backtrace.capture exn in
    Report_error.report exn;
    exit 1
