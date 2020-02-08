open Stdune
open Import

let doc =
  "Execute a command in a similar environment as if installation was performed."

let man =
  [ `S "DESCRIPTION"
  ; `P
      {|$(b,dune exec -- COMMAND) should behave in the same way as if you
          do:|}
  ; `Pre "  \\$ dune install\n  \\$ COMMAND"
  ; `P
      {|In particular if you run $(b,dune exec ocaml), you will have
          access to the libraries defined in the workspace using your usual
          directives ($(b,#require) for instance)|}
  ; `P
      {|When a leading / is present in the command (absolute path), then the
          path is interpreted as an absolute path|}
  ; `P
      {|When a / is present at any other position (relative path), then the
          path is interpreted as relative to the build context + current
          working directory (or the value of $(b,--root) when ran outside of
          the project root)|}
  ; `Blocks Common.help_secs
  ; Common.examples
      [ ("Run the executable named `my_exec'", "dune exec my_exec")
      ; ( "Run the executable defined in `foo.ml' with the argument `arg'"
        , "dune exec ./foo.exe -- arg" )
      ]
  ]

let info = Term.info "exec" ~doc ~man

let term =
  let+ common = Common.term
  and+ context =
    Common.context_arg ~doc:{|Run the command in this build context.|}
  and+ prog =
    Arg.(required & pos 0 (some string) None (Arg.info [] ~docv:"PROG"))
  and+ no_rebuild =
    Arg.(
      value & flag
      & info [ "no-build" ] ~doc:"don't rebuild target before executing")
  and+ args = Arg.(value & pos_right 0 string [] (Arg.info [] ~docv:"ARGS")) in
  Common.set_common common ~targets:[ Arg.Dep.file prog ];
  let setup = Scheduler.go ~common (fun () -> Import.Main.setup common) in
  let context = Import.Main.find_context_exn setup.workspace ~name:context in
  let path_relative_to_build_root p =
    Common.prefix_target common p
    |> Path.Build.relative context.build_dir
    |> Path.build
  in
  let prog_where =
    match Filename.analyze_program_name prog with
    | Absolute -> `This_abs (Path.of_string prog)
    | In_path -> `Search prog
    | Relative_to_current_dir -> `This_rel (path_relative_to_build_root prog)
  in
  let targets =
    lazy
      ( ( match prog_where with
        | `Search p ->
          [ Path.Build.relative
              (Config.local_install_bin_dir ~context:context.name)
              p
            |> Path.build
          ]
        | `This_rel p when Sys.win32 ->
          [ p; Path.extend_basename p ~suffix:Bin.exe ]
        | `This_rel p -> [ p ]
        | `This_abs p when Path.is_in_build_dir p -> [ p ]
        | `This_abs _ -> [] )
      |> List.map ~f:(fun p -> Target.Path p)
      |> Target.resolve_targets_mixed common setup
      |> List.concat_map ~f:(function
           | Ok targets -> targets
           | Error _ -> []) )
  in
  let real_prog =
    ( if not no_rebuild then
      match Lazy.force targets with
      | [] -> ()
      | targets ->
        Scheduler.go ~common (fun () -> do_build targets);
        Hooks.End_of_build.run () );
    match prog_where with
    | `Search prog ->
      let path =
        Path.build (Config.local_install_bin_dir ~context:context.name)
        :: context.path
      in
      Bin.which prog ~path
    | `This_rel prog
    | `This_abs prog ->
      if Path.exists prog then
        Some prog
      else if not Sys.win32 then
        None
      else
        let prog = Path.extend_basename prog ~suffix:Bin.exe in
        Option.some_if (Path.exists prog) prog
  in
  (* Good candidates for the "./x.exe" instead of "x.exe" error are executables
     present in the current directory *)
  let hints () =
    let candidates =
      let path = path_relative_to_build_root "" in
      let targets =
        Scheduler.go ~common (fun () -> Build_system.targets_of ~dir:path)
      in
      Path.Set.to_list targets
      |> List.filter ~f:(fun p -> Path.extension p = ".exe")
      |> List.map ~f:(fun p -> "./" ^ Path.basename p)
    in
    User_message.did_you_mean prog ~candidates
  in
  match (real_prog, no_rebuild) with
  | None, true -> (
    match Lazy.force targets with
    | [] ->
      let hints = hints () in
      User_error.raise ~hints [ Pp.textf "Program %S not found!" prog ]
    | _ :: _ ->
      User_error.raise
        [ Pp.textf
            "Program %S isn't built yet. You need to build it first or remove \
             the --no-build option."
            prog
        ] )
  | None, false ->
    let hints = hints () in
    User_error.raise ~hints [ Pp.textf "Program %S not found!" prog ]
  | Some real_prog, _ ->
    let real_prog = Path.to_string real_prog in
    let argv = prog :: args in
    restore_cwd_and_execve common real_prog argv context.env

let command = (term, info)
