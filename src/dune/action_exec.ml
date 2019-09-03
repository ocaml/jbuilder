open! Stdune
open Import
open Fiber.O

type done_or_more_deps =
  | Done
  | Need_more_deps of Dep.t Dune_action.Protocol.Dependency.Map.t

type exec_context =
  { targets : Path.Build.Set.t
  ; context : Context.t option
  ; purpose : Process.purpose
  ; rule_loc : Loc.t
  ; prepared_dependencies : Dune_action.Protocol.Dependency.Set.t
  }

type exec_environment =
  { working_dir : Path.t
  ; env : Env.t
  ; stdout_to : Process.Io.output Process.Io.t
  ; stderr_to : Process.Io.output Process.Io.t
  ; stdin_from : Process.Io.input Process.Io.t
  }

let validate_context_and_prog context prog =
  match context with
  | None
   |Some { Context.for_host = None; _ } ->
    ()
  | Some ({ Context.for_host = Some host; _ } as target) ->
    let invalid_prefix prefix =
      match Path.descendant prog ~of_:prefix with
      | None -> ()
      | Some _ ->
        User_error.raise
          [ Pp.textf "Context %s has a host %s." target.name host.name
          ; Pp.textf "It's not possible to execute binary %s in it."
            (Path.to_string_maybe_quoted prog)
          ; Pp.nop
          ; Pp.text "This is a bug and should be reported upstream."
          ]
    in
    invalid_prefix (Path.relative Path.build_dir target.name);
    invalid_prefix (Path.relative Path.build_dir ("install/" ^ target.name))

let ensure_at_most_one_dynamic_run ~ectx action =
  let rec loop : Action.t -> bool = function
    | Dynamic_run _ -> true
    | Chdir (_, t)
     |Setenv (_, _, t)
     |Redirect_out (_, _, t)
     |Redirect_in (_, _, t)
     |Ignore (_, t) ->
      loop t
    | Run _
     |Echo _
     |Cat _
     |Copy _
     |Symlink _
     |Copy_and_add_line_directive _
     |System _
     |Bash _
     |Write_file _
     |Rename _
     |Remove_tree _
     |Mkdir _
     |Digest_files _
     |Diff _
     |Merge_files_into _ ->
      false
    | Progn ts ->
      List.fold_left ts ~init:false ~f:(fun acc t ->
        match (acc, loop t) with
        | false, true
         |true, false ->
          true
        | false, false -> false
        | true, true ->
          User_error.raise ~loc:ectx.rule_loc
            [ Pp.text
              "Multiple 'dynamic-run' commands within single action are not \
               supported."
            ])
  in
  ignore (loop action)

let exec_run ~ectx ~eenv prog args =
  validate_context_and_prog ectx.context prog;
  Process.run Strict ~dir:eenv.working_dir ~env:eenv.env
    ~stdout_to:eenv.stdout_to ~stderr_to:eenv.stderr_to
    ~stdin_from:eenv.stdin_from ~purpose:ectx.purpose prog args

let exec_run_dynamic_client ~ectx ~eenv prog args =
  validate_context_and_prog ectx.context prog;
  let open Dune_action in
  let to_dune_dep : Protocol.Dependency.t -> Dep.t =
    let to_dune_path = Stdune.Path.relative eenv.working_dir in
    function
    | File path -> Dep.file (to_dune_path path)
    | Directory path ->
      Dep.file_selector
        (File_selector.from_glob ~dir:(to_dune_path path) Glob.universal)
  in
  let run_arguments_fn = Filename.temp_file "" ".run_in_dune" in
  let response_fn = Filename.temp_file "" ".response" in
  let run_arguments =
    let prepared_dependencies = ectx.prepared_dependencies in
    let targets =
      let to_relative path =
        path |> Stdune.Path.build |> Stdune.Path.reach ~from:eenv.working_dir
      in
      Stdune.Path.Build.Set.to_list ectx.targets
      |> List.map ~f:to_relative |> String.Set.of_list
    in
    Protocol.Run_arguments.{ prepared_dependencies; targets }
  in
  Io.String_path.write_file run_arguments_fn
    (run_arguments |> Protocol.Run_arguments.sexp_of_t |> Csexp.to_string);
  let env =
    let value =
      Protocol.Greeting.(sexp_of_t { run_arguments_fn; response_fn })
      |> Csexp.to_string
    in
    Env.add eenv.env ~var:Protocol.run_by_dune_env_variable ~value
  in
  let+ () =
    Process.run Strict ~dir:eenv.working_dir ~env ~stdout_to:eenv.stdout_to
      ~stderr_to:eenv.stderr_to ~stdin_from:eenv.stdin_from
      ~purpose:ectx.purpose prog args
  in
  let response = Io.String_path.read_file response_fn in
  Stdune.Path.(
    unlink_no_err (of_string run_arguments_fn);
    unlink_no_err (of_string response_fn));
  let prog_name = Stdune.Path.reach ~from:eenv.working_dir prog in
  match
    Result.O.(Csexp.parse_string response >>| Protocol.Response.t_of_sexp)
  with
  | Error _
   |Ok None
    when String.is_empty response ->
    User_error.raise ~loc:ectx.rule_loc
      [ Pp.textf
        "Executable '%s' that was declared to support dynamic dependency \
         discovery (declared by using 'dynamic-run' tag) failed to respond to \
         dune."
        prog_name
      ; Pp.nop
      ; Pp.text
        "If you don't use dynamic dependency discovery in your executable you \
         may consider changing 'dynamic-run' to 'run' in your rule definition."
      ]
  | Error _
   |Ok None ->
    User_error.raise ~loc:ectx.rule_loc
      [ Pp.textf
        "Executable '%s' declared as a dynamic dune action responded with \
         invalid message."
        prog_name
      ; Pp.text
        "Are you using different dune version to compile the executable?"
      ]
  | Ok (Some Done) -> Done
  | Ok (Some (Need_more_deps deps)) ->
    Need_more_deps
      Protocol.Dependency.(
        deps |> Set.to_map |> Map.mapi ~f:(fun e () -> to_dune_dep e))

let exec_echo stdout_to str =
  Fiber.return (output_string (Process.Io.out_channel stdout_to) str)

let rec exec t ~ectx ~eenv =
  match (t : Action.t) with
  | Run (Error e, _) -> Action.Prog.Not_found.raise e
  | Run (Ok prog, args) ->
    let+ () = exec_run ~ectx ~eenv prog args in
    Done
  | Dynamic_run (Error e, _) -> Action.Prog.Not_found.raise e
  | Dynamic_run (Ok prog, args) ->
    exec_run_dynamic_client ~ectx ~eenv prog args
  | Chdir (dir, t) -> exec t ~ectx ~eenv:{ eenv with working_dir = dir }
  | Setenv (var, value, t) ->
    exec t ~ectx ~eenv:{ eenv with env = Env.add eenv.env ~var ~value }
  | Redirect_out (Stdout, fn, Echo s) ->
    Io.write_file (Path.build fn) (String.concat s ~sep:" ");
    Fiber.return Done
  | Redirect_out (outputs, fn, t) ->
    let fn = Path.build fn in
    redirect_out t ~ectx ~eenv outputs fn
  | Redirect_in (inputs, fn, t) -> redirect_in t ~ectx ~eenv inputs fn
  | Ignore (outputs, t) -> redirect_out t ~ectx ~eenv outputs Config.dev_null
  | Progn ts -> exec_list ts ~ectx ~eenv
  | Echo strs ->
    let+ () = exec_echo eenv.stdout_to (String.concat strs ~sep:" ") in
    Done
  | Cat fn ->
    Io.with_file_in fn ~f:(fun ic ->
      Io.copy_channels ic (Process.Io.out_channel eenv.stdout_to));
    Fiber.return Done
  | Copy (src, dst) ->
    let dst = Path.build dst in
    Io.copy_file ~src ~dst ();
    Fiber.return Done
  | Symlink (src, dst) ->
    ( if Sys.win32 then
      let dst = Path.build dst in
      Io.copy_file ~src ~dst ()
    else
      let src =
        match Path.Build.parent dst with
        | None -> Path.to_string src
        | Some from ->
          let from = Path.build from in
          Path.reach ~from src
      in
      let dst = Path.Build.to_string dst in
      match Unix.readlink dst with
      | target ->
        if target <> src then (
          (* @@DRA Win32 remove read-only attribute needed when symlinking
            enabled *)
          Unix.unlink dst;
          Unix.symlink src dst
        )
      | exception _ -> Unix.symlink src dst );
    Fiber.return Done
  | Copy_and_add_line_directive (src, dst) ->
    Io.with_file_in src ~f:(fun ic ->
      Path.build dst
      |> Io.with_file_out ~f:(fun oc ->
        let fn = Path.drop_optional_build_context_maybe_sandboxed src in
        output_string oc
          (Utils.line_directive ~filename:(Path.to_string fn) ~line_number:1);
        Io.copy_channels ic oc));
    Fiber.return Done
  | System cmd ->
    let path, arg =
      Utils.system_shell_exn ~needed_to:"interpret (system ...) actions"
    in
    let+ () = exec_run ~ectx ~eenv path [ arg; cmd ] in
    Done
  | Bash cmd ->
    let+ () =
      exec_run ~ectx ~eenv
        (Utils.bash_exn ~needed_to:"interpret (bash ...) actions")
        [ "-e"; "-u"; "-o"; "pipefail"; "-c"; cmd ]
    in
    Done
  | Write_file (fn, s) ->
    Io.write_file (Path.build fn) s;
    Fiber.return Done
  | Rename (src, dst) ->
    Unix.rename (Path.Build.to_string src) (Path.Build.to_string dst);
    Fiber.return Done
  | Remove_tree path ->
    Path.rm_rf (Path.build path);
    Fiber.return Done
  | Mkdir path ->
    if Path.is_in_build_dir path then
      Path.mkdir_p path
    else
      Code_error.raise "Action_exec.exec: mkdir on non build dir"
        [ ("path", Path.to_dyn path) ];
    Fiber.return Done
  | Digest_files paths ->
    let s =
      let data =
        List.map paths ~f:(fun fn ->
          (Path.to_string fn, Cached_digest.file fn))
      in
      Digest.generic data
    in
    let+ () = exec_echo eenv.stdout_to (Digest.to_string_raw s) in
    Done
  | Diff ({ optional; file1; file2; mode } as diff) ->
    let remove_intermediate_file () =
      if optional then
        try Path.unlink file2 with Unix.Unix_error (ENOENT, _, _) -> ()
    in
    if Diff.eq_files diff then (
      remove_intermediate_file ();
      Fiber.return Done
    ) else
      let is_copied_from_source_tree file =
        match Path.extract_build_context_dir_maybe_sandboxed file with
        | None -> false
        | Some (_, file) -> Path.exists (Path.source file)
      in
      let+ () =
        Fiber.finalize
          (fun () ->
            if mode = Binary then
              User_error.raise
                [ Pp.textf "Files %s and %s differ."
                  (Path.to_string_maybe_quoted file1)
                    (Path.to_string_maybe_quoted file2)
                ]
            else
              Print_diff.print file1 file2
                ~skip_trailing_cr:(mode = Text && Sys.win32))
          ~finally:(fun () ->
            ( match optional with
            | false ->
              if
                is_copied_from_source_tree file1
                && not (is_copied_from_source_tree file2)
              then
                Promotion.File.register_dep
                  ~source_file:
                    (snd
                      (Option.value_exn
                        (Path.extract_build_context_dir_maybe_sandboxed file1)))
                  ~correction_file:(Path.as_in_build_dir_exn file2)
            | true ->
              if is_copied_from_source_tree file1 then
                Promotion.File.register_intermediate
                  ~source_file:
                    (snd
                      (Option.value_exn
                        (Path.extract_build_context_dir_maybe_sandboxed file1)))
                  ~correction_file:(Path.as_in_build_dir_exn file2)
              else
                remove_intermediate_file () );
            Fiber.return ())
      in
      Done
  | Merge_files_into (sources, extras, target) ->
    let lines =
      List.fold_left
        ~init:(String.Set.of_list extras)
        ~f:(fun set source_path ->
          Io.lines_of_file source_path
          |> String.Set.of_list |> String.Set.union set)
        sources
    in
    let target = Path.build target in
    Io.write_lines target (String.Set.to_list lines);
    Fiber.return Done

and redirect_out t ~ectx ~eenv outputs fn =
  let out = Process.Io.file fn Process.Io.Out in
  let stdout_to, stderr_to =
    match outputs with
    | Stdout -> (out, eenv.stderr_to)
    | Stderr -> (eenv.stdout_to, out)
    | Outputs -> (out, out)
  in
  exec t ~ectx ~eenv:{ eenv with stdout_to; stderr_to }
  >>| fun result ->
  Process.Io.release out;
  result

and redirect_in t ~ectx ~eenv inputs fn =
  let in_ = Process.Io.file fn Process.Io.In in
  let stdin_from =
    match inputs with
    | Stdin -> in_
  in
  exec t ~ectx ~eenv:{ eenv with stdin_from }
  >>| fun result ->
  Process.Io.release in_;
  result

and exec_list ts ~ectx ~eenv =
  match ts with
  | [] -> Fiber.return Done
  | [ t ] -> exec t ~ectx ~eenv
  | t :: rest -> (
    let* done_or_deps =
      let stdout_to = Process.Io.multi_use eenv.stdout_to in
      let stderr_to = Process.Io.multi_use eenv.stderr_to in
      let stdin_from = Process.Io.multi_use eenv.stdin_from in
      exec t ~ectx ~eenv:{ eenv with stdout_to; stderr_to; stdin_from }
    in
    match done_or_deps with
    | Need_more_deps _ as need -> Fiber.return need
    | Done -> exec_list rest ~ectx ~eenv )

let exec ~targets ~context ~env ~rule_loc ~prepared_dependencies t =
  let purpose = Process.Build_job targets in
  let ectx = { targets; purpose; context; rule_loc; prepared_dependencies }
  and eenv =
    { working_dir = Path.root
    ; env
    ; stdout_to = Process.Io.stdout
    ; stderr_to = Process.Io.stderr
    ; stdin_from = Process.Io.stdin
    }
  in
  (* TODO jstaron: Maybe it would be better if this check would be somewhere
    earlier in the processing of action? (Like parsing instead of execution) *)
  ensure_at_most_one_dynamic_run ~ectx t;
  exec t ~ectx ~eenv
