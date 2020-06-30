open! Stdune
open Import
open Fiber.O

let () = Inline_tests.linkme

type workspace =
  { contexts : Context.t list
  ; conf : Dune_load.conf
  ; env : Env.t
  }

type build_system =
  { workspace : workspace
  ; scontexts : Super_context.t Context_name.Map.t
  }

let package_install_file ~findlib_toolchain w pkg =
  match Package.Name.Map.find w.conf.packages pkg with
  | None -> Error ()
  | Some p ->
    Ok
      (Path.Source.relative p.path
         (Utils.install_file ~package:p.name ~findlib_toolchain))

let setup_env ~capture_outputs =
  let env =
    let env =
      if
        (not capture_outputs)
        || not (Lazy.force Ansi_color.stderr_supports_color)
      then
        Env.initial
      else
        Colors.setup_env_for_colors Env.initial
    in
    Env.add env ~var:"INSIDE_DUNE" ~value:"1"
  in
  Memo.Run.Fdecl.set Global.env env;
  env

let scan_workspace ?workspace_file ?x ?(capture_outputs = true) ?profile
    ~ancestor_vcs () =
  let env = setup_env ~capture_outputs in
  let conf = Dune_load.load ~ancestor_vcs in
  let () =
    let path : Path.t option =
      match workspace_file with
      | None ->
        let p = Path.of_string Workspace.filename in
        Option.some_if (Path.exists p) p
      | Some p ->
        if not (Path.exists p) then
          User_error.raise
            [ Pp.textf "Workspace file %s does not exist"
                (Path.to_string_maybe_quoted p)
            ];
        Some p
    in
    Workspace.init ?x ?profile ?path ()
  in
  let+ contexts = Context.DB.all () in
  List.iter contexts ~f:(fun (ctx : Context.t) ->
      let open Pp.O in
      Log.info
        [ Pp.box ~indent:1
            (Pp.text "Dune context:" ++ Pp.cut ++ Dyn.pp (Context.to_dyn ctx))
        ]);
  { contexts; conf; env }

let init_build_system ?only_packages ~sandboxing_preference ?caching w =
  Build_system.reset ();
  Build_system.init ~sandboxing_preference ~contexts:w.contexts ?caching;
  let+ scontexts = Gen_rules.gen w.conf ~contexts:w.contexts ?only_packages in
  { workspace = w; scontexts }

let auto_concurrency =
  let v = ref None in
  fun () ->
    match !v with
    | Some n -> Fiber.return n
    | None ->
      let+ n =
        if Sys.win32 then
          match Env.get Env.initial "NUMBER_OF_PROCESSORS" with
          | None -> Fiber.return 1
          | Some s -> (
            match int_of_string s with
            | exception _ -> Fiber.return 1
            | n -> Fiber.return n )
        else
          let commands =
            [ ("nproc", [])
            ; ("getconf", [ "_NPROCESSORS_ONLN" ])
            ; ("getconf", [ "NPROCESSORS_ONLN" ])
            ]
          in
          let rec loop = function
            | [] -> Fiber.return 1
            | (prog, args) :: rest -> (
              match Bin.which ~path:(Env.path Env.initial) prog with
              | None -> loop rest
              | Some prog -> (
                let* result =
                  Process.run_capture (Accept Predicate_lang.any) prog args
                    ~env:Env.initial
                    ~stderr_to:(Process.Io.file Config.dev_null Process.Io.Out)
                in
                match result with
                | Error _ -> loop rest
                | Ok s -> (
                  match int_of_string (String.trim s) with
                  | n -> Fiber.return n
                  | exception _ -> loop rest ) ) )
          in
          loop commands
      in
      Log.info [ Pp.textf "Auto-detected concurrency: %d" n ];
      v := Some n;
      n

let set_concurrency (config : Config.t) =
  let+ n =
    match config.concurrency with
    | Fixed n -> Fiber.return n
    | Auto -> auto_concurrency ()
  in
  if n >= 1 then Scheduler.set_concurrency n

let find_context_exn t ~name =
  match List.find t.contexts ~f:(fun c -> Context_name.equal c.name name) with
  | Some ctx -> ctx
  | None ->
    User_error.raise
      [ Pp.textf "Context %S not found!" (Context_name.to_string name) ]
