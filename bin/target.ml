open Stdune
module Log = Dune_util.Log
module Context = Dune.Context
module Build = Dune.Build
module Build_system = Dune.Build_system

type t =
  | File of Path.t
  | Alias of Alias.t

type resolve_input =
  | Path of Path.t
  | Dep of Arg.Dep.t

let request targets =
  List.fold_left targets ~init:(Build.return ()) ~f:(fun acc target ->
      let open Build.O in
      acc
      >>>
      match target with
      | File path -> Build.path path
      | Alias { Alias.name; recursive; dir; contexts } ->
        let contexts = List.map ~f:Dune.Context.name contexts in
        ( if recursive then
          Build_system.Alias.dep_rec_multi_contexts
        else
          Build_system.Alias.dep_multi_contexts )
          ~dir ~name ~contexts)

let target_hint (_setup : Dune.Main.build_system) path =
  assert (Path.is_managed path);
  let sub_dir = Option.value ~default:path (Path.parent path) in
  let candidates = Path.Build.Set.to_list (Build_system.all_targets ()) in
  let candidates =
    if Path.is_in_build_dir path then
      List.map ~f:Path.build candidates
    else
      List.map candidates ~f:(fun path ->
          match Path.Build.extract_build_context path with
          | None -> Path.build path
          | Some (_, path) -> Path.source path)
  in
  let candidates =
    (* Only suggest hints for the basename, otherwise it's slow when there are
       lots of files *)
    List.filter_map candidates ~f:(fun path ->
        if Path.equal (Path.parent_exn path) sub_dir then
          Some (Path.to_string path)
        else
          None)
  in
  let candidates = String.Set.of_list candidates |> String.Set.to_list in
  User_message.did_you_mean (Path.to_string path) ~candidates

let resolve_path path ~(setup : Dune.Main.build_system) =
  let checked = Util.check_path setup.workspace.contexts path in
  let can't_build path = Error (target_hint setup path) in
  let as_source_dir src =
    if Dune.File_tree.dir_exists src then
      Some
        [ Alias
            (Alias.in_dir ~name:Dune.Alias.Name.default ~recursive:true
               ~contexts:setup.workspace.contexts path)
        ]
    else
      None
  in
  let build () =
    if Build_system.is_target path then
      Ok [ File path ]
    else
      can't_build path
  in
  match checked with
  | External _ -> Ok [ File path ]
  | In_source_dir src -> (
    match as_source_dir src with
    | Some res -> Ok res
    | None -> (
      match
        List.filter_map setup.workspace.contexts ~f:(fun ctx ->
            let path =
              Path.append_source (Path.build ctx.Context.build_dir) src
            in
            if Build_system.is_target path then
              Some (File path)
            else
              None)
      with
      | [] -> can't_build path
      | l -> Ok l ) )
  | In_build_dir (_ctx, src) -> (
    match as_source_dir src with
    | Some res -> Ok res
    | None -> build () )
  | In_install_dir _ -> build ()

let expand_path common ~(setup : Dune.Main.build_system) ctx sv =
  let sctx =
    Dune.Context_name.Map.find_exn setup.scontexts (Context.name ctx)
  in
  let dir =
    Path.Build.relative ctx.Context.build_dir
      (String.concat ~sep:Filename.dir_sep (Common.root common).to_cwd)
  in
  let expander = Dune.Super_context.expander sctx ~dir in
  let lookup ~f ~dir name =
    f
      (Result.ok_exn
         (Dune.Dir_contents.artifacts (Dune.Dir_contents.get sctx ~dir)))
      name
  in
  let lookup_module =
    lookup ~f:(fun a m ->
        Result.ok_exn (Dune.Ml_sources.Artifacts.lookup_module a m))
  in
  let lookup_library = lookup ~f:Dune.Ml_sources.Artifacts.lookup_library in
  let expander = Dune.Expander.set_lookup_module expander ~lookup_module in
  let expander = Dune.Expander.set_lookup_library expander ~lookup_library in
  Path.relative Path.root
    (Common.prefix_target common (Dune.Expander.expand_str expander sv))

let resolve_alias common ~recursive sv ~(setup : Dune.Main.build_system) =
  match Dune.String_with_vars.text_only sv with
  | Some s ->
    Ok
      [ Alias
          (Alias.of_string common ~recursive s
             ~contexts:setup.workspace.contexts)
      ]
  | None -> Error [ Pp.text "alias cannot contain variables" ]

let resolve_target common ~setup = function
  | Dune.Dep_conf.Alias sv as dep ->
    Result.map_error
      ~f:(fun hints -> (dep, hints))
      (resolve_alias common ~recursive:false sv ~setup)
  | Alias_rec sv as dep ->
    Result.map_error
      ~f:(fun hints -> (dep, hints))
      (resolve_alias common ~recursive:true sv ~setup)
  | File sv as dep ->
    let f ctx =
      let path = expand_path common ~setup ctx sv in
      Result.map_error ~f:(fun hints -> (dep, hints)) (resolve_path path ~setup)
    in
    Result.List.concat_map ~f setup.workspace.contexts
  | dep -> Error (dep, [])

let resolve_targets_mixed common setup user_targets =
  match user_targets with
  | [] -> []
  | _ ->
    let targets =
      List.map user_targets ~f:(function
        | Dep d -> resolve_target common ~setup d
        | Path p ->
          Result.map_error
            ~f:(fun hints -> (Arg.Dep.file (Path.to_string p), hints))
            (resolve_path p ~setup))
    in
    let config = Common.config common in
    if config.display = Verbose then
      Log.info
        [ Pp.text "Actual targets:"
        ; Pp.enumerate
            (List.concat_map targets ~f:(function
              | Ok targets -> targets
              | Error _ -> []))
            ~f:(function
              | File p -> Pp.verbatim (Path.to_string_maybe_quoted p)
              | Alias a -> Alias.pp a)
        ];
    targets

let resolve_targets common (setup : Dune.Main.build_system) user_targets =
  List.map ~f:(fun dep -> Dep dep) user_targets
  |> resolve_targets_mixed common setup

let resolve_targets_exn common setup user_targets =
  resolve_targets common setup user_targets
  |> List.concat_map ~f:(function
       | Error (dep, hints) ->
         User_error.raise
           [ Pp.textf "Don't know how to build %s"
               (Arg.Dep.to_string_maybe_quoted dep)
           ]
           ~hints
       | Ok targets -> targets)
