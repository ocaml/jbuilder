open Import

let flag_of_kind : Ml_kind.t -> _ =
  function
  | Impl -> "--impl"
  | Intf -> "--intf"

let add_diff sctx loc alias ~dir ~input ~output =
  let open Build.O in
  let action = Action.diff input output in
  Super_context.add_alias_action sctx alias ~dir ~loc:(Some loc) ~locks:[]
    ~stamp:input
    (Build.paths [input; output]
     >>>
     Build.action
       ~dir
       ~targets:[]
       action)

let rec subdirs_until_root dir =
  match Path.parent dir with
  | None -> [dir]
  | Some d -> dir :: subdirs_until_root d

let depend_on_existing_paths paths =
  let open Build.O in
  let build_id = Build.arr Fn.id in
  List.fold_left
    ~f:(fun acc path ->
      Build.if_file_exists
        path
        ~then_:(Build.path path)
        ~else_:build_id
      >>>
      acc)
    ~init:build_id
    paths

let depend_on_files ~named dir =
  subdirs_until_root dir
  |> List.concat_map ~f:(fun dir -> List.map named ~f:(Path.relative dir))
  |> depend_on_existing_paths

let formatted = ".formatted"

let gen_rules_output sctx (config : Dune_file.Auto_format.t) ~output_dir =
  assert (formatted = Path.basename output_dir);
  let loc = Dune_file.Auto_format.loc config in
  let dir = Path.parent_exn output_dir in
  let source_dir = Path.drop_build_context_exn dir in
  let alias_formatted = Alias.fmt ~dir:output_dir in
  let resolve_program =
    Super_context.resolve_program ~dir sctx ~loc:(Some loc) in
  let ocamlformat_deps = lazy (
    depend_on_files ~named:[".ocamlformat"; ".ocamlformat-ignore"] source_dir
  ) in
  let setup_formatting file =
    let open Build.O in
    let input_basename = Path.basename file in
    let input = Path.relative dir input_basename in
    let output = Path.relative output_dir input_basename in

    let ocaml kind =
      if Dune_file.Auto_format.includes config Ocaml then
        let exe = resolve_program "ocamlformat" in
        let args =
          let open Arg_spec in
          [ A (flag_of_kind kind)
          ; Dep input
          ; A "--name"
          ; Path file
          ; A "-o"
          ; Target output
          ]
        in
        Some (Lazy.force ocamlformat_deps >>> Build.run ~dir exe args)
      else
        None
    in

    let formatter =
      match Path.basename file, Path.extension file with
      | _, ".ml" -> ocaml Impl
      | _, ".mli" -> ocaml Intf
      | _, ".re"
      | _, ".rei" when Dune_file.Auto_format.includes config Reason ->
        let exe = resolve_program "refmt" in
        let args = [Arg_spec.Dep input] in
        Some (Build.run ~dir ~stdout_to:output exe args)
      | "dune", _ when Dune_file.Auto_format.includes config Dune ->
        let exe = resolve_program "dune" in
        let args = [Arg_spec.A "format-dune-file"; Dep input] in
        Some (Build.run ~dir ~stdout_to:output exe args)
      | _ -> None
    in

    Option.iter formatter ~f:(fun arr ->
      Super_context.add_rule sctx ~mode:Standard ~loc ~dir arr;
      add_diff sctx loc alias_formatted ~dir ~input ~output)
  in
  File_tree.files_of (Super_context.file_tree sctx) source_dir
  |> Path.Set.iter ~f:setup_formatting;
  Build_system.Alias.add_deps alias_formatted Dep.Set.empty

let gen_rules ~dir =
  let output_dir = Path.relative dir formatted in
  let alias = Alias.fmt ~dir in
  let alias_formatted = Alias.fmt ~dir:output_dir in
  Dep.alias alias_formatted
  |> Dep.Set.singleton
  |> Build_system.Alias.add_deps alias
