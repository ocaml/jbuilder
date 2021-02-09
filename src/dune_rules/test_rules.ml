open! Dune_engine
open Import
open! No_io

let rules (t : Dune_file.Tests.t) ~sctx ~dir ~scope ~expander ~dir_contents =
  let test_kind (loc, name) =
    let files = Dir_contents.text_files dir_contents in
    let expected_basename = name ^ ".expected" in
    if String.Set.mem files expected_basename then
      `Expect
        { Diff.file1 = String_with_vars.make_text loc expected_basename
        ; file2 = String_with_vars.make_text loc (name ^ ".output")
        ; optional = false
        ; mode = Text
        }
    else
      `Regular
  in
  List.iter t.exes.names ~f:(fun (loc, s) ->
      let test_pform = Pform.Var Test in
      let run_action =
        match t.action with
        | Some a -> a
        | None ->
          Action_unexpanded.Run (String_with_vars.make_pform loc test_pform, [])
      in
      let extra_bindings =
        let test_exe = s ^ ".exe" in
        let test_exe_path =
          Expander.map_exe expander (Path.relative (Path.build dir) test_exe)
        in
        Pform.Map.singleton test_pform [ Value.Path test_exe_path ]
      in
      let add_alias ~loc ~action ~locks =
        let alias =
          { Dune_file.Alias_conf.name = Alias.Name.runtest
          ; locks
          ; package = t.package
          ; deps = t.deps
          ; action = Some (loc, action)
          ; enabled_if = t.enabled_if
          ; loc
          }
        in
        Simple_rules.alias sctx ~extra_bindings ~dir ~expander alias
      in
      match test_kind (loc, s) with
      | `Regular -> add_alias ~loc ~action:run_action ~locks:[]
      | `Expect diff ->
        let rule =
          { Dune_file.Rule.targets = Infer
          ; deps = Bindings.empty
          ; action =
              ( loc
              , Action_unexpanded.Redirect_out (Stdout, diff.file2, run_action)
              )
          ; mode = Standard
          ; locks = t.locks
          ; loc
          ; enabled_if = t.enabled_if
          ; alias = None
          ; package = t.package
          }
        in
        add_alias ~loc ~action:(Diff diff) ~locks:t.locks;
        ignore
          (Simple_rules.user_rule sctx rule ~extra_bindings ~dir ~expander
            : Path.Build.Set.t));
  Exe_rules.rules t.exes ~sctx ~dir ~scope ~expander ~dir_contents
