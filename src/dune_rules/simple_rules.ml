open! Dune_engine
open! Stdune
open Import
open Dune_file
open! No_io
module SC = Super_context
open Memo.Build.O

module Alias_rules = struct
  let stamp ~deps ~action ~extra_bindings =
    ( "user-alias"
    , Bindings.map ~f:Dep_conf.remove_locs deps
    , Option.map ~f:Action_unexpanded.remove_locs action
    , Option.map extra_bindings ~f:Pform.Map.to_list )

  let add sctx ~alias ~stamp ~loc ~locks build =
    let dir = Alias.dir alias in
    SC.add_alias_action sctx alias ~dir ~loc ~locks ~stamp build

  let add_empty sctx ~loc ~alias ~stamp =
    let action = Action_builder.With_targets.return Action.empty in
    add sctx ~loc ~alias ~stamp action ~locks:[]
end

let interpret_locks ~expander =
  List.map ~f:(Expander.Static.expand_path expander)

let check_filename =
  let not_in_dir ~error_loc s =
    User_error.raise ~loc:error_loc
      [ Pp.textf "%s does not denote a file in the current directory" s ]
  in
  fun ~error_loc ~dir fn ->
    match fn with
    | Value.String ("." | "..") ->
      User_error.raise ~loc:error_loc
        [ Pp.text "'.' and '..' are not valid filenames" ]
    | String s ->
      if Filename.dirname s <> Filename.current_dir_name then
        not_in_dir ~error_loc s;
      Path.Build.relative ~error_loc dir s
    | Path p ->
      if
        Option.compare Path.compare (Path.parent p) (Some (Path.build dir))
        <> Eq
      then
        not_in_dir ~error_loc (Path.to_string p);
      Path.as_in_build_dir_exn p
    | Dir p -> not_in_dir ~error_loc (Path.to_string p)

type rule_kind =
  | Alias_only of Alias.Name.t
  | Alias_with_targets of Alias.Name.t * Path.Build.t
  | No_alias

let rule_kind ~(rule : Rule.t)
    ~(action : Action.t Action_builder.With_targets.t) =
  match rule.alias with
  | None -> No_alias
  | Some alias -> (
    match action.targets |> Path.Build.Set.choose with
    | None -> Alias_only alias
    | Some target -> Alias_with_targets (alias, target))

let add_user_rule sctx ~dir ~(rule : Rule.t) ~action ~expander =
  SC.add_rule_get_targets
    sctx
    (* user rules may have extra requirements, in which case they will be
       specified as a part of rule.deps, which will be correctly taken care of
       by the action builder *)
    ~sandbox:Sandbox_config.no_special_requirements ~dir ~mode:rule.mode
    ~loc:rule.loc
    ~locks:(interpret_locks ~expander rule.locks)
    action

let user_rule sctx ?extra_bindings ~dir ~expander (rule : Rule.t) =
  match Expander.eval_blang expander rule.enabled_if with
  | false -> (
    match rule.alias with
    | None -> Memo.Build.return Path.Build.Set.empty
    | Some name ->
      let alias = Alias.make ~dir name in
      let action = Some (snd rule.action) in
      let stamp = Alias_rules.stamp ~deps:rule.deps ~action ~extra_bindings in
      let+ () = Alias_rules.add_empty sctx ~alias ~loc:(Some rule.loc) ~stamp in
      Path.Build.Set.empty)
  | true -> (
    let targets : Targets.Or_forbidden.t =
      Targets
        (match rule.targets with
        | Infer -> Infer
        | Static { targets; multiplicity } ->
          let targets =
            List.concat_map targets ~f:(fun target ->
                let error_loc = String_with_vars.loc target in
                (match multiplicity with
                | One -> [ Expander.Static.expand expander ~mode:Single target ]
                | Multiple -> Expander.Static.expand expander ~mode:Many target)
                |> List.map ~f:(check_filename ~dir ~error_loc))
          in
          Static { multiplicity; targets })
    in
    let expander =
      match extra_bindings with
      | None -> expander
      | Some bindings -> Expander.add_bindings expander ~bindings
    in
    let action =
      Action_unexpanded.expand (snd rule.action) ~loc:(fst rule.action)
        ~expander ~deps:rule.deps ~targets ~targets_dir:dir
    in
    match rule_kind ~rule ~action with
    | No_alias -> add_user_rule sctx ~dir ~rule ~action ~expander
    | Alias_with_targets (alias, alias_target) ->
      let () =
        let alias = Alias.make alias ~dir in
        Path.Set.singleton (Path.build alias_target)
        |> Rules.Produce.Alias.add_static_deps alias
      in
      add_user_rule sctx ~dir ~rule ~action ~expander
    | Alias_only name ->
      let alias = Alias.make ~dir name in
      let stamp =
        let action = Some (snd rule.action) in
        Alias_rules.stamp ~deps:rule.deps ~extra_bindings ~action
      in
      let locks = interpret_locks ~expander rule.locks in
      let+ () =
        Alias_rules.add sctx ~alias ~stamp ~loc:(Some rule.loc) action ~locks
      in
      Path.Build.Set.empty)

let copy_files sctx ~dir ~expander ~src_dir (def : Copy_files.t) =
  let loc = String_with_vars.loc def.files in
  let glob_in_src =
    let src_glob = Expander.Static.expand_str expander def.files in
    if Filename.is_relative src_glob then
      Path.Source.relative src_dir src_glob ~error_loc:loc |> Path.source
    else
      let since = (2, 7) in
      if def.syntax_version < since then
        Dune_lang.Syntax.Error.since loc Stanza.syntax since
          ~what:(sprintf "%s is an absolute path. This" src_glob);
      Path.external_ (Path.External.of_string src_glob)
  in
  let since = (1, 3) in
  if
    def.syntax_version < since
    && not (Path.is_descendant glob_in_src ~of_:(Path.source src_dir))
  then
    Dune_lang.Syntax.Error.since loc Stanza.syntax since
      ~what:
        (sprintf "%s is not a sub-directory of %s. This"
           (Path.to_string_maybe_quoted glob_in_src)
           (Path.Source.to_string_maybe_quoted src_dir));
  let src_in_src = Path.parent_exn glob_in_src in
  let pred =
    Path.basename glob_in_src |> Glob.of_string_exn loc |> Glob.to_pred
  in
  let exists =
    match Path.as_in_source_tree src_in_src with
    | None -> Path.exists src_in_src
    | Some src_in_src -> File_tree.dir_exists src_in_src
  in
  if not exists then
    User_error.raise ~loc
      [ Pp.textf "Cannot find directory: %s" (Path.to_string src_in_src) ];
  if Path.equal src_in_src (Path.source src_dir) then
    User_error.raise ~loc
      [ Pp.textf
          "Cannot copy files onto themselves. The format is <dir>/<glob> where \
           <dir> is not the current directory."
      ];
  (* add rules *)
  let src_in_build =
    match Path.as_in_source_tree src_in_src with
    | None -> src_in_src
    | Some src_in_src ->
      let context = Context.DB.get dir in
      Path.Build.append_source context.build_dir src_in_src |> Path.build
  in
  let* files =
    Build_system.eval_pred (File_selector.create ~dir:src_in_build pred)
  in
  (* CR-someday amokhov: We currently traverse the set [files] twice: first, to
     add the corresponding rules, and then to convert the files to [targets]. To
     do only one traversal we need [Memo.Build.parallel_map_set]. *)
  let+ () =
    Memo.Build.parallel_iter_set
      (module Path.Set)
      files
      ~f:(fun file_src ->
        let basename = Path.basename file_src in
        let file_dst = Path.Build.relative dir basename in
        SC.add_rule sctx ~loc ~dir ~mode:def.mode
          ((if def.add_line_directive then
             Action_builder.copy_and_add_line_directive
           else
             Action_builder.copy)
             ~src:file_src ~dst:file_dst))
  in
  let targets =
    Path.Set.map files ~f:(fun file_src ->
        let basename = Path.basename file_src in
        let file_dst = Path.Build.relative dir basename in
        Path.build file_dst)
  in
  Option.iter def.alias ~f:(fun alias ->
      let alias = Alias.make alias ~dir in
      Rules.Produce.Alias.add_static_deps alias targets);
  targets

let copy_files sctx ~dir ~expander ~src_dir (def : Copy_files.t) =
  if Expander.eval_blang expander def.enabled_if then
    copy_files sctx ~dir ~expander ~src_dir def
  else
    Memo.Build.return Path.Set.empty

let alias sctx ?extra_bindings ~dir ~expander (alias_conf : Alias_conf.t) =
  let alias = Alias.make ~dir alias_conf.name in
  let stamp =
    let action = Option.map ~f:snd alias_conf.action in
    Alias_rules.stamp ~deps:alias_conf.deps ~extra_bindings ~action
  in
  let loc = Some alias_conf.loc in
  let check_before_2_9 () =
    if
      Dune_project.dune_version @@ Scope.project @@ Expander.scope expander
      < (2, 9)
    then
      try ignore (Expander.eval_blang expander alias_conf.enabled_if) with
      | User_error.E _ ->
        User_error.raise ?loc
          [ Pp.textf
              "Extended enabled_if is not compatible with dune lang < (2, 9)."
          ]
  in
  let enabled_if = Expander.eval_blang_partial expander alias_conf.enabled_if in
  let locks = interpret_locks ~expander alias_conf.locks in
  match Action_builder.static_eval enabled_if with
  | Some (false, deps) ->
    check_before_2_9 ();
    let action =
      let open Action_builder.With_targets.O in
      let+ () = Action_builder.with_no_targets deps in
      Action.empty
    in
    Alias_rules.add sctx ~loc ~stamp ~locks:[] action ~alias
  | None -> (
    check_before_2_9 ();
    match alias_conf.action with
    | None ->
      let builder, _expander = Dep_conf_eval.named ~expander alias_conf.deps in
      let builder =
        let open Action_builder.O in
        let+ b = enabled_if in
        if b then
          builder
        else
          Action_builder.return ()
      in
      let builder = Action_builder.Expert.action_builder builder in
      Rules.Produce.Alias.add_deps alias ?loc builder
    | Some (action_loc, _) ->
      let action =
        Action_builder.fail
          { fail =
              User_error.raise ~loc:action_loc
                [ Pp.textf
                    "Extended enabled_if is not compatible with deprecrated \
                     action field."
                ]
          }
      in
      let action = Action_builder.with_no_targets action in
      Alias_rules.add sctx ~loc ~stamp ~locks action ~alias)
  | Some (true, deps) -> (
    check_before_2_9 ();
    match alias_conf.action with
    | None ->
      let builder, _expander = Dep_conf_eval.named ~expander alias_conf.deps in
      Rules.Produce.Alias.add_deps alias ?loc builder;
      Memo.Build.return ()
    | Some (action_loc, action) ->
      let action =
        let builder, expander = Dep_conf_eval.named ~expander alias_conf.deps in
        let builder = Action_builder.O.( >>> ) deps builder in
        let open Action_builder.With_targets.O in
        let+ () = Action_builder.with_no_targets builder
        and+ action =
          let expander =
            match extra_bindings with
            | None -> expander
            | Some bindings -> Expander.add_bindings expander ~bindings
          in
          Action_unexpanded.expand action ~loc:action_loc ~expander
            ~deps:alias_conf.deps ~targets:(Forbidden "aliases")
            ~targets_dir:dir
        in
        action
      in
      Alias_rules.add sctx ~loc ~stamp ~locks action ~alias)
