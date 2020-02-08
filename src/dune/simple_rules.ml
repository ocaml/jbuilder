open! Stdune
open Import
open Dune_file
open! No_io
module SC = Super_context

module Alias_rules = struct
  let stamp ~deps ~action ~extra_bindings =
    ( "user-alias"
    , Bindings.map ~f:Dep_conf.remove_locs deps
    , Option.map ~f:Action_unexpanded.remove_locs action
    , Option.map extra_bindings ~f:Pform.Map.to_stamp )

  let add sctx ~alias ~stamp ~loc ?(locks = []) build =
    let dir = Alias.dir alias in
    SC.add_alias_action sctx alias ~dir ~loc ~locks ~stamp build

  let add_empty sctx ~loc ~alias ~stamp =
    let action = Build.With_targets.return Action.empty in
    add sctx ~loc ~alias ~stamp action
end

let interpret_locks ~expander = List.map ~f:(Expander.expand_path expander)

let dep_bindings ~extra_bindings deps =
  let base = Pform.Map.of_bindings deps in
  match extra_bindings with
  | Some bindings -> Pform.Map.superpose base bindings
  | None -> base

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

let rule_kind ~(rule : Rule.t) ~(action : Action.t Build.With_targets.t) =
  match rule.alias with
  | None -> No_alias
  | Some alias -> (
    match action.targets |> Path.Build.Set.choose with
    | None -> Alias_only alias
    | Some target -> Alias_with_targets (alias, target) )

let add_user_rule sctx ~dir ~(rule : Rule.t) ~action ~expander =
  SC.add_rule_get_targets
    sctx
    (* user rules may have extra requirements, in which case they will be
       specified as a part of rule.deps, which will be correctly taken care of
       by the build description *)
    ~sandbox:Sandbox_config.no_special_requirements ~dir ~mode:rule.mode
    ~loc:rule.loc
    ~locks:(interpret_locks ~expander rule.locks)
    action

let user_rule sctx ?extra_bindings ~dir ~expander (rule : Rule.t) =
  match Expander.eval_blang expander rule.enabled_if with
  | false ->
    Option.iter rule.alias ~f:(fun name ->
        let alias = Alias.make ~dir name in
        let action = Some (snd rule.action) in
        let stamp = Alias_rules.stamp ~deps:rule.deps ~action ~extra_bindings in
        Alias_rules.add_empty sctx ~alias ~loc:(Some rule.loc) ~stamp);
    Path.Build.Set.empty
  | true -> (
    let targets : Expander.Targets.t =
      match rule.targets with
      | Infer -> Infer
      | Static { targets; multiplicity } ->
        let targets =
          List.concat_map targets ~f:(fun target ->
              let error_loc = String_with_vars.loc target in
              ( match multiplicity with
              | One ->
                [ Expander.expand expander ~mode:Single ~template:target ]
              | Multiple -> Expander.expand expander ~mode:Many ~template:target
              )
              |> List.map ~f:(check_filename ~dir ~error_loc))
        in
        Expander.Targets.Static { multiplicity; targets }
    in
    let bindings = dep_bindings ~extra_bindings rule.deps in
    let expander = Expander.add_bindings expander ~bindings in
    let action =
      SC.Deps.interpret_named sctx ~expander rule.deps
      |> SC.Action.run sctx (snd rule.action) ~loc:(fst rule.action) ~expander
           ~dep_kind:Required ~targets ~targets_dir:dir
    in
    match rule_kind ~rule ~action with
    | No_alias -> add_user_rule sctx ~dir ~rule ~action ~expander
    | Alias_with_targets (alias, alias_target) ->
      let () =
        let alias = Alias.make alias ~dir in
        Path.Set.singleton (Path.build alias_target)
        |> Rules.Produce.Alias.add_deps alias
      in
      add_user_rule sctx ~dir ~rule ~action ~expander
    | Alias_only name ->
      let alias = Alias.make ~dir name in
      let stamp =
        let action = Some (snd rule.action) in
        Alias_rules.stamp ~deps:rule.deps ~extra_bindings ~action
      in
      Alias_rules.add sctx ~alias ~stamp ~loc:(Some rule.loc) action;
      Path.Build.Set.empty )

let copy_files sctx ~dir ~expander ~src_dir (def : Copy_files.t) =
  let loc = String_with_vars.loc def.glob in
  let glob_in_src =
    let src_glob = Expander.expand_str expander def.glob in
    Path.Source.relative src_dir src_glob ~error_loc:loc
  in
  let since = (1, 3) in
  if
    def.syntax_version < since
    && not (Path.Source.is_descendant glob_in_src ~of_:src_dir)
  then
    Dune_lang.Syntax.Error.since loc Stanza.syntax since
      ~what:
        (sprintf "%s is not a sub-directory of %s. This"
           (Path.Source.to_string_maybe_quoted glob_in_src)
           (Path.Source.to_string_maybe_quoted src_dir));
  let src_in_src = Path.Source.parent_exn glob_in_src in
  let pred =
    Path.Source.basename glob_in_src
    |> Glob.of_string_exn (String_with_vars.loc def.glob)
    |> Glob.to_pred
  in
  if not (File_tree.dir_exists src_in_src) then
    User_error.raise ~loc
      [ Pp.textf "Cannot find directory: %s" (Path.Source.to_string src_in_src)
      ];
  if Path.Source.equal src_in_src src_dir then
    User_error.raise ~loc
      [ Pp.textf
          "Cannot copy files onto themselves. The format is <dir>/<glob> where \
           <dir> is not the current directory."
      ];
  (* add rules *)
  let src_in_build =
    let context = Context.DB.get dir in
    Path.Build.append_source context.build_dir src_in_src
  in
  let open Fiber.O in
  let+ files =
    Build_system.eval_pred
      (File_selector.create ~dir:(Path.build src_in_build) pred)
  in
  Path.Set.map files ~f:(fun file_src ->
      let basename = Path.basename file_src in
      let file_dst = Path.Build.relative dir basename in
      SC.add_rule sctx ~loc ~dir
        (( if def.add_line_directive then
           Build.copy_and_add_line_directive
         else
           Build.copy )
           ~src:file_src ~dst:file_dst);
      Path.build file_dst)

let alias sctx ?extra_bindings ~dir ~expander (alias_conf : Alias_conf.t) =
  let alias = Alias.make ~dir alias_conf.name in
  let stamp =
    let action = Option.map ~f:snd alias_conf.action in
    Alias_rules.stamp ~deps:alias_conf.deps ~extra_bindings ~action
  in
  let loc = Some alias_conf.loc in
  match Expander.eval_blang expander alias_conf.enabled_if with
  | false -> Alias_rules.add_empty sctx ~loc ~alias ~stamp
  | true ->
    let locks = interpret_locks ~expander alias_conf.locks in
    let action =
      SC.Deps.interpret_named sctx ~expander alias_conf.deps
      |>
      match alias_conf.action with
      | None ->
        fun x ->
          Build.with_no_targets
            (let open Build.O in
            let+ (_ : Path.t Bindings.t) = x in
            Action.empty)
      | Some (loc, action) ->
        let bindings = dep_bindings ~extra_bindings alias_conf.deps in
        let expander = Expander.add_bindings expander ~bindings in
        SC.Action.run sctx action ~loc ~expander ~dep_kind:Required
          ~targets:(Forbidden "aliases") ~targets_dir:dir
    in
    Alias_rules.add sctx ~loc ~stamp ~locks action ~alias
