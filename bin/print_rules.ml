open Stdune
open Import

let doc = "Dump internal rules."

let man =
  [ `S "DESCRIPTION"
  ; `P
      {|Dump Dune internal rules for the given targets.
           If no targets are given, dump all the internal rules.|}
  ; `P
      {|By default the output is a list of S-expressions,
           one S-expression per rule. Each S-expression is of the form:|}
  ; `Pre
      "  ((deps    (<dependencies>))\n\
      \   (targets (<targets>))\n\
      \   (context <context-name>)\n\
      \   (action  <action>))"
  ; `P
      {|$(b,<context-name>) is the context is which the action is executed.
           It is omitted if the action is independent from the context.|}
  ; `P
      {|$(b,<action>) is the action following the same syntax as user actions,
           as described in the manual.|}
  ; `Blocks Common.help_secs
  ]

let info = Term.info "rules" ~doc ~man

module Eval_rules : sig
  module Rule : sig
    type t = private
      { dir : Path.Build.t
      ; deps : Dep.Set.t
      ; expanded_deps : Path.Set.t
      ; targets : Path.Build.Set.t
      ; context : Dune_engine.Build_context.t option
      ; action : Action.t
      }
  end

  val eval :
    recursive:bool -> request:unit Action_builder.t -> Rule.t list Memo.Build.t
end = struct
  open Dune_engine

  module Non_evaluated_rule = struct
    include Rule

    let equal a b = Path.Build.equal (head_target a) (head_target b)

    let hash t = Path.Build.hash (head_target t)
  end

  open Memo.Build.O

  module Rule = struct
    type t =
      { dir : Path.Build.t
      ; deps : Dep.Set.t
      ; expanded_deps : Path.Set.t
      ; targets : Path.Build.Set.t
      ; context : Build_context.t option
      ; action : Action.t
      }

    let head_target t = Path.Build.Set.min_elt t.targets |> Option.value_exn
  end

  module Rule_top_closure = Top_closure.Make (Path.Build.Set) (Memo.Build)

  module rec Expand : sig
    val alias : Alias.t -> Path.Set.t Memo.Build.t

    val deps : Dep.Set.t -> Path.Set.t Memo.Build.t
  end = struct
    let alias =
      let memo =
        Memo.create "expand-alias"
          ~input:(module Alias)
          (fun alias ->
            let* l =
              Build_system.get_alias_definition alias
              >>= Memo.Build.parallel_map ~f:(fun (loc, definition) ->
                      Memo.push_stack_frame
                        (fun () ->
                          Action_builder.run
                            (Build_system.dep_on_alias_definition definition)
                            Lazy
                          >>| snd)
                        ~human_readable_description:(fun () ->
                          Alias.describe alias ~loc))
            in
            let deps = List.fold_left l ~init:Dep.Set.empty ~f:Dep.Set.union in
            Expand.deps deps)
      in
      Memo.exec memo

    let deps deps =
      Memo.Build.parallel_map (Dep.Set.to_list deps) ~f:(fun (dep : Dep.t) ->
          match dep with
          | File p -> Memo.Build.return (Path.Set.singleton p)
          | File_selector g -> Build_system.eval_pred g
          | Alias a -> Expand.alias a
          | Env _
          | Universe
          | Sandbox_config _ ->
            Memo.Build.return Path.Set.empty)
      >>| Path.Set.union_all
  end

  let evaluate_rule =
    let memo =
      Memo.create "evaluate-rule"
        ~input:(module Non_evaluated_rule)
        (fun rule ->
          let* action, deps = Action_builder.run rule.action Lazy in
          let* expanded_deps = Expand.deps deps in
          Memo.Build.return
            { Rule.dir = rule.dir
            ; deps
            ; expanded_deps
            ; targets = rule.targets
            ; context = rule.context
            ; action = action.action
            })
    in
    Memo.exec memo

  let eval ~recursive ~request =
    let rules_of_deps deps =
      Expand.deps deps >>| Path.Set.to_list
      >>= Memo.Build.parallel_map ~f:(fun p ->
              Build_system.get_rule p >>= function
              | None -> Memo.Build.return None
              | Some rule -> evaluate_rule rule >>| Option.some)
      >>| List.filter_map ~f:Fun.id
    in
    let* (), deps = Action_builder.run request Lazy in
    let* root_rules = rules_of_deps deps in
    Rule_top_closure.top_closure root_rules
      ~key:(fun rule -> Rule.head_target rule)
      ~deps:(fun rule ->
        if recursive then
          rules_of_deps rule.deps
        else
          Memo.Build.return [])
    >>| function
    | Ok l -> l
    | Error cycle ->
      User_error.raise
        [ Pp.text "Dependency cycle detected:"
        ; Pp.chain cycle ~f:(fun rule ->
              Pp.verbatim
                (Path.to_string_maybe_quoted
                   (Path.build (Path.Build.Set.choose_exn rule.targets))))
        ]
end

let print_rule_makefile ppf (rule : Eval_rules.Rule.t) =
  let action =
    Action.For_shell.Progn
      [ Mkdir (Path.to_string (Path.build rule.dir))
      ; Action.for_shell rule.action
      ]
  in
  Format.fprintf ppf
    "@[<hov 2>@{<makefile-stuff>%a:%t@}@]@,@<0>\t@{<makefile-action>%a@}@,@,"
    (Format.pp_print_list ~pp_sep:Format.pp_print_space (fun ppf p ->
         Format.pp_print_string ppf (Path.to_string p)))
    (List.map ~f:Path.build (Path.Build.Set.to_list rule.targets))
    (fun ppf ->
      Path.Set.iter rule.expanded_deps ~f:(fun dep ->
          Format.fprintf ppf "@ %s" (Path.to_string dep)))
    Pp.to_fmt (Action_to_sh.pp action)

let print_rule_sexp ppf (rule : Eval_rules.Rule.t) =
  let sexp_of_action action =
    Action.for_shell action |> Action.For_shell.encode
  in
  let paths ps = Dune_lang.Encoder.list Dpath.encode (Path.Set.to_list ps) in
  let sexp =
    Dune_lang.Encoder.record
      (List.concat
         [ [ ("deps", Dep.Set.encode rule.deps)
           ; ( "targets"
             , paths
                 (Path.Build.Set.to_list rule.targets
                 |> Path.set_of_build_paths_list) )
           ]
         ; (match rule.context with
           | None -> []
           | Some c -> [ ("context", Dune_engine.Context_name.encode c.name) ])
         ; [ ("action", sexp_of_action rule.action) ]
         ])
  in
  Format.fprintf ppf "%a@," Dune_lang.Deprecated.pp_split_strings sexp

module Syntax = struct
  type t =
    | Makefile
    | Sexp

  let term =
    let doc = "Output the rules in Makefile syntax." in
    let+ makefile = Arg.(value & flag & info [ "m"; "makefile" ] ~doc) in
    if makefile then
      Makefile
    else
      Sexp

  let print_rule = function
    | Makefile -> print_rule_makefile
    | Sexp -> print_rule_sexp

  let print_rules syntax ppf rules =
    Dune_lang.Deprecated.prepare_formatter ppf;
    Format.pp_open_vbox ppf 0;
    Format.pp_print_list (print_rule syntax) ppf rules;
    Format.pp_print_flush ppf ()
end

let term =
  let+ common = Common.term
  and+ out =
    Arg.(
      value
      & opt (some string) None
      & info [ "o" ] ~docv:"FILE" ~doc:"Output to a file instead of stdout.")
  and+ recursive =
    Arg.(
      value & flag
      & info [ "r"; "recursive" ]
          ~doc:
            "Print all rules needed to build the transitive dependencies of \
             the given targets.")
  and+ syntax = Syntax.term
  and+ targets = Arg.(value & pos_all dep [] & Arg.info [] ~docv:"TARGET") in
  let config = Common.init common in
  let out = Option.map ~f:Path.of_string out in
  Scheduler.go ~common ~config (fun () ->
      let open Fiber.O in
      let* setup = Import.Main.setup () in
      Build_system.run_exn (fun () ->
          let open Memo.Build.O in
          let* request =
            match targets with
            | [] ->
              Build_system.all_targets ()
              >>| Path.Build.Set.fold ~init:[] ~f:(fun p acc ->
                      Path.build p :: acc)
              >>| Action_builder.paths
            | _ ->
              Memo.Build.return
                (Target.interpret_targets (Common.root common) config setup
                   targets)
          in
          let+ rules = Eval_rules.eval ~request ~recursive in
          let print oc =
            let ppf = Format.formatter_of_out_channel oc in
            Syntax.print_rules syntax ppf rules
          in
          match out with
          | None -> print stdout
          | Some fn -> Io.with_file_out fn ~f:print))

let command = (term, info)
