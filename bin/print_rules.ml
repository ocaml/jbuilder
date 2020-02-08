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

let print_rule_makefile ppf (rule : Build_system.Evaluated_rule.t) =
  let action =
    Action.For_shell.Progn
      [ Mkdir (Path.to_string (Path.build rule.dir))
      ; Action.for_shell rule.action
      ]
  in
  let open Fiber.O in
  let+ deps =
    let eval_pred = Build_system.eval_pred in
    Dep.Set.paths rule.deps ~eval_pred
  in
  Format.fprintf ppf
    "@[<hov 2>@{<makefile-stuff>%a:%t@}@]@,@<0>\t@{<makefile-action>%a@}@,@,"
    (Format.pp_print_list ~pp_sep:Format.pp_print_space (fun ppf p ->
         Format.pp_print_string ppf (Path.to_string p)))
    (List.map ~f:Path.build (Path.Build.Set.to_list rule.targets))
    (fun ppf ->
      Path.Set.iter deps ~f:(fun dep ->
          Format.fprintf ppf "@ %s" (Path.to_string dep)))
    Pp.render_ignore_tags (Action_to_sh.pp action)

let print_rule_sexp ppf (rule : Build_system.Evaluated_rule.t) =
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
                 ( Path.Build.Set.to_list rule.targets
                 |> Path.set_of_build_paths_list ) )
           ]
         ; ( match rule.context with
           | None -> []
           | Some c -> [ ("context", Dune.Context_name.encode c.name) ] )
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

  let print_rule syntax ppf rule =
    match syntax with
    | Makefile -> print_rule_makefile ppf rule
    | Sexp -> Fiber.return (print_rule_sexp ppf rule)

  let print_rules syntax ppf rules : unit Fiber.t =
    Dune_lang.Deprecated.prepare_formatter ppf;
    Format.pp_open_vbox ppf 0;
    let open Fiber.O in
    let+ () =
      Fiber.sequential_iter rules ~f:(fun rule -> print_rule syntax ppf rule)
    in
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
  and+ targets = Arg.(value & pos_all string [] & Arg.info [] ~docv:"TARGET") in
  let out = Option.map ~f:Path.of_string out in
  let targets = List.map ~f:Arg.Dep.file targets in
  Common.set_common common ~targets;
  Scheduler.go ~common (fun () ->
      let open Fiber.O in
      let* setup = Import.Main.setup common ~external_lib_deps_mode:true in
      let request =
        match targets with
        | [] ->
          Build_system.all_targets ()
          |> Path.Build.Set.fold ~init:[] ~f:(fun p acc -> Path.build p :: acc)
          |> Build.paths
        | _ -> Target.resolve_targets_exn common setup targets |> Target.request
      in
      let* rules = Build_system.evaluate_rules ~request ~recursive in
      let print oc =
        let ppf = Format.formatter_of_out_channel oc in
        Syntax.print_rules syntax ppf rules
      in
      match out with
      | None -> print stdout
      | Some fn -> Io.with_file_out fn ~f:print)

let command = (term, info)
