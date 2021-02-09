open Stdune
open Import

let doc = "Print the environment of a directory"

let man =
  [ `S "DESCRIPTION"
  ; `P {|$(b,dune printenv DIR) prints the environment of a directory|}
  ; `Blocks Common.help_secs
  ]

let info = Term.info "printenv" ~doc ~man

let dump sctx ~dir =
  let open Action_builder.O in
  let+ env = Super_context.dump_env sctx ~dir in
  ((Super_context.context sctx).name, env)

let pp ppf ~fields sexps =
  let fields = String.Set.of_list fields in
  List.iter sexps ~f:(fun sexp ->
      let do_print =
        String.Set.is_empty fields
        ||
        match sexp with
        | Dune_lang.List (Atom (A name) :: _) -> String.Set.mem fields name
        | _ -> false
      in
      if do_print then
        let version =
          Dune_lang.Syntax.greatest_supported_version Dune_engine.Stanza.syntax
        in
        Dune_lang.Ast.add_loc sexp ~loc:Loc.none
        |> Dune_lang.Cst.concrete |> List.singleton
        |> Dune_engine.Format_dune_lang.pp_top_sexps ~version
        |> Format.fprintf ppf "%a@?" Pp.to_fmt)

let term =
  let+ common = Common.term
  and+ dir = Arg.(value & pos 0 dir "" & info [] ~docv:"PATH")
  and+ fields =
    Arg.(
      value & opt_all string []
      & info [ "field" ] ~docv:"FIELD"
          ~doc:
            "Only print this field. This option can be repeated multiple times \
             to print multiple fields.")
  in
  Common.set_common common ~targets:[];
  Scheduler.go ~common (fun () ->
      let open Fiber.O in
      let* setup = Memo.Build.run (Import.Main.setup common) in
      let dir = Path.of_string dir in
      let checked = Util.check_path setup.workspace.contexts dir in
      let request =
        Action_builder.all
          (match checked with
          | In_build_dir (ctx, _) ->
            let sctx =
              Dune_engine.Context_name.Map.find_exn setup.scontexts ctx.name
            in
            [ dump sctx ~dir:(Path.as_in_build_dir_exn dir) ]
          | In_source_dir dir ->
            Dune_engine.Context_name.Map.values setup.scontexts
            |> List.map ~f:(fun sctx ->
                   let dir =
                     Path.Build.append_source
                       (Super_context.context sctx).build_dir dir
                   in
                   dump sctx ~dir)
          | External _ ->
            User_error.raise
              [ Pp.text "Environment is not defined for external paths" ]
          | In_install_dir _ ->
            User_error.raise
              [ Pp.text "Environment is not defined in install dirs" ])
      in
      Memo.Build.run (Build_system.do_build ~request) >>| function
      | [ (_, env) ] -> Format.printf "%a" (pp ~fields) env
      | l ->
        List.iter l ~f:(fun (name, env) ->
            Format.printf "@[<v2>Environment for context %s:@,%a@]@."
              (Dune_engine.Context_name.to_string name)
              (pp ~fields) env))

let command = (term, info)
