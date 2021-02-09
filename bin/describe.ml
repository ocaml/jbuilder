open Stdune
open Import

(* This command is not yet versioned, but some people are using it in
   non-released tools. If you change the format of the output, please contact:

   - rotor people for "describe workspace"

   - duniverse people for "describe opam-files" *)

let doc = "Describe the workspace."

let man =
  [ `S "DESCRIPTION"
  ; `P
      {|Describe what is in the current workspace in either human or
        machine readable form.

        By default, this command output a human readable description of
        the current workspace. This output is aimed at human and is not
        suitable for machine processing. In particular, it is not versioned.

        If you want to interpret the output of this command from a program,
        you must use the $(b,--format) option to specify a machine readable
        format as well as the $(b,--lang) option to get a stable output.|}
  ; `Blocks Common.help_secs
  ]

let info = Term.info "describe" ~doc ~man

(* Crawl the workspace to get all the data *)
module Crawl = struct
  open Dune_rules
  open Dune_engine

  let uid_of_library lib =
    Digest.generic
      (Lib.name lib, Path.to_string (Lib_info.src_dir (Lib.info lib)))
    |> Digest.to_string

  let dyn_path p = Dyn.String (Path.to_string p)

  let modules ~obj_dir =
    Modules.fold_no_vlib ~init:[] ~f:(fun m acc ->
        let source ml_kind =
          Dyn.Encoder.option dyn_path
            (Option.map (Module.source m ~ml_kind) ~f:Module.File.path)
        in
        let cmt ml_kind =
          Dyn.Encoder.option dyn_path
            (Obj_dir.Module.cmt_file obj_dir m ~ml_kind)
        in
        Dyn.Encoder.record
          [ ("name", Module_name.to_dyn (Module.name m))
          ; ("impl", source Impl)
          ; ("intf", source Intf)
          ; ("cmt", cmt Impl)
          ; ("cmti", cmt Intf)
          ]
        :: acc)

  let executables sctx ~project ~dir exes =
    let modules_, obj_dir =
      let first_exe = snd (List.hd exes.Dune_file.Executables.names) in
      Dir_contents.get sctx ~dir |> Dir_contents.ocaml
      |> Ml_sources.modules_and_obj_dir ~for_:(Exe { first_exe })
    in
    let obj_dir = Obj_dir.of_local obj_dir in
    let modules_ = modules ~obj_dir modules_ in
    let scope = Super_context.find_scope_by_project sctx project in
    let compile_info = Exe_rules.compile_info ~scope exes in
    match Lib.Compile.direct_requires compile_info with
    | Error _ -> None
    | Ok libs ->
      let include_dirs = Obj_dir.all_cmis obj_dir in
      Some
        (Dyn.Variant
           ( "executables"
           , [ Dyn.Encoder.record
                 [ ( "names"
                   , List
                       (List.map
                          ~f:(fun (_, name) -> Dyn.String name)
                          exes.names) )
                 ; ( "requires"
                   , Dyn.Encoder.(list string) (List.map ~f:uid_of_library libs)
                   )
                 ; ("modules", List modules_)
                 ; ("include_dirs", Dyn.Encoder.list dyn_path include_dirs)
                 ]
             ] ))

  let library sctx lib =
    match Lib.requires lib with
    | Error _ -> None
    | Ok requires ->
      let name = Lib.name lib in
      let info = Lib.info lib in
      let src_dir = Lib_info.src_dir info in
      let obj_dir = Lib_info.obj_dir info in
      let modules_ =
        if Lib.is_local lib then
          Dir_contents.get sctx ~dir:(Path.as_in_build_dir_exn src_dir)
          |> Dir_contents.ocaml
          |> Ml_sources.modules ~for_:(Library name)
          |> modules ~obj_dir
        else
          []
      in
      let include_dirs = Obj_dir.all_cmis obj_dir in
      Some
        (let open Dyn.Encoder in
        Dyn.Variant
          ( "library"
          , [ Dyn.Encoder.record
                [ ("name", Lib_name.to_dyn name)
                ; ("uid", String (uid_of_library lib))
                ; ("local", Bool (Lib.is_local lib))
                ; ( "requires"
                  , (list string) (List.map requires ~f:uid_of_library) )
                ; ("source_dir", dyn_path src_dir)
                ; ("modules", List modules_)
                ; ("include_dirs", (list dyn_path) include_dirs)
                ]
            ] ))

  let workspace { Dune_rules.Main.workspace; scontexts } (context : Context.t) =
    let sctx = Context_name.Map.find_exn scontexts context.name in
    let libs =
      List.fold_left workspace.conf.projects ~init:Lib.Set.empty
        ~f:(fun libs project ->
          Super_context.find_scope_by_project sctx project
          |> Scope.libs |> Lib.DB.all |> Lib.Set.union libs)
    in
    let libs =
      Lib.Set.fold libs ~init:libs ~f:(fun lib libs ->
          match Lib.requires lib with
          | Error _ -> libs
          | Ok requires -> Lib.Set.of_list requires |> Lib.Set.union libs)
      |> Lib.Set.to_list
      |> List.filter_map ~f:(library sctx)
    in
    let open Memo.Build.O in
    let+ dune_files =
      Dune_load.Dune_files.eval workspace.conf.dune_files ~context
    in
    let exes_and_libs =
      Dune_load.Dune_file.fold_stanzas dune_files ~init:libs
        ~f:(fun dune_file stanza accu ->
          let dir = Path.Build.append_source context.build_dir dune_file.dir in
          match stanza with
          | Dune_file.Executables exes -> (
            match executables sctx ~project:dune_file.project ~dir exes with
            | None -> accu
            | Some exes -> exes :: accu)
          | _ -> accu)
    in
    Dyn.List exes_and_libs
end

module Opam_files = struct
  let get () =
    let project =
      Dune_engine.File_tree.root () |> Dune_engine.File_tree.Dir.project
    in
    let packages =
      Dune_project.packages project |> Dune_engine.Package.Name.Map.values
    in
    Dyn.List
      (List.map packages ~f:(fun pkg ->
           let opam_file = Path.source (Dune_engine.Package.opam_file pkg) in
           let contents =
             if not (Dune_project.generate_opam_files project) then
               Io.read_file opam_file
             else
               let template_file =
                 Dune_rules.Opam_create.template_file opam_file
               in
               let template =
                 if Path.exists template_file then
                   Some (template_file, Io.read_file template_file)
                 else
                   None
               in
               Dune_rules.Opam_create.generate project pkg ~template
           in
           Dyn.Tuple [ String (Path.to_string opam_file); String contents ]))
end

(* What to describe. To determine what to describe, we convert the positional
   arguments of the command line to a list of atoms and we parse it using the
   regular [Dune_lang.Decoder].

   This way we can reuse all the existing versioning, error reporting, etc...
   machinery. This also allow to easily extend this to arbitrary complex phrases
   without hassle. *)
module What = struct
  type t =
    | Workspace
    | Opam_files

  let default = Workspace

  let parse =
    let open Dune_lang.Decoder in
    sum [ ("workspace", return Workspace); ("opam-files", return Opam_files) ]

  let parse ~lang args =
    match args with
    | [] -> default
    | _ ->
      let parse =
        Dune_lang.Syntax.set Dune_engine.Stanza.syntax (Active lang) parse
      in
      let ast =
        Dune_lang.Ast.add_loc ~loc:Loc.none
          (List (List.map args ~f:Dune_lang.atom_or_quoted_string))
      in
      Dune_lang.Decoder.parse parse Univ_map.empty ast

  let describe t setup context =
    match t with
    | Workspace -> Crawl.workspace setup context
    | Opam_files -> Memo.Build.return (Opam_files.get ())
end

module Format = struct
  type t =
    | Sexp
    | Csexp

  let all = [ ("sexp", Sexp); ("csexp", Csexp) ]

  let arg =
    Arg.(
      value
      & opt (enum all) Sexp
      & info [ "format" ] ~docv:"FORMAT" ~doc:"Output format.")
end

module Lang = struct
  type t = Dune_lang.Syntax.Version.t

  let arg_conv =
    let parser s =
      match Scanf.sscanf s "%u.%u" (fun a b -> (a, b)) with
      | Ok t -> Ok t
      | Error () -> Error (`Msg "Expected version of the form NNN.NNN.")
    in
    let printer ppf t =
      Stdlib.Format.fprintf ppf "%s" (Dune_lang.Syntax.Version.to_string t)
    in
    Arg.conv ~docv:"VERSION" (parser, printer)

  let arg : t Term.t =
    Term.ret
    @@ let+ v =
         Arg.(
           value
           & opt arg_conv (0, 1)
           & info [ "lang" ] ~docv:"VERSION"
               ~doc:"Behave the same as this version of Dune.")
       in
       if v = (0, 1) then
         `Ok v
       else
         let msg =
           let pp =
             "Only --lang 0.1 is available at the moment as this command is \
              not yet stabilised. If you would like to release a software that \
              relies on the output of 'dune describe', please open a ticket on \
              https://github.com/ocaml/dune." |> Pp.text
           in
           Stdlib.Format.asprintf "%a" Pp.to_fmt pp
         in
         `Error (true, msg)
end

let print_as_sexp dyn =
  let rec dune_lang_of_sexp : Sexp.t -> Dune_lang.t = function
    | Atom s -> Dune_lang.atom_or_quoted_string s
    | List l -> List (List.map l ~f:dune_lang_of_sexp)
  in
  let cst =
    dyn |> Sexp.of_dyn |> dune_lang_of_sexp
    |> Dune_lang.Ast.add_loc ~loc:Loc.none
    |> Dune_lang.Cst.concrete
  in
  let version =
    Dune_lang.Syntax.greatest_supported_version Dune_engine.Stanza.syntax
  in
  Pp.to_fmt Stdlib.Format.std_formatter
    (Dune_engine.Format_dune_lang.pp_top_sexps ~version [ cst ])

let term =
  let+ common = Common.term
  and+ what =
    Arg.(
      value & pos_all string []
      & info [] ~docv:"STRING"
          ~doc:
            "What to describe. The syntax of this desciption is tied to the \
             version passed to $(b,--lang)")
  and+ context_name = Common.context_arg ~doc:"Build context to use."
  and+ format = Format.arg
  and+ lang = Lang.arg in
  Common.set_common common ~targets:[] ~external_lib_deps_mode:false;
  let what = What.parse what ~lang in
  Scheduler.go ~common (fun () ->
      let open Fiber.O in
      let* setup = Memo.Build.run (Import.Main.setup common) in
      let context =
        Import.Main.find_context_exn setup.workspace ~name:context_name
      in
      let+ res = Memo.Build.run (What.describe what setup context) in
      match format with
      | Csexp -> Csexp.to_channel stdout (Sexp.of_dyn res)
      | Sexp -> print_as_sexp res)

let command = (term, info)
