open Stdune
open Import

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
  open Dune

  let uid_of_library lib =
    Digest.generic
      (Lib.name lib, Path.to_string (Lib_info.src_dir (Lib.info lib)))
    |> Digest.to_string

  let library sctx lib =
    match Lib.requires lib with
    | Error _ -> None
    | Ok requires ->
      let name = Lib.name lib in
      let info = Lib.info lib in
      let src_dir = Lib_info.src_dir info in
      let obj_dir = Lib_info.obj_dir info in
      let dyn_path p = Dyn.String (Path.to_string p) in
      let modules_ =
        Dir_contents.get sctx ~dir:(Path.as_in_build_dir_exn src_dir)
        |> Dir_contents.ocaml
        |> Ml_sources.modules_of_library ~name
        |> Result.ok_exn
        |> Modules.fold_no_vlib ~init:[] ~f:(fun m acc ->
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
      in
      Some
        (Dyn.Variant
           ( "library"
           , [ Dyn.Encoder.record
                 [ ("name", Lib_name.to_dyn name)
                 ; ("uid", String (uid_of_library lib))
                 ; ( "requires"
                   , Dyn.Encoder.(list string)
                       (List.map requires ~f:uid_of_library) )
                 ; ("source_dir", dyn_path src_dir)
                 ; ("modules", List modules_)
                 ]
             ] ))

  let workspace { Dune.Main.workspace; scontexts } (context : Context.t) =
    let sctx = Context_name.Map.find_exn scontexts context.name in
    Dyn.List
      (List.concat_map workspace.conf.projects ~f:(fun project ->
           Super_context.find_scope_by_project sctx project
           |> Scope.libs |> Lib.DB.all |> Lib.Set.to_list
           |> List.filter_map ~f:(library sctx)))
end

(* What to describe. To determine what to describe, we convert the positional
   arguments of the command line to a list of atoms and we parse it using the
   regular [Dune_lang.Decoder].

   This way we can reuse all the existing versionning, error reporting, etc...
   machinery. This also allow to easily extend this to arbitrary complex phrases
   without hassle. *)
module What = struct
  type t = Workspace

  let default = Workspace

  let parse =
    let open Dune_lang.Decoder in
    sum [ ("workspace", return Workspace) ]

  let parse ~lang args =
    match args with
    | [] -> default
    | _ ->
      let parse = Dune_lang.Syntax.set Dune.Stanza.syntax lang parse in
      let ast =
        Dune_lang.Ast.add_loc ~loc:Loc.none
          (List (List.map args ~f:Dune_lang.atom_or_quoted_string))
      in
      Dune_lang.Decoder.parse parse Univ_map.empty ast

  let describe t setup context =
    match t with
    | Workspace -> Crawl.workspace setup context
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
         `Error
           ( true
           , "Only --lang 0.1 is available at the moment as this command is \
              not yet stabilised. If you would like to release a software that \
              relies on the output of 'dune describe', please open a ticket on \
              https://github.com/ocaml/dune." )
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
  Dune.Format_dune_lang.pp_top_sexps Stdlib.Format.std_formatter [ cst ]

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
      let* setup = Import.Main.setup common in
      let context =
        Import.Main.find_context_exn setup.workspace ~name:context_name
      in
      let res = What.describe what setup context in
      Fiber.return
        ( match format with
        | Csexp -> Csexp.to_channel stdout (Sexp.of_dyn res)
        | Sexp -> print_as_sexp res ))

let command = (term, info)
