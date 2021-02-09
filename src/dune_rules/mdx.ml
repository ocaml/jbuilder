open! Dune_engine
open Import

let mdx_version_required = "1.6.0"

module Files = struct
  type t =
    { src : Path.Build.t
    ; deps : Path.Build.t
    ; corrected : Path.Build.t
    }

  let corrected_file build_path =
    Path.Build.extend_basename ~suffix:".corrected" build_path

  let deps_file build_path =
    Path.Build.extend_basename ~suffix:".mdx.deps" build_path

  let from_source_file ~mdx_dir src =
    let basename = Path.Build.basename src in
    let dot_mdx_path = Path.Build.relative mdx_dir basename in
    let deps = deps_file dot_mdx_path in
    let corrected = corrected_file dot_mdx_path in
    { src; deps; corrected }

  let diff_action { src; corrected; deps = _ } =
    let src = Path.build src in
    let open Action_builder.O in
    let+ () = Action_builder.path src
    and+ () = Action_builder.path (Path.build corrected) in
    Action.diff ~optional:false src corrected
end

module Deps = struct
  type t =
    | File of string
    | Dir of string

  let parse_one sexp =
    match (sexp : Sexp.t) with
    | List [ Atom "dir"; Atom dir ] -> Ok (Dir dir)
    | List [ Atom "file"; Atom file ] -> Ok (File file)
    | _ ->
      Result.errorf "Unknown 'ocaml-mdx deps' item: %s" (Sexp.to_string sexp)

  let parse s =
    match Csexp.parse_string s with
    | Ok (List items) -> Result.List.map ~f:parse_one items
    | Ok _ -> Result.errorf "Unsupported 'ocaml-mdx deps' output format"
    | Error (_, msg) -> Error msg

  let read (files : Files.t) =
    let open Action_builder.O in
    let path = Path.build files.deps in
    let+ content = Action_builder.contents path in
    match parse content with
    | Ok deps -> deps
    | Error msg ->
      User_error.raise
        [ Pp.textf "Mdx dependencies for %s could not be interpreted:"
            (Path.to_string_maybe_quoted (Path.build files.src))
        ; Pp.text msg
        ; Pp.textf "Please make sure you are using mdx.%s or higher"
            mdx_version_required
        ]

  let rule ~dir ~mdx_prog files =
    Command.run ~dir:(Path.build dir) mdx_prog
      [ A "deps"; Dep (Path.build files.Files.src) ]
      ~stdout_to:files.Files.deps

  let to_path ~dir str =
    let local = Path.Local.of_string str in
    let build = Path.Build.append_local dir local in
    Path.build build

  let dirs_and_files ~dir t_list =
    List.partition_map t_list ~f:(function
      | Dir d -> Left (to_path ~dir d)
      | File f -> Right (to_path ~dir f))

  let to_dep_set ~dir t_list =
    let dirs, files = dirs_and_files ~dir t_list in
    let dep_set = Dep.Set.of_files files in
    List.fold_left dirs ~init:dep_set ~f:(fun acc dir ->
        Dep.Set.union acc (Dep.Set.source_tree dir))
end

module Prelude = struct
  type t =
    | Default of Path.Local.t
    | Env of
        { env : string
        ; file : Path.Local.t
        }

  let decode =
    let open Dune_lang.Decoder in
    let path = string >>| Path.Local.of_string in
    let decode_env =
      let+ () = keyword "env"
      and+ env = string
      and+ file = path in
      Env { env; file }
    in
    let decode_default =
      let+ file = path in
      Default file
    in
    enter decode_env <|> decode_default

  let to_args ~dir t : _ Command.Args.t list =
    let bpath p = Path.build (Path.Build.append_local dir p) in
    match t with
    | Default file -> [ A "--prelude"; Dep (bpath file) ]
    | Env { env; file } ->
      let arg = sprintf "%s:%s" env (Path.Local.to_string file) in
      [ A "--prelude"; A arg; Hidden_deps (Dep.Set.of_files [ bpath file ]) ]
end

type t =
  { loc : Loc.t
  ; files : Predicate_lang.Glob.t
  ; packages : (Loc.t * Package.Name.t) list
  ; preludes : Prelude.t list
  }

type Stanza.t += T of t

let syntax =
  let name = "mdx" in
  let desc = "mdx extension to verify code blocks in .md files" in
  Dune_lang.Syntax.create ~name ~desc [ ((0, 1), `Since (2, 4)) ]

let default_files =
  let has_extension ext s = String.equal ext (Filename.extension s) in
  Predicate_lang.Glob.of_pred (has_extension ".md")

let decode =
  let open Dune_lang.Decoder in
  fields
    (let+ loc = loc
     and+ files =
       field "files" Predicate_lang.Glob.decode ~default:default_files
     and+ packages =
       field ~default:[] "packages" (repeat (located Package.Name.decode))
     and+ preludes = field ~default:[] "preludes" (repeat Prelude.decode) in
     { loc; files; packages; preludes })

let () =
  let open Dune_lang.Decoder in
  let decode = Dune_lang.Syntax.since Stanza.syntax (2, 4) >>> decode in
  Dune_project.Extension.register_simple syntax
    (return [ ("mdx", decode >>| fun x -> [ T x ]) ])

(** Returns the list of files (in _build) to be passed to mdx for the given
    stanza and context *)
let files_to_mdx t ~sctx ~dir =
  let src_dir = Path.Build.drop_build_context_exn dir in
  let src_dir_files = Path.Source.Set.to_list (File_tree.files_of src_dir) in
  let must_mdx src_path =
    let file = Path.Source.basename src_path in
    Predicate_lang.Glob.exec t.files ~standard:default_files file
  in
  let build_path src_path =
    Path.Build.append_source (Super_context.context sctx).build_dir src_path
  in
  List.filter_map src_dir_files ~f:(fun src_path ->
      if must_mdx src_path then
        Some (build_path src_path)
      else
        None)

(** Generates the rules for a single [src] file covered covered by the given
    [stanza]. *)
let gen_rules_for_single_file stanza ~sctx ~dir ~expander ~mdx_prog src =
  let loc = stanza.loc in
  let mdx_dir = Path.Build.relative dir ".mdx" in
  let files = Files.from_source_file ~mdx_dir src in
  (* Add the rule for generating the .mdx.deps file with ocaml-mdx deps *)
  Super_context.add_rule sctx ~loc ~dir (Deps.rule ~dir ~mdx_prog files);
  (* Add the rule for generating the .corrected file using ocaml-mdx test *)
  let mdx_action =
    let open Action_builder.With_targets.O in
    let deps = Action_builder.map (Deps.read files) ~f:(Deps.to_dep_set ~dir) in
    let dyn_deps = Action_builder.map deps ~f:(fun d -> ((), d)) in
    let pkg_deps =
      stanza.packages
      |> List.map ~f:(fun (loc, pkg) ->
             Dep_conf.Package
               (Package.Name.to_string pkg |> String_with_vars.make_text loc))
    in
    let prelude_args =
      List.concat_map stanza.preludes ~f:(Prelude.to_args ~dir)
    in
    Action_builder.(with_no_targets (Dep_conf_eval.unnamed ~expander pkg_deps))
    >>> Action_builder.with_no_targets (Action_builder.dyn_deps dyn_deps)
    >>> Command.run ~dir:(Path.build dir) mdx_prog
          ([ Command.Args.A "test" ] @ prelude_args
          @ [ A "-o"; Target files.corrected; Dep (Path.build files.src) ])
  in
  Super_context.add_rule sctx ~loc ~dir mdx_action;
  (* Attach the diff action to the @runtest for the src and corrected files *)
  let diff_action = Files.diff_action files in
  Super_context.add_alias_action sctx (Alias.runtest ~dir) ~loc:(Some loc) ~dir
    ~stamp:("mdx", files.src)
    (Action_builder.with_no_targets diff_action)

(** Generates the rules for a given mdx stanza *)
let gen_rules t ~sctx ~dir ~expander =
  let files_to_mdx = files_to_mdx t ~sctx ~dir in
  let mdx_prog =
    Super_context.resolve_program sctx ~dir ~loc:(Some t.loc)
      ~hint:"opam install mdx" "ocaml-mdx"
  in
  List.iter files_to_mdx
    ~f:(gen_rules_for_single_file t ~sctx ~dir ~expander ~mdx_prog)
