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
    let open Memo.Build.O in
    let dirs, files = dirs_and_files ~dir t_list in
    let dep_set = Dep.Set.of_files files in
    let+ l =
      Memo.Build.parallel_map dirs ~f:(fun dir -> Dep.Set.source_tree dir)
    in
    List.fold_left l ~init:dep_set ~f:Dep.Set.union
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
  ; version : Dune_lang.Syntax.Version.t
  ; files : Predicate_lang.Glob.t
  ; packages : (Loc.t * Package.Name.t) list
  ; deps : Dep_conf.t Bindings.t
  ; preludes : Prelude.t list
  ; enabled_if : Blang.t
  ; package : Package.t option
  ; libraries : Lib_dep.t list
  }

let enabled_if t = t.enabled_if

type Stanza.t += T of t

let syntax =
  let name = "mdx" in
  let desc = "mdx extension to verify code blocks in .md files" in
  Dune_lang.Syntax.create ~name ~desc
    [ ((0, 1), `Since (2, 4)); ((0, 2), `Since (3, 0)) ]

let default_files =
  let has_extension ext s = String.equal ext (Filename.extension s) in
  Predicate_lang.Glob.of_pred (has_extension ".md")

let decode =
  let open Dune_lang.Decoder in
  fields
    (let+ loc = loc
     and+ version = Dune_lang.Syntax.get_exn syntax
     and+ files =
       field "files" Predicate_lang.Glob.decode ~default:default_files
     and+ enabled_if =
       Enabled_if.decode ~allowed_vars:Any ~since:(Some (2, 9)) ()
     and+ package =
       Stanza_common.Pkg.field_opt ()
         ~check:(Dune_lang.Syntax.since Stanza.syntax (2, 9))
     and+ packages =
       field ~default:[] "packages"
         (Dune_lang.Syntax.deprecated_in syntax (0, 2)
         >>> repeat (located Package.Name.decode))
     and+ deps =
       field "deps" ~default:Bindings.empty
         (Dune_lang.Syntax.since syntax (0, 2)
         >>> Bindings.decode Dep_conf.decode)
     and+ preludes = field ~default:[] "preludes" (repeat Prelude.decode)
     and+ libraries =
       field "libraries" ~default:[]
         (Dune_lang.Syntax.since syntax (0, 2)
         >>> Dune_file.Lib_deps.decode Executable)
     in
     { loc
     ; version
     ; files
     ; packages
     ; deps
     ; preludes
     ; libraries
     ; enabled_if
     ; package
     })

let () =
  let open Dune_lang.Decoder in
  let decode = Dune_lang.Syntax.since Stanza.syntax (2, 4) >>> decode in
  Dune_project.Extension.register_simple syntax
    (return [ ("mdx", decode >>| fun x -> [ T x ]) ])

(** Returns the list of files (in _build) to be passed to mdx for the given
    stanza and context *)
let files_to_mdx t ~sctx ~dir =
  let open Memo.Build.O in
  let src_dir = Path.Build.drop_build_context_exn dir in
  let+ src_dir_files =
    Source_tree.files_of src_dir >>| Path.Source.Set.to_list
  in
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
let gen_rules_for_single_file stanza ~sctx ~dir ~expander ~mdx_prog
    ~mdx_prog_gen src =
  let loc = stanza.loc in
  let mdx_dir = Path.Build.relative dir ".mdx" in
  let files = Files.from_source_file ~mdx_dir src in
  (* Add the rule for generating the .mdx.deps file with ocaml-mdx deps *)
  let open Memo.Build.O in
  let* () =
    Super_context.add_rule sctx ~loc ~dir (Deps.rule ~dir ~mdx_prog files)
  and* () =
    (* Add the rule for generating the .corrected file using ocaml-mdx test *)
    let mdx_action =
      let open Action_builder.With_targets.O in
      let mdx_input_dependencies =
        Action_builder.bind (Deps.read files) ~f:(fun dep_set ->
            Action_builder.memo_build (Deps.to_dep_set dep_set ~dir))
      in
      let dyn_deps =
        Action_builder.map mdx_input_dependencies ~f:(fun d -> ((), d))
      in
      let mdx_package_deps =
        stanza.packages
        |> List.map ~f:(fun (loc, pkg) ->
               Dep_conf.Package
                 (Package.Name.to_string pkg |> String_with_vars.make_text loc))
      in
      let mdx_generic_deps = Bindings.to_list stanza.deps in

      let executable, command_line =
        (*The old mdx stanza calls the [ocaml-mdx] executable, new ones the
          generated executable *)
        let open Command.Args in
        match mdx_prog_gen with
        | Some prog -> (Ok (Path.build prog), [ Dep (Path.build files.src) ])
        | None ->
          let prelude_args =
            List.concat_map stanza.preludes ~f:(Prelude.to_args ~dir)
          in
          ( mdx_prog
          , [ A "test" ] @ prelude_args
            @ [ A "-o"; Target files.corrected; Dep (Path.build files.src) ] )
      in

      Action_builder.(
        with_no_targets
          (Dep_conf_eval.unnamed ~expander
             (mdx_package_deps @ mdx_generic_deps)))
      >>> Action_builder.with_no_targets (Action_builder.dyn_deps dyn_deps)
      >>> Command.run ~dir:(Path.build dir) ~stdout_to:files.corrected
            executable command_line
    in
    Super_context.add_rule sctx ~loc ~dir mdx_action
  in
  (* Attach the diff action to the @runtest for the src and corrected files *)
  let diff_action = Files.diff_action files in
  Super_context.add_alias_action sctx (Alias.runtest ~dir) ~loc:(Some loc) ~dir
    diff_action

let name = "mdx_gen"

let mdx_prog_gen t ~sctx ~dir ~scope ~expander ~mdx_prog =
  let loc = t.loc in
  let dune_version = Scope.project scope |> Dune_project.dune_version in
  let file = Path.Build.relative dir "mdx_gen.ml-gen" in

  (* Libs from the libraries field should have their include directories sent to
     mdx *)
  let open Resolve.Build.O in
  let directory_args =
    let+ libs_to_include =
      Resolve.Build.List.filter_map t.libraries ~f:(function
        | Direct lib
        | Re_export lib ->
          let+ lib = Lib.DB.resolve (Scope.libs scope) lib in
          Some lib
        | _ -> Resolve.Build.return None)
    in

    let mode = Context.best_mode (Super_context.context sctx) in

    let libs_include_paths = Lib.L.include_paths libs_to_include mode in

    let open Command.Args in
    let args =
      Path.Set.to_list libs_include_paths
      |> List.map ~f:(fun p -> S [ A "--directory"; Path p ])
    in
    S args
  in

  let prelude_args =
    Command.Args.S (List.concat_map t.preludes ~f:(Prelude.to_args ~dir))
  in

  (* We call mdx to generate the testing executable source *)
  let action =
    Command.run ~dir:(Path.build dir) mdx_prog ~stdout_to:file
      [ A "dune-gen"; prelude_args; Resolve.Build.args directory_args ]
  in
  let open Memo.Build.O in
  let* () = Super_context.add_rule sctx ~loc ~dir action in

  (* We build the generated executable linking in the libs from the libraries
     field *)
  let obj_dir = Obj_dir.make_exe ~dir ~name in
  let main_module_name = Module_name.of_string name in
  let module_ = Module.generated ~src_dir:(Path.build dir) main_module_name in
  let modules = Modules.singleton_exe module_ in
  let flags = Ocaml_flags.default ~dune_version ~profile:Release in
  let compile_info =
    Lib.DB.resolve_user_written_deps_for_exes (Scope.libs scope)
      [ (t.loc, name) ]
      (Lib_dep.Direct (loc, Lib_name.of_string "mdx.test") :: t.libraries)
      ~pps:[] ~dune_version
  in
  let* cctx =
    let+ requires_compile = Lib.Compile.direct_requires compile_info
    and+ requires_link =
      Memo.Lazy.force (Lib.Compile.requires_link compile_info)
    in
    let requires_link = lazy requires_link in
    Compilation_context.create ~super_context:sctx ~scope ~expander ~obj_dir
      ~modules ~flags ~requires_compile ~requires_link ~opaque:(Explicit false)
      ~js_of_ocaml:None ~package:None ()
  in
  let+ () =
    Exe.build_and_link cctx
      ~program:{ name; main_module_name; loc }
      ~link_args:(Action_builder.return (Command.Args.A "-linkall"))
      ~linkages:[ Exe.Linkage.byte ] ~promote:None
  in
  Path.Build.relative dir (name ^ ".bc")

(** Generates the rules for a given mdx stanza *)
let gen_rules t ~sctx ~dir ~scope ~expander =
  let open Memo.Build.O in
  let register_rules () =
    let* files_to_mdx = files_to_mdx t ~sctx ~dir
    and* mdx_prog =
      Super_context.resolve_program sctx ~dir ~loc:(Some t.loc)
        ~hint:"opam install mdx" "ocaml-mdx"
    in
    let* mdx_prog_gen =
      if Dune_lang.Syntax.Version.Infix.(t.version >= (0, 2)) then
        Memo.Build.Option.map (Some t)
          ~f:(mdx_prog_gen ~sctx ~dir ~scope ~expander ~mdx_prog)
      else
        Memo.Build.return None
    in
    Memo.Build.parallel_iter files_to_mdx
      ~f:
        (gen_rules_for_single_file t ~sctx ~dir ~expander ~mdx_prog
           ~mdx_prog_gen)
  in
  let* only_packages = Only_packages.get () in
  let do_it =
    match (only_packages, t.package) with
    | None, _
    | Some _, None ->
      true
    | Some only, Some stanza_package ->
      Package.Name.Map.mem only (Package.name stanza_package)
  in
  Memo.Build.when_ do_it register_rules
