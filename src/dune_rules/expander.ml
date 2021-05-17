open! Dune_engine
open Import
open Action_builder.O

type any_package =
  | Local of Package.t
  | Installed of Dune_package.t

module Expanding_what = struct
  type t =
    | Nothing_special
    | Deps_like_field
    | User_action of Path.Build.t Targets.t
    | User_action_without_targets of { what : string }
end

type t =
  { dir : Path.Build.t
  ; env : Env.t
  ; local_env : string Action_builder.t Env.Var.Map.t
  ; lib_artifacts : Artifacts.Public_libs.t
  ; lib_artifacts_host : Artifacts.Public_libs.t
  ; bin_artifacts_host : Artifacts.Bin.t
  ; bindings : Value.t list Action_builder.t Pform.Map.t
  ; scope : Scope.t
  ; scope_host : Scope.t
  ; c_compiler : string
  ; context : Context.t
  ; artifacts_dynamic : bool
  ; lookup_artifacts :
      (dir:Path.Build.t -> Ml_sources.Artifacts.t Memo.Build.t) option
  ; foreign_flags :
         dir:Path.Build.t
      -> string list Action_builder.t Foreign_language.Dict.t Memo.Build.t
  ; find_package : Package.Name.t -> any_package option
  ; expanding_what : Expanding_what.t
  }

let scope t = t.scope

let artifacts t = t.bin_artifacts_host

let dir t = t.dir

let context t = t.context

let set_foreign_flags t ~f:foreign_flags = { t with foreign_flags }

let set_local_env_var t ~var ~value =
  { t with local_env = Env.Var.Map.set t.local_env var value }

let set_dir t ~dir = { t with dir }

let set_scope t ~scope = { t with scope }

let set_bin_artifacts t ~bin_artifacts_host = { t with bin_artifacts_host }

let set_artifacts_dynamic t artifacts_dynamic = { t with artifacts_dynamic }

let set_lookup_ml_sources t ~f = { t with lookup_artifacts = Some f }

let set_expanding_what t x = { t with expanding_what = x }

let map_exe t p =
  match t.expanding_what with
  | Deps_like_field -> p
  | Nothing_special
  | User_action _
  | User_action_without_targets _ ->
    Context.map_exe t.context p

let extend_env t ~env =
  (* [t.local_env] has precedence over [t.env], so we cannot extend [env] if
     there are already local bidings.. *)
  assert (Env.Var.Map.is_empty t.local_env);
  { t with env = Env.extend_env t.env env }

let add_bindings_full t ~bindings =
  { t with bindings = Pform.Map.superpose t.bindings bindings }

let add_bindings t ~bindings =
  add_bindings_full t
    ~bindings:(Pform.Map.map bindings ~f:(fun v -> Action_builder.return v))

let path p = [ Value.Path p ]

let string s = [ Value.String s ]

let strings l = Value.L.strings l

let dep p =
  let+ () = Action_builder.path p in
  [ Value.Path p ]

let expand_version { scope; _ } ~source s =
  let value_from_version = function
    | None -> [ Value.String "" ]
    | Some s -> [ String s ]
  in
  let project = Scope.project scope in
  match
    Package.Name.Map.find
      (Dune_project.packages project)
      (Package.Name.of_string s)
  with
  | Some p -> value_from_version p.version
  | None when Dune_project.dune_version project < (2, 9) ->
    User_error.raise ~loc:source.Dune_lang.Template.Pform.loc
      [ Pp.textf "Package %S doesn't exist in the current project." s ]
      ~hints:
        [ Pp.text
            "If you want to refer to an installed package, or more generally \
             to a package from another project, you need at least (lang dune \
             2.9)."
        ]
  | None -> (
    let libname = Lib_name.of_string s in
    let pkgname = Lib_name.package_name libname in
    if not (String.equal (Package.Name.to_string pkgname) s) then
      User_error.raise ~loc:source.Dune_lang.Template.Pform.loc
        [ Pp.textf
            "Library names are not allowed in this position. Only package \
             names are allowed"
        ];
    match Lib.DB.find (Scope.libs scope) libname with
    | Some lib -> value_from_version (Lib_info.version (Lib.info lib))
    | None ->
      User_error.raise ~loc:source.Dune_lang.Template.Pform.loc
        [ Pp.textf
            "Package %S doesn't exist in the current project and isn't \
             installed either."
            s
        ])

let isn't_allowed_in_this_position_message ~source =
  User_error.make ~loc:source.Dune_lang.Template.Pform.loc
    [ Pp.textf "%s isn't allowed in this position."
        (Dune_lang.Template.Pform.describe source)
    ]

let isn't_allowed_in_this_position ~source =
  raise
    (User_error.E
       ( isn't_allowed_in_this_position_message ~source
       , User_error.Annotations.none ))

let expand_artifact ~source t a s =
  match t.lookup_artifacts with
  | None -> isn't_allowed_in_this_position ~source
  | Some lookup -> (
    let path = Path.Build.relative t.dir s in
    let name = Path.Build.basename path in
    let dir = Path.Build.parent_exn path in
    let does_not_exist ~loc ~what name =
      User_error.raise ~loc [ Pp.textf "%s %s does not exist." what name ]
    in
    let* artifacts = Action_builder.memo_build (lookup ~dir) in
    match a with
    | Pform.Artifact.Mod kind -> (
      let name =
        Module_name.of_string_allow_invalid
          (Dune_lang.Template.Pform.loc source, name)
      in
      match Ml_sources.Artifacts.lookup_module artifacts name with
      | None ->
        does_not_exist
          ~loc:(Dune_lang.Template.Pform.loc source)
          ~what:"Module"
          (Module_name.to_string name)
      | Some (t, m) -> (
        match Obj_dir.Module.cm_file t m ~kind with
        | None -> Action_builder.return [ Value.String "" ]
        | Some path -> dep (Path.build path)))
    | Lib mode -> (
      let name =
        Lib_name.parse_string_exn (Dune_lang.Template.Pform.loc source, name)
      in
      match Ml_sources.Artifacts.lookup_library artifacts name with
      | None ->
        does_not_exist
          ~loc:(Dune_lang.Template.Pform.loc source)
          ~what:"Library" (Lib_name.to_string name)
      | Some lib ->
        let archives = Mode.Dict.get (Lib_info.archives lib) mode in
        Action_builder.all
          (List.map archives ~f:(fun fn ->
               let fn = Path.build fn in
               let+ () = Action_builder.path fn in
               Value.Path fn))))

let cc t =
  Memo.Build.map (t.foreign_flags ~dir:t.dir) ~f:(fun cc ->
      Foreign_language.Dict.map cc ~f:(fun cc ->
          let+ flags = cc in
          strings (t.c_compiler :: flags)))

let get_prog = function
  | Ok p -> path p
  | Error err -> Action.Prog.Not_found.raise err

let c_compiler_and_flags (context : Context.t) =
  Ocaml_config.c_compiler context.ocaml_config
  :: Ocaml_config.ocamlc_cflags context.ocaml_config

let relative ~source d s =
  Path.build
    (Path.Build.relative ~error_loc:(Dune_lang.Template.Pform.loc source) d s)

type nonrec expansion_result =
  | Direct of Value.t list Action_builder.t
  | Need_full_expander of (t -> Value.t list Action_builder.t)

let static v = Direct (Action_builder.return v)

let[@inline never] invalid_use_of_target_variable t
    ~(source : Dune_lang.Template.Pform.t) ~var_multiplicity =
  match t.expanding_what with
  | Nothing_special
  | Deps_like_field ->
    isn't_allowed_in_this_position ~source
  | User_action_without_targets { what } ->
    User_error.raise ~loc:source.loc
      [ Pp.textf "You cannot use %s in %s."
          (Dune_lang.Template.Pform.describe source)
          what
      ]
  | User_action targets -> (
    match targets with
    | Infer ->
      User_error.raise ~loc:source.loc
        [ Pp.textf "You cannot use %s with inferred rules."
            (Dune_lang.Template.Pform.describe source)
        ]
    | Static { targets = _; multiplicity } ->
      assert (multiplicity <> var_multiplicity);
      Targets.Multiplicity.check_variable_matches_field ~loc:source.loc
        ~field:multiplicity ~variable:var_multiplicity;
      assert false)

let expand_pform_gen ~(context : Context.t) ~bindings ~dir ~source
    (pform : Pform.t) : expansion_result =
  match Pform.Map.find bindings pform with
  | Some x -> Direct x
  | None -> (
    match pform with
    | Var var -> (
      match var with
      | Nothing -> static []
      | User_var _
      | Deps
      | Input_file
      | Library_name
      | Impl_files
      | Intf_files
      | Inline_tests
      | Test
      | Corrected_suffix ->
        (* These would be part of [bindings] *)
        isn't_allowed_in_this_position ~source
      | First_dep ->
        (* This case is for %{<} which was only allowed inside jbuild files *)
        assert false
      | Target ->
        Need_full_expander
          (fun t ->
            invalid_use_of_target_variable t ~source ~var_multiplicity:One)
      | Targets ->
        Need_full_expander
          (fun t ->
            invalid_use_of_target_variable t ~source ~var_multiplicity:Multiple)
      | Ocaml -> static (get_prog context.ocaml)
      | Ocamlc -> static (path context.ocamlc)
      | Ocamlopt -> static (get_prog context.ocamlopt)
      | Make ->
        Direct
          (Action_builder.memo_build (context.which "make") >>| function
           | None ->
             Utils.program_not_found ~context:context.name
               ~loc:(Some (Dune_lang.Template.Pform.loc source))
               "make"
           | Some p -> path p)
      | Cpp -> static (strings (c_compiler_and_flags context @ [ "-E" ]))
      | Pa_cpp ->
        static
          (strings
             (c_compiler_and_flags context
             @ [ "-undef"; "-traditional"; "-x"; "c"; "-E" ]))
      | Arch_sixtyfour ->
        static (string (string_of_bool context.arch_sixtyfour))
      | Ocaml_bin_dir -> static [ Dir context.ocaml_bin ]
      | Ocaml_version ->
        static (string (Ocaml_config.version_string context.ocaml_config))
      | Ocaml_stdlib_dir -> static (string (Path.to_string context.stdlib_dir))
      | Dev_null -> static (string (Path.to_string Config.dev_null))
      | Ext_obj -> static (string context.lib_config.ext_obj)
      | Ext_asm -> static (string (Ocaml_config.ext_asm context.ocaml_config))
      | Ext_lib -> static (string context.lib_config.ext_lib)
      | Ext_dll -> static (string context.lib_config.ext_dll)
      | Ext_exe -> static (string (Ocaml_config.ext_exe context.ocaml_config))
      | Ext_plugin ->
        static
          (string
             (Mode.plugin_ext
                (if Ocaml_config.natdynlink_supported context.ocaml_config then
                  Mode.Native
                else
                  Mode.Byte)))
      | Profile -> static (string (Profile.to_string context.profile))
      | Workspace_root -> static [ Value.Dir (Path.build context.build_dir) ]
      | Context_name -> static (string (Context_name.to_string context.name))
      | Os_type ->
        static
          (string
             (Ocaml_config.Os_type.to_string
                (Ocaml_config.os_type context.ocaml_config)))
      | Architecture ->
        static (string (Ocaml_config.architecture context.ocaml_config))
      | System -> static (string (Ocaml_config.system context.ocaml_config))
      | Model -> static (string (Ocaml_config.model context.ocaml_config))
      | Ignoring_promoted_rules ->
        static (string (string_of_bool !Clflags.ignore_promoted_rules))
      | Project_root ->
        Need_full_expander
          (fun t ->
            Action_builder.return
              [ Value.Dir (Path.build (Scope.root t.scope)) ])
      | Cc ->
        Need_full_expander
          (fun t ->
            let* cc = Action_builder.memo_build (cc t) in
            cc.c)
      | Cxx ->
        Need_full_expander
          (fun t ->
            let* cc = Action_builder.memo_build (cc t) in
            cc.cxx)
      | Ccomp_type ->
        static
          (string
             (Ocaml_config.Ccomp_type.to_string context.lib_config.ccomp_type)))
    | Macro (macro, s) -> (
      match macro with
      | Ocaml_config ->
        static
          (match Ocaml_config.by_name context.ocaml_config s with
          | None ->
            User_error.raise
              ~loc:(Dune_lang.Template.Pform.loc source)
              [ Pp.textf "Unknown ocaml configuration variable %S" s ]
          | Some v -> (
            match v with
            | Bool x -> string (string_of_bool x)
            | Int x -> string (string_of_int x)
            | String x -> string x
            | Words x -> strings x
            | Prog_and_args x -> strings (x.prog :: x.args)))
      | Env ->
        Need_full_expander
          (fun t ->
            match String.rsplit2 s ~on:'=' with
            | None ->
              User_error.raise ~loc:source.Dune_lang.Template.Pform.loc
                [ Pp.textf "%s must always come with a default value."
                    (Dune_lang.Template.Pform.describe source)
                ]
                ~hints:[ Pp.text "the syntax is %{env:VAR=DEFAULT-VALUE}" ]
            | Some (var, default) -> (
              match Env.Var.Map.find t.local_env var with
              | Some v ->
                let+ v = v in
                string v
              | None ->
                Action_builder.return
                  (string (Option.value ~default (Env.get t.env var)))))
      | Version ->
        Need_full_expander
          (fun t -> Action_builder.return (expand_version t ~source s))
      | Artifact a ->
        Need_full_expander
          (fun t ->
            if t.artifacts_dynamic then
              let* () = Action_builder.return () in
              expand_artifact ~source t a s
            else
              expand_artifact ~source t a s)
      | Path_no_dep ->
        (* This case is for %{path-no-dep:...} which was only allowed inside
           jbuild files *)
        assert false
      | Exe ->
        Need_full_expander (fun t -> dep (map_exe t (relative ~source t.dir s)))
      | Dep -> Need_full_expander (fun t -> dep (relative ~source t.dir s))
      | Bin ->
        Need_full_expander
          (fun t ->
            let* prog =
              Action_builder.memo_build
                (Artifacts.Bin.binary
                   ~loc:(Some (Dune_lang.Template.Pform.loc source))
                   t.bin_artifacts_host s)
            in
            dep (Action.Prog.ok_exn prog))
      | Lib { lib_exec; lib_private } ->
        Need_full_expander
          (fun t ->
            let lib, file =
              let loc = Dune_lang.Template.Pform.loc source in
              match String.lsplit2 s ~on:':' with
              | None ->
                User_error.raise ~loc
                  [ Pp.textf "invalid %%{lib:...} form: %s" s ]
              | Some (lib, f) -> (Lib_name.parse_string_exn (loc, lib), f)
            in
            let scope =
              if lib_exec then
                t.scope_host
              else
                t.scope
            in
            let p =
              let open Resolve.O in
              if lib_private then
                let* lib =
                  Lib.DB.resolve (Scope.libs scope)
                    (Dune_lang.Template.Pform.loc source, lib)
                in
                let current_project = Scope.project t.scope
                and referenced_project =
                  Lib.info lib |> Lib_info.status |> Lib_info.Status.project
                in
                if
                  Option.equal Dune_project.equal (Some current_project)
                    referenced_project
                then
                  Resolve.return
                    (Path.relative (Lib_info.src_dir (Lib.info lib)) file)
                else
                  Resolve.fail
                    (User_error.make
                       ~loc:(Dune_lang.Template.Pform.loc source)
                       [ Pp.textf
                           "The variable \"lib%s-private\" can only refer to \
                            libraries within the same project. The current \
                            project's name is %S, but the reference is to %s."
                           (if lib_exec then
                             "exec"
                           else
                             "")
                           (Dune_project.Name.to_string_hum
                              (Dune_project.name current_project))
                           (match referenced_project with
                           | None -> "an external library"
                           | Some project ->
                             Dune_project.name project
                             |> Dune_project.Name.to_string_hum |> String.quoted)
                       ])
              else
                let artifacts =
                  if lib_exec then
                    t.lib_artifacts_host
                  else
                    t.lib_artifacts
                in
                Artifacts.Public_libs.file_of_lib artifacts
                  ~loc:(Dune_lang.Template.Pform.loc source)
                  ~lib ~file
            in
            match Resolve.peek p with
            | Ok p ->
              if
                (not lib_exec) || (not Sys.win32)
                || Filename.extension s = ".exe"
              then
                dep p
              else
                let p_exe = Path.extend_basename p ~suffix:".exe" in
                Action_builder.if_file_exists p_exe ~then_:(dep p_exe)
                  ~else_:(dep p)
            | Error () ->
              let p =
                let open Resolve.O in
                if lib_private || not (Lib.DB.available (Scope.libs scope) lib)
                then
                  p >>| fun _ -> assert false
                else
                  Resolve.fail
                    (User_error.make
                       ~loc:(Dune_lang.Template.Pform.loc source)
                       [ Pp.textf
                           "The library %S is not public. The variable \
                            \"lib%s\" expands to the file's installation path \
                            which is not defined for private libraries."
                           (Lib_name.to_string lib)
                           (if lib_exec then
                             "exec"
                           else
                             "")
                       ])
              in
              Resolve.read p)
      | Lib_available ->
        Need_full_expander
          (fun t ->
            let lib =
              Lib_name.parse_string_exn (Dune_lang.Template.Pform.loc source, s)
            in
            Action_builder.return
              (Lib.DB.available (Scope.libs t.scope) lib
              |> string_of_bool |> string))
      | Read ->
        let path = relative ~source dir s in
        Direct (Action_builder.map (Action_builder.contents path) ~f:string)
      | Read_lines ->
        let path = relative ~source dir s in
        Direct (Action_builder.map (Action_builder.lines_of path) ~f:strings)
      | Read_strings ->
        let path = relative ~source dir s in
        Direct (Action_builder.map (Action_builder.strings path) ~f:strings)))

(* Make sure to delay exceptions *)
let expand_pform_gen ~context ~bindings ~dir ~source pform =
  match expand_pform_gen ~context ~bindings ~source ~dir pform with
  | exception (User_error.E _ as exn) ->
    Direct (Action_builder.fail { fail = (fun () -> reraise exn) })
  | Direct _ as x -> x
  | Need_full_expander f ->
    Need_full_expander
      (fun t ->
        try f t with
        | User_error.E _ as exn ->
          Action_builder.fail { fail = (fun () -> reraise exn) })

let expand_pform t ~source pform =
  match
    expand_pform_gen ~context:t.context ~bindings:t.bindings ~dir:t.dir ~source
      pform
  with
  | Direct v -> v
  | Need_full_expander f -> f t

let expand t ~mode template =
  Action_builder.Expander.expand ~dir:(Path.build t.dir) ~mode template
    ~f:(expand_pform t)

let make ~scope ~scope_host ~(context : Context.t) ~lib_artifacts
    ~lib_artifacts_host ~bin_artifacts_host ~find_package =
  { dir = context.build_dir
  ; env = context.env
  ; local_env = Env.Var.Map.empty
  ; bindings = Pform.Map.empty
  ; scope
  ; scope_host
  ; lib_artifacts
  ; lib_artifacts_host
  ; bin_artifacts_host
  ; c_compiler = Ocaml_config.c_compiler context.ocaml_config
  ; context
  ; artifacts_dynamic = false
  ; lookup_artifacts = None
  ; foreign_flags =
      (fun ~dir ->
        Code_error.raise "foreign flags expander is not set"
          [ ("dir", Path.Build.to_dyn dir) ])
  ; find_package
  ; expanding_what = Nothing_special
  }

let expand_path t sw =
  let+ v = expand t ~mode:Single sw in
  Value.to_path v ~error_loc:(String_with_vars.loc sw) ~dir:(Path.build t.dir)

let expand_str t sw =
  let+ v = expand t ~mode:Single sw in
  Value.to_string v ~dir:(Path.build t.dir)

module Static = struct
  let expand_pform t ~source pform =
    match Action_builder.static_eval (expand_pform t ~source pform) with
    | Some (v, _) -> v
    | None -> isn't_allowed_in_this_position ~source

  let expand t ~mode template =
    String_with_vars.expand ~dir:(Path.build t.dir) ~mode template
      ~f:(expand_pform t)

  let expand_path t sw =
    let v = expand t ~mode:Single sw in
    Value.to_path v ~error_loc:(String_with_vars.loc sw) ~dir:(Path.build t.dir)

  let expand_str t sw =
    let v = expand t ~mode:Single sw in
    Value.to_string v ~dir:(Path.build t.dir)

  module Or_exn = struct
    let expand_path t sw = Result.try_with (fun () -> expand_path t sw)

    let expand_str t sw = Result.try_with (fun () -> expand_str t sw)
  end

  module With_reduced_var_set = struct
    let expand_pform_opt ~(context : Context.t) ~dir ~source pform =
      match
        expand_pform_gen ~context ~bindings:Pform.Map.empty ~dir ~source pform
      with
      | Direct v -> (
        match Action_builder.static_eval v with
        | Some (v, _) -> Some v
        | None -> None)
      | Need_full_expander _ -> None

    let expand_pform ~context ~dir ~source pform =
      match expand_pform_opt ~context ~dir ~source pform with
      | Some v -> v
      | None -> isn't_allowed_in_this_position ~source

    let expand ~(context : Context.t) ~dir ~mode template =
      String_with_vars.expand ~dir:(Path.build dir) ~mode template
        ~f:(expand_pform ~context ~dir)

    let expand_path ~context ~dir sw =
      let v = expand ~context ~dir ~mode:Single sw in
      Value.to_path v ~error_loc:(String_with_vars.loc sw) ~dir:(Path.build dir)

    let expand_str ~context ~dir sw =
      let v = expand ~context ~dir ~mode:Single sw in
      Value.to_string v ~dir:(Path.build dir)

    let expand_str_partial ~context ~dir sw =
      String_with_vars.expand_as_much_as_possible sw ~dir:(Path.build dir)
        ~f:(expand_pform_opt ~context ~dir)
  end
end

let expand_and_eval_set t set ~standard =
  let dir = Path.build (dir t) in
  let+ standard =
    if Ordered_set_lang.Unexpanded.has_special_forms set then
      standard
    else
      Action_builder.return []
  and+ set = Ordered_set_lang.Unexpanded.expand set ~dir ~f:(expand_pform t) in
  Ordered_set_lang.eval set ~standard ~eq:String.equal ~parse:(fun ~loc:_ s ->
      s)

let eval_blang t = function
  | Blang.Const x -> x (* common case *)
  | blang -> Blang.eval blang ~dir:(Path.build t.dir) ~f:(Static.expand_pform t)

let find_package t pkg = t.find_package pkg
