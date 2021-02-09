open! Dune_engine
open Import

module Expanded = struct
  type t =
    | Value of Value.t list
    | Deferred of Dune_lang.Template.Pform.t * Pform.t
    | Unknown
    | Error of User_message.t

  let to_value_opt t ~on_deferred =
    match t with
    | Value v -> Some v
    | Deferred (source, pform) -> on_deferred ~source pform
    | Error m -> raise (User_error.E m)
    | Unknown -> None

  let of_value_opt = function
    | None -> Unknown
    | Some v -> Value v
end

type any_package =
  | Local of Package.t
  | Installed of Dune_package.t

type t =
  { dir : Path.Build.t
  ; hidden_env : Env.Var.Set.t
  ; env : Env.t
  ; lib_artifacts : Artifacts.Public_libs.t
  ; lib_artifacts_host : Artifacts.Public_libs.t
  ; bin_artifacts_host : Artifacts.Bin.t
  ; ocaml_config : Value.t list String.Map.t Lazy.t
  ; bindings : Value.t list Pform.Map.t
  ; scope : Scope.t
  ; scope_host : Scope.t
  ; c_compiler : string
  ; context : Context.t
  ; expand_var : t -> Expanded.t String_with_vars.expander
  ; artifacts_dynamic : bool
  ; lookup_artifacts : (dir:Path.Build.t -> Ml_sources.Artifacts.t) option
  ; map_exe : Path.t -> Path.t
  ; foreign_flags :
      dir:Path.Build.t -> string list Action_builder.t Foreign_language.Dict.t
  ; find_package : Package.Name.t -> any_package option
  }

let scope t = t.scope

let artifacts t = t.bin_artifacts_host

let dir t = t.dir

let context t = t.context

let make_ocaml_config ocaml_config =
  let string s = [ Value.String s ] in
  Ocaml_config.to_list ocaml_config
  |> String.Map.of_list_map_exn ~f:(fun (k, v) ->
         ( k
         , match (v : Ocaml_config.Value.t) with
           | Bool x -> string (string_of_bool x)
           | Int x -> string (string_of_int x)
           | String x -> string x
           | Words x -> Value.L.strings x
           | Prog_and_args x -> Value.L.strings (x.prog :: x.args) ))

let set_foreign_flags t ~f:foreign_flags = { t with foreign_flags }

let set_env t ~var ~value =
  { t with
    env = Env.add t.env ~var ~value
  ; hidden_env = Env.Var.Set.remove t.hidden_env var
  }

let hide_env t ~var = { t with hidden_env = Env.Var.Set.add t.hidden_env var }

let set_dir t ~dir = { t with dir }

let set_scope t ~scope = { t with scope }

let set_bin_artifacts t ~bin_artifacts_host = { t with bin_artifacts_host }

let set_artifacts_dynamic t artifacts_dynamic = { t with artifacts_dynamic }

let set_lookup_ml_sources t ~f = { t with lookup_artifacts = Some f }

let extend_env t ~env =
  { t with
    env = Env.extend_env t.env env
  ; hidden_env = Env.Var.Set.diff t.hidden_env (Env.vars env)
  }

let add_bindings t ~bindings =
  { t with bindings = Pform.Map.superpose t.bindings bindings }

let expand_ocaml_config ocaml_config ~source name =
  match String.Map.find ocaml_config name with
  | Some x -> x
  | None ->
    User_error.raise
      ~loc:(Dune_lang.Template.Pform.loc source)
      [ Pp.textf "Unknown ocaml configuration variable %S" name ]

let expand_env t ~source s : Value.t list option =
  match String.rsplit2 s ~on:'=' with
  | None ->
    User_error.raise ~loc:source.Dune_lang.Template.Pform.loc
      [ Pp.textf "%s must always come with a default value."
          (Dune_lang.Template.Pform.describe source)
      ]
      ~hints:[ Pp.text "the syntax is %{env:VAR=DEFAULT-VALUE}" ]
  | Some (var, default) ->
    if Env.Var.Set.mem t.hidden_env var then
      None
    else
      Some [ String (Option.value ~default (Env.get t.env var)) ]

let expand_version scope ~source s =
  let value_from_version = function
    | None -> [ Value.String "" ]
    | Some s -> [ String s ]
  in
  match
    Package.Name.Map.find
      (Dune_project.packages (Scope.project scope))
      (Package.Name.of_string s)
  with
  | Some p -> value_from_version p.version
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

let isn't_allowed_in_this_position_message ~source _pform =
  User_error.make ~loc:source.Dune_lang.Template.Pform.loc
    [ Pp.textf "%s isn't allowed in this position"
        (Dune_lang.Template.Pform.describe source)
    ]

let isn't_allowed_in_this_position ~source pform =
  raise (User_error.E (isn't_allowed_in_this_position_message ~source pform))

let expand_var_exn t ~source pform =
  t.expand_var t ~source pform
  |> Expanded.to_value_opt ~on_deferred:isn't_allowed_in_this_position

let expand_artifact ~dir ~source t a s : Expanded.t =
  let path = Path.Build.relative dir s in
  let name = Path.Build.basename path in
  let dir = Path.Build.parent_exn path in
  match t.lookup_artifacts with
  | None -> Unknown
  | Some lookup -> (
    let does_not_exist ~loc ~what name =
      Expanded.Error
        (User_error.make ~loc [ Pp.textf "%s %s does not exist." what name ])
    in
    let artifacts = lookup ~dir in
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
      | Some (t, m) ->
        Value
          (match Obj_dir.Module.cm_file t m ~kind with
          | None -> [ Value.String "" ]
          | Some path -> [ Value.Path (Path.build path) ]))
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
        Value (Value.L.paths (List.map ~f:Path.build archives))))

let path_exp path = [ Value.Path path ]

let str_exp str = [ Value.String str ]

let path p = Expanded.Value (path_exp p)

let string s = Expanded.Value (str_exp s)

let strings l = Expanded.Value (Value.L.strings l)

let get_prog = function
  | Ok p -> Expanded.Value (path_exp p)
  | Error err -> Error (Action.Prog.Not_found.user_message err)

let c_compiler_and_flags (context : Context.t) =
  Ocaml_config.c_compiler context.ocaml_config
  :: Ocaml_config.ocamlc_cflags context.ocaml_config

let common_static_expand ~ocaml_config ~(context : Context.t) ~source
    (pform : Pform.t) =
  match pform with
  | Var Ocaml -> Some (get_prog context.ocaml)
  | Var Ocamlc -> Some (path context.ocamlc)
  | Var Ocamlopt -> Some (get_prog context.ocamlopt)
  | Var Make ->
    Some
      (match context.which "make" with
      | None ->
        Error
          (Utils.program_not_found_message ~context:context.name
             ~loc:(Some (Dune_lang.Template.Pform.loc source))
             "make")
      | Some p -> path p)
  | Var Cpp -> Some (strings (c_compiler_and_flags context @ [ "-E" ]))
  | Var Pa_cpp ->
    Some
      (strings
         (c_compiler_and_flags context
         @ [ "-undef"; "-traditional"; "-x"; "c"; "-E" ]))
  | Var Arch_sixtyfour -> Some (string (string_of_bool context.arch_sixtyfour))
  | Var Ocaml_bin_dir -> Some (Value [ Dir context.ocaml_bin ])
  | Var Ocaml_version ->
    Some (string (Ocaml_config.version_string context.ocaml_config))
  | Var Ocaml_stdlib_dir -> Some (string (Path.to_string context.stdlib_dir))
  | Var Dev_null -> Some (string (Path.to_string Config.dev_null))
  | Var Ext_obj -> Some (string context.lib_config.ext_obj)
  | Var Ext_asm -> Some (string (Ocaml_config.ext_asm context.ocaml_config))
  | Var Ext_lib -> Some (string context.lib_config.ext_lib)
  | Var Ext_dll -> Some (string context.lib_config.ext_dll)
  | Var Ext_exe -> Some (string (Ocaml_config.ext_exe context.ocaml_config))
  | Var Ext_plugin ->
    Some
      (string
         (Mode.plugin_ext
            (if Ocaml_config.natdynlink_supported context.ocaml_config then
              Mode.Native
            else
              Mode.Byte)))
  | Var Profile -> Some (string (Profile.to_string context.profile))
  | Var Workspace_root ->
    Some (Value [ Value.Dir (Path.build context.build_dir) ])
  | Var Context_name -> Some (string (Context_name.to_string context.name))
  | Var Os_type ->
    Some
      (string
         (Ocaml_config.Os_type.to_string
            (Ocaml_config.os_type context.ocaml_config)))
  | Var Architecture ->
    Some (string (Ocaml_config.architecture context.ocaml_config))
  | Var System -> Some (string (Ocaml_config.system context.ocaml_config))
  | Var Model -> Some (string (Ocaml_config.model context.ocaml_config))
  | Var Ignoring_promoted_rules ->
    Some (string (string_of_bool !Clflags.ignore_promoted_rules))
  | Macro (Ocaml_config, s) ->
    Some (Value (expand_ocaml_config (Lazy.force ocaml_config) ~source s))
  | Var (Library_name | Impl_files | Intf_files | Input_file | Test) ->
    (* These are always set locally. If they are not part of [bindings], then
       they were used in a context where they shouldn't. *)
    Some (Error (isn't_allowed_in_this_position_message ~source pform))
  | _ -> None

(* This expansion function only expands the most "static" variables and macros.
   These are all known without building anything, evaluating any dune files, and
   they do not introduce any dependencies. *)
let static_expand
    ({ ocaml_config; bindings; dir; scope; artifacts_dynamic; context; _ } as t)
    ~source (pform : Pform.t) : Expanded.t =
  match Pform.Map.find bindings pform with
  | Some x -> Expanded.Value x
  | None -> (
    match common_static_expand ~ocaml_config ~context ~source pform with
    | Some x -> x
    | None -> (
      match pform with
      | Var Project_root -> Value [ Value.Dir (Path.build (Scope.root scope)) ]
      | Macro (Env, s) -> Expanded.of_value_opt (expand_env t ~source s)
      | Macro (Version, s) -> Value (expand_version scope ~source s)
      | Macro (Artifact a, s) when not artifacts_dynamic ->
        expand_artifact ~dir ~source t a s
      | pform -> Deferred (source, pform)))

let make ~scope ~scope_host ~(context : Context.t) ~lib_artifacts
    ~lib_artifacts_host ~bin_artifacts_host ~find_package =
  let ocaml_config = lazy (make_ocaml_config context.ocaml_config) in
  { dir = context.build_dir
  ; hidden_env = Env.Var.Set.empty
  ; env = context.env
  ; ocaml_config
  ; bindings = Pform.Map.empty
  ; scope
  ; scope_host
  ; lib_artifacts
  ; lib_artifacts_host
  ; bin_artifacts_host
  ; expand_var = static_expand
  ; c_compiler = Ocaml_config.c_compiler context.ocaml_config
  ; context
  ; artifacts_dynamic = false
  ; lookup_artifacts = None
  ; (* For dependency field expansion, we do not expand dependencies to the host
       context *)
    map_exe = Context.map_exe context
  ; foreign_flags =
      (fun ~dir ->
        Code_error.raise "foreign flags expander is not set"
          [ ("dir", Path.Build.to_dyn dir) ])
  ; find_package
  }

let expand t ~mode ~template =
  String_with_vars.expand ~dir:(Path.build t.dir) ~mode template
    ~f:(expand_var_exn t)

let expand_path t sw =
  expand t ~mode:Single ~template:sw
  |> Value.to_path ~error_loc:(String_with_vars.loc sw) ~dir:(Path.build t.dir)

let expand_str t sw =
  expand t ~mode:Single ~template:sw |> Value.to_string ~dir:(Path.build t.dir)

module Or_exn = struct
  let expand t ~mode ~template =
    match
      String_with_vars.expand ~dir:(Path.build t.dir) ~mode template
        ~f:(expand_var_exn t)
    with
    | x -> Ok x
    | exception (User_error.E _ as e) -> Error e

  let expand_path t sw =
    expand t ~mode:Single ~template:sw
    |> Result.map
         ~f:
           (Value.to_path ~error_loc:(String_with_vars.loc sw)
              ~dir:(Path.build t.dir))

  let expand_str t sw =
    expand t ~mode:Single ~template:sw
    |> Result.map ~f:(Value.to_string ~dir:(Path.build t.dir))
end

type reduced_var_result =
  | Unknown
  | Restricted
  | Expanded of Value.t list

let expand_with_reduced_var_set ~(context : Context.t) =
  let ocaml_config = lazy (make_ocaml_config context.ocaml_config) in
  fun ~source pform ->
    match common_static_expand ~ocaml_config ~context ~source pform with
    | None -> Restricted
    | Some x -> (
      match x with
      | Value x -> Expanded x
      | Error e -> raise (User_error.E e)
      | Deferred (source, pform) -> isn't_allowed_in_this_position ~source pform
      | Unknown -> Unknown)

module Resolved_forms = struct
  type t =
    { (* Failed resolutions *)
      mutable failure : Import.fail option
    ; (* All "name" for %{lib:name:...}/%{lib-available:name} forms *)
      mutable lib_deps : Lib_deps_info.t
    ; (* Static deps from %{...} variables. For instance %{exe:...} *)
      mutable sdeps : Path.Set.t
    ; (* Dynamic deps from %{...} variables. For instance %{read:...} *)
      mutable ddeps : Value.t list Action_builder.t Pform.Map.t
    }

  let create () =
    { failure = None
    ; lib_deps = Lib_name.Map.empty
    ; sdeps = Path.Set.empty
    ; ddeps = Pform.Map.empty
    }

  let add_lib_dep acc lib kind =
    acc.lib_deps <- Lib_name.Map.set acc.lib_deps lib kind

  let add_value_opt t =
    Option.iter ~f:(fun v ->
        t.sdeps <-
          Path.Set.union (Path.Set.of_list (Value.L.deps_only v)) t.sdeps)

  let to_build t =
    let open Action_builder.O in
    let ddeps = Pform.Map.to_list t.ddeps in
    let+ () = Action_builder.label (Lib_deps_info.Label t.lib_deps)
    and+ () = Action_builder.path_set t.sdeps
    and+ values = Action_builder.all (List.map ddeps ~f:snd)
    and+ () =
      match t.failure with
      | None -> Action_builder.return ()
      | Some fail -> Action_builder.fail fail
    in
    List.fold_left2 ddeps values ~init:Pform.Map.empty
      ~f:(fun acc (var, _) value -> Pform.Map.add_exn acc var value)
end

let parse_lib_file ~loc s =
  match String.lsplit2 s ~on:':' with
  | None -> User_error.raise ~loc [ Pp.textf "invalid %%{lib:...} form: %s" s ]
  | Some (lib, f) -> (Lib_name.parse_string_exn (loc, lib), f)

let cc t ~dir =
  let open Action_builder.O in
  let cc = t.foreign_flags ~dir in
  Foreign_language.Dict.map cc ~f:(fun cc ->
      let+ flags = cc in
      Value.L.strings (t.c_compiler :: flags))

type expand_result =
  | Static of Value.t list
  | Dynamic of Value.t list Action_builder.t

let expand_and_record_generic acc ~dep_kind ~(dir : Path.Build.t) t ~source
    (pform : Pform.t) =
  let relative d s =
    Path.build
      (Path.Build.relative ~error_loc:(Dune_lang.Template.Pform.loc source) d s)
  in
  let open Action_builder.O in
  match pform with
  | Var Cc -> Dynamic (cc t ~dir).c
  | Var Cxx -> Dynamic (cc t ~dir).cxx
  | Macro (Artifact a, s) ->
    let data =
      Action_builder.dyn_paths
        (let+ values =
           Action_builder.delayed (fun () ->
               match expand_artifact ~dir ~source t a s with
               | Value v -> v
               | Error msg -> raise (User_error.E msg)
               | Unknown
               | Deferred _ ->
                 isn't_allowed_in_this_position ~source pform)
         in
         (values, Value.L.deps_only values))
    in
    Dynamic data
  | Macro (Path_no_dep, s) -> Static [ Value.Dir (relative dir s) ]
  | Macro (Exe, s) -> Static (path_exp (t.map_exe (relative dir s)))
  | Macro (Dep, s) -> Static (path_exp (relative dir s))
  | Macro (Bin, s) ->
    Static
      (Artifacts.Bin.binary
         ~loc:(Some (Dune_lang.Template.Pform.loc source))
         t.bin_artifacts_host s
      |> Action.Prog.ok_exn |> path_exp)
  | Macro (Lib { lib_exec; lib_private }, s) -> (
    let lib, file =
      parse_lib_file ~loc:(Dune_lang.Template.Pform.loc source) s
    in
    Resolved_forms.add_lib_dep acc lib dep_kind;
    let scope =
      if lib_exec then
        t.scope_host
      else
        t.scope
    in
    match
      if lib_private then
        let open Result.O in
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
          Ok (Path.relative (Lib_info.src_dir (Lib.info lib)) file)
        else
          Error
            (User_error.E
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
                  ]))
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
    with
    | Ok path ->
      if (not lib_exec) || (not Sys.win32) || Filename.extension s = ".exe" then
        Static (path_exp path)
      else
        let path_exe = Path.extend_basename path ~suffix:".exe" in
        let dep =
          Action_builder.if_file_exists path_exe
            ~then_:
              (let+ () = Action_builder.path path_exe in
               path_exp path_exe)
            ~else_:
              (let+ () = Action_builder.path path in
               path_exp path)
        in
        Dynamic dep
    | Error e ->
      raise
        (match lib_private with
        | true -> e
        | false ->
          if Lib.DB.available (Scope.libs scope) lib then
            User_error.E
              (User_error.make
                 ~loc:(Dune_lang.Template.Pform.loc source)
                 [ Pp.textf
                     "The library %S is not public. The variable \"lib%s\" \
                      expands to the file's installation path which is not \
                      defined for private libraries."
                     (Lib_name.to_string lib)
                     (if lib_exec then
                       "exec"
                     else
                       "")
                 ])
          else
            e))
  | Macro (Lib_available, s) ->
    let lib =
      Lib_name.parse_string_exn (Dune_lang.Template.Pform.loc source, s)
    in
    Resolved_forms.add_lib_dep acc lib Optional;
    Static
      (Lib.DB.available (Scope.libs t.scope) lib |> string_of_bool |> str_exp)
  | Macro (Read, s) ->
    let path = relative dir s in
    let data =
      let+ s = Action_builder.contents path in
      [ Value.String s ]
    in
    Dynamic data
  | Macro (Read_lines, s) ->
    let path = relative dir s in
    let data =
      Action_builder.map (Action_builder.lines_of path) ~f:Value.L.strings
    in
    Dynamic data
  | Macro (Read_strings, s) ->
    let path = relative dir s in
    let data =
      Action_builder.map (Action_builder.strings path) ~f:Value.L.strings
    in
    Dynamic data
  | _ -> assert false

let gen_with_record_deps ~expand t resolved_forms ~dep_kind =
  let expand_var =
    expand
      (* we keep the dir constant here to replicate the old behavior of: (chdir
         foo %{exe:bar}). This should lookup ./bar rather than ./foo/bar *)
      resolved_forms ~dir:t.dir ~dep_kind ~expand_var:t.expand_var
  in
  { t with expand_var }

module Deps_like : sig
  val expand_deps_like_field :
       t
    -> dep_kind:Lib_deps_info.Kind.t
    -> f:(t -> 'a Action_builder.t)
    -> 'a Action_builder.t
end = struct
  let expand_and_record_static acc ~dep_kind ~(dir : Path.Build.t) t ~source
      pform =
    match expand_and_record_generic acc ~dep_kind ~dir t ~source pform with
    | Static l -> l
    | Dynamic _ -> isn't_allowed_in_this_position ~source pform

  let expand_no_ddeps acc ~dir ~dep_kind ~expand_var t ~source pform =
    let res =
      expand_var t ~source pform
      |> Expanded.to_value_opt ~on_deferred:(fun ~source pform ->
             Some (expand_and_record_static acc ~dep_kind ~dir t ~source pform))
    in
    Resolved_forms.add_value_opt acc res;
    Expanded.of_value_opt res

  let expand_deps_like_field t ~dep_kind ~f =
    let open Action_builder.O in
    let forms = Resolved_forms.create () in
    let t = gen_with_record_deps ~expand:expand_no_ddeps t forms ~dep_kind in
    let t = { t with map_exe = Fun.id } in
    let build = f t in
    let+ x = build
    and+ dynamic_expansions = Resolved_forms.to_build forms in
    if Pform.Map.is_empty dynamic_expansions then
      x
    else
      Code_error.raise "ddeps are not allowed in this position" []
end

include Deps_like

module Action_like : sig
  val expand_action :
       t
    -> deps_written_by_user:Path.t Bindings.t Action_builder.t
    -> targets_written_by_user:Targets.Or_forbidden.t
    -> dep_kind:Lib_deps_info.Kind.t
    -> partial:(t -> 'a)
    -> final:(t -> 'a -> 'b)
    -> 'a * 'b Action_builder.t
end = struct
  (* Expand variables that correspond to user defined variables, deps, and
     target(s) fields *)
  let expand_special_vars ~deps_written_by_user ~source:_ pform =
    match pform with
    | Pform.Var (User_var var) -> (
      match Bindings.find deps_written_by_user var with
      | None ->
        Code_error.raise "Local named variable not present in named deps"
          [ ("pform", Pform.to_dyn pform)
          ; ( "deps_written_by_user"
            , Bindings.to_dyn Path.to_dyn deps_written_by_user )
          ]
      | Some x -> Value.L.paths x)
    | Var Deps -> deps_written_by_user |> Bindings.to_list |> Value.L.paths
    | Var First_dep ->
      (* This case is for %{<} which was only allowed inside jbuild files *)
      assert false
    | _ ->
      Code_error.raise "Unexpected percent form in step2"
        [ ("pform", Pform.to_dyn pform) ]

  (* Responsible for the 2nd phase expansions of dynamic dependencies. After
     we've discovered all Action_builder.t values, waited for them to build, we
     can finally substitute them back form the dynamic_expansions map *)
  let expand_ddeps_and_bindings ~(dynamic_expansions : Value.t list Pform.Map.t)
      ~(deps_written_by_user : Path.t Bindings.t) ~expand_var t ~source pform =
    (match Pform.Map.find dynamic_expansions pform with
    | Some v -> Some v
    | None ->
      expand_var t ~source pform
      |> Expanded.to_value_opt ~on_deferred:(fun ~source pform ->
             Some (expand_special_vars ~deps_written_by_user ~source pform)))
    |> Expanded.of_value_opt

  let add_ddeps_and_bindings t ~dynamic_expansions ~deps_written_by_user =
    let expand_var =
      expand_ddeps_and_bindings ~dynamic_expansions ~deps_written_by_user
        ~expand_var:t.expand_var
    in
    { t with expand_var }

  (* Expansion where every Dynamic value is not expanded and recorded to be
     substituted later. *)
  let expand_and_record_dynamic acc ~dep_kind ~(dir : Path.Build.t) t ~source
      pform =
    match expand_and_record_generic acc ~dep_kind ~dir t ~source pform with
    | Static l -> Some l
    | Dynamic dep ->
      acc.ddeps <- Pform.Map.set acc.ddeps pform dep;
      None
    | exception (User_error.E _ as e) ->
      acc.failure <- Some { fail = (fun () -> raise e) };
      None

  let expand_and_record_deps acc ~(dir : Path.Build.t) ~dep_kind
      ~targets_written_by_user ~expand_var t
      ~(source : Dune_lang.Template.Pform.t) pform =
    let res =
      let targets ~(multiplicity : Targets.Multiplicity.t) =
        match (targets_written_by_user : Targets.Or_forbidden.t) with
        | Targets.Or_forbidden.Targets Infer ->
          User_error.raise ~loc:source.loc
            [ Pp.textf "You cannot use %s with inferred rules."
                (Dune_lang.Template.Pform.describe source)
            ]
        | Forbidden context ->
          User_error.raise ~loc:source.loc
            [ Pp.textf "You cannot use %s in %s." context
                (Dune_lang.Template.Pform.describe source)
            ]
        | Targets.Or_forbidden.Targets
            (Static { targets; multiplicity = field_multiplicity }) ->
          Targets.Multiplicity.check_variable_matches_field ~loc:source.loc
            ~field:field_multiplicity ~variable:multiplicity;
          (* XXX hack to signal no dep *)
          List.map ~f:Path.build targets |> Value.L.dirs
      in
      expand_var t ~source pform
      |> Expanded.to_value_opt ~on_deferred:(fun ~source pform ->
             match pform with
             | Var (First_dep | Deps | User_var _) -> None
             | Var Targets -> Some (targets ~multiplicity:Multiple)
             | Var Target -> Some (targets ~multiplicity:One)
             | _ -> expand_and_record_dynamic acc ~dep_kind ~dir t ~source pform)
    in
    Resolved_forms.add_value_opt acc res;
    Expanded.of_value_opt res

  let expand_action t ~deps_written_by_user ~targets_written_by_user ~dep_kind
      ~partial ~final =
    let open Action_builder.O in
    let forms = Resolved_forms.create () in
    let x =
      let expand = expand_and_record_deps ~targets_written_by_user in
      let t = gen_with_record_deps ~expand t forms ~dep_kind in
      partial t
    in
    let y =
      let+ dynamic_expansions = Resolved_forms.to_build forms
      and+ deps_written_by_user = deps_written_by_user in
      let t =
        add_ddeps_and_bindings t ~dynamic_expansions ~deps_written_by_user
      in
      final t x
    in
    (x, y)
end

include Action_like

let expand_and_eval_set t set ~standard =
  let open Action_builder.O in
  let dir = Path.build (dir t) in
  let standard =
    if Ordered_set_lang.Unexpanded.has_special_forms set then
      standard
    else
      Action_builder.return []
  in
  let files =
    let f template =
      expand t ~mode:Single ~template
      |> Value.to_path ~error_loc:(String_with_vars.loc template) ~dir
    in
    Ordered_set_lang.Unexpanded.files set ~f
  in
  let expand =
    let f template = expand t ~mode:Many ~template in
    Ordered_set_lang.Unexpanded.expand ~dir ~f
  in
  let eval =
    let parse ~loc:_ s = s in
    Ordered_set_lang.eval ~parse ~eq:String.equal
  in
  match Path.Set.to_list files with
  | [] ->
    let set = expand set ~files_contents:Path.Map.empty in
    let+ standard = standard in
    eval set ~standard
  | paths ->
    let+ standard = standard
    and+ sexps =
      Action_builder.all (List.map paths ~f:Action_builder.read_sexp)
    in
    let files_contents = List.combine paths sexps |> Path.Map.of_list_exn in
    expand set ~files_contents |> eval ~standard

let eval_blang t = function
  | Blang.Const x -> x (* common case *)
  | blang -> Blang.eval blang ~dir:(Path.build t.dir) ~f:(expand_var_exn t)

let map_exe t = t.map_exe

let find_package t pkg = t.find_package pkg
