open Import
open Jbuild

module A = Action
module Alias = Build_system.Alias

module Dir_with_jbuild = struct
  type t =
    { src_dir : Path.t
    ; ctx_dir : Path.t
    ; stanzas : Stanzas.t
    ; scope   : Scope.t
    ; kind    : File_tree.Dune_file.Kind.t
    }
end

module Installable = struct
  type t =
    { dir    : Path.t
    ; scope  : Scope.t
    ; stanza : Stanza.t
    ; kind   : File_tree.Dune_file.Kind.t
    }
end

module Env_node = struct
  type t =
    { dir                 : Path.t
    ; inherit_from        : t Lazy.t option
    ; scope               : Scope.t
    ; config              : Dune_env.Stanza.t
    ; mutable ocaml_flags : Ocaml_flags.t option
    }
end

type t =
  { context                          : Context.t
  ; build_system                     : Build_system.t
  ; scopes                           : Scope.DB.t
  ; public_libs                      : Lib.DB.t
  ; installed_libs                   : Lib.DB.t
  ; stanzas                          : Dir_with_jbuild.t list
  ; stanzas_per_dir                  : Dir_with_jbuild.t Path.Map.t
  ; packages                         : Package.t Package.Name.Map.t
  ; file_tree                        : File_tree.t
  ; artifacts                        : Artifacts.t
  ; stanzas_to_consider_for_install  : Installable.t list
  ; cxx_flags                        : string list
  ; pforms                           : Pform.Map.t
  ; ocaml_config                     : Value.t list String.Map.t
  ; chdir                            : (Action.t, Action.t) Build.t
  ; host                             : t option
  ; libs_by_package : (Package.t * Lib.Set.t) Package.Name.Map.t
  ; env                              : (Path.t, Env_node.t) Hashtbl.t
  }

let context t = t.context
let stanzas t = t.stanzas
let stanzas_in t ~dir = Path.Map.find t.stanzas_per_dir dir
let packages t = t.packages
let libs_by_package t = t.libs_by_package
let artifacts t = t.artifacts
let file_tree t = t.file_tree
let stanzas_to_consider_for_install t = t.stanzas_to_consider_for_install
let cxx_flags t = t.cxx_flags
let build_dir t = t.context.build_dir
let profile t = t.context.profile
let build_system t = t.build_system

let host t = Option.value t.host ~default:t

let internal_lib_names t =
  List.fold_left t.stanzas ~init:String.Set.empty
    ~f:(fun acc { Dir_with_jbuild. stanzas; _ } ->
      List.fold_left stanzas ~init:acc ~f:(fun acc -> function
        | Library lib ->
          String.Set.add
            (match lib.public with
             | None -> acc
             | Some { name; _ } -> String.Set.add acc name)
            lib.name
        | _ -> acc))

let public_libs    t = t.public_libs
let installed_libs t = t.installed_libs

let find_scope_by_dir  t dir  = Scope.DB.find_by_dir  t.scopes dir
let find_scope_by_name t name = Scope.DB.find_by_name t.scopes name

let prefix_rules t prefix ~f =
  Build_system.prefix_rules t.build_system prefix ~f

let add_rule t ?sandbox ?mode ?locks ?loc build =
  let build = Build.O.(>>>) build t.chdir in
  Build_system.add_rule t.build_system
    (Build_interpret.Rule.make ?sandbox ?mode ?locks ?loc
       ~context:(Some t.context) build)

let add_rule_get_targets t ?sandbox ?mode ?locks ?loc build =
  let build = Build.O.(>>>) build t.chdir in
  let rule =
    Build_interpret.Rule.make ?sandbox ?mode ?locks ?loc
      ~context:(Some t.context) build
  in
  Build_system.add_rule t.build_system rule;
  List.map rule.targets ~f:Build_interpret.Target.path

let add_rules t ?sandbox builds =
  List.iter builds ~f:(add_rule t ?sandbox)

let add_alias_deps t alias ?dyn_deps deps =
  Alias.add_deps t.build_system alias ?dyn_deps deps

let add_alias_action t alias ?locks ~stamp action =
  Alias.add_action t.build_system ~context:t.context alias ?locks ~stamp action

let eval_glob t ~dir re = Build_system.eval_glob t.build_system ~dir re
let load_dir t ~dir = Build_system.load_dir t.build_system ~dir
let on_load_dir t ~dir ~f = Build_system.on_load_dir t.build_system ~dir ~f

let source_files t ~src_path =
  match File_tree.find_dir t.file_tree src_path with
  | None -> String.Set.empty
  | Some dir -> File_tree.Dir.files dir


let expand_ocaml_config t pform name =
  match String.Map.find t.ocaml_config name with
  | Some x -> x
  | None ->
    Loc.fail (String_with_vars.Var.loc pform)
      "Unknown ocaml configuration variable %S"
      name

let expand_vars t ~mode ~scope ~dir ?(bindings=Pform.Map.empty) s =
  String_with_vars.expand ~mode ~dir s ~f:(fun pform syntax_version ->
    (match Pform.Map.expand bindings pform syntax_version with
     | None -> Pform.Map.expand t.pforms pform syntax_version
     | Some _ as x -> x)
    |> Option.map ~f:(function
      | Pform.Expansion.Var (Values l) -> l
      | Macro (Ocaml_config, s) -> expand_ocaml_config t pform s
      | Var Project_root -> [Value.Dir (Scope.root scope)]
      | _ ->
        Loc.fail (String_with_vars.Var.loc pform)
          "%s isn't allowed in this position"
          (String_with_vars.Var.describe pform)))

let expand_vars_string t ~scope ~dir ?bindings s =
  expand_vars t ~mode:Single ~scope ~dir ?bindings s
  |> Value.to_string ~dir

let expand_vars_path t ~scope ~dir ?bindings s =
  expand_vars t ~mode:Single ~scope ~dir ?bindings s
  |> Value.to_path ~error_loc:(String_with_vars.loc s) ~dir

type targets =
  | Static of Path.t list
  | Infer
  | Alias

module Pkg_version = struct
  open Build.O

  module V = Vfile_kind.Make(struct
      type t = string option
      let t = Sexp.To_sexp.(option string)
      let name = "Pkg_version"
    end)

  let spec sctx (p : Package.t) =
    let fn =
      Path.relative (Path.append sctx.context.build_dir p.path)
        (sprintf "%s.version.sexp" (Package.Name.to_string p.name))
    in
    Build.Vspec.T (fn, (module V))

  let read sctx p = Build.vpath (spec sctx p)

  let set sctx p get =
    let spec = spec sctx p in
    add_rule sctx (get >>> Build.store_vfile spec);
    Build.vpath spec
end

module Expander : sig
  module Resolved_forms : sig
    type t

    (* Failed resolutions *)
    val failures : t -> fail list

    (* All "name" for %{lib:name:...}/%{lib-available:name} forms *)
    val lib_deps : t -> Build.lib_deps

    (* Static deps from %{...} variables. For instance %{exe:...} *)
    val sdeps    : t -> Path.Set.t

    (* Dynamic deps from %{...} variables. For instance %{read:...} *)
    val ddeps    : t -> (unit, Value.t list) Build.t String.Map.t
  end

  type sctx = t

  val with_expander
    :  sctx
    -> dir:Path.t
    -> dep_kind:Build.lib_dep_kind
    -> scope:Scope.t
    -> targets_written_by_user:targets
    -> map_exe:(Path.t -> Path.t)
    -> bindings:Pform.Map.t
    -> f:(Value.t list option String_with_vars.expander -> 'a)
    -> 'a * Resolved_forms.t
end = struct
  module Resolved_forms = struct
    type t =
      { (* Failed resolutions *)
        mutable failures  : fail list
      ; (* All "name" for %{lib:name:...}/%{lib-available:name} forms *)
        mutable lib_deps  : Build.lib_deps
      ; (* Static deps from %{...} variables. For instance %{exe:...} *)
        mutable sdeps     : Path.Set.t
      ; (* Dynamic deps from %{...} variables. For instance %{read:...} *)
        mutable ddeps     : (unit, Value.t list) Build.t String.Map.t
      }

    let failures t = t.failures
    let lib_deps t = t.lib_deps
    let sdeps t = t.sdeps
    let ddeps t = t.ddeps

    let empty () =
      { failures  = []
      ; lib_deps  = String.Map.empty
      ; sdeps     = Path.Set.empty
      ; ddeps     = String.Map.empty
      }

    let add_lib_dep acc lib kind =
      acc.lib_deps <- String.Map.add acc.lib_deps lib kind

    let add_fail acc fail =
      acc.failures <- fail :: acc.failures;
      None

    let add_ddep acc ~key dep =
      acc.ddeps <- String.Map.add acc.ddeps key dep;
      None
  end

  type sctx = t

  let path_exp path = [Value.Path path]
  let str_exp  str  = [Value.String str]

  let parse_lib_file ~loc s =
    match String.lsplit2 s ~on:':' with
    | None ->
      Loc.fail loc "invalid %%{lib:...} form: %s" s
    | Some x -> x

  open Build.O

  let expander ~acc sctx ~dir ~dep_kind ~scope ~targets_written_by_user
        ~map_exe ~bindings pform syntax_version =
    let loc = String_with_vars.Var.loc pform in
    let key = String_with_vars.Var.full_name pform in
    let res =
      Pform.Map.expand bindings pform syntax_version
      |> Option.bind ~f:(function
        | Pform.Expansion.Var (Values l) -> Some l
        | Macro (Ocaml_config, s) -> Some (expand_ocaml_config sctx pform s)
        | Var Project_root -> Some [Value.Dir (Scope.root scope)]
        | Var (First_dep | Deps | Named_local) -> None
        | Var Targets ->
          begin match targets_written_by_user with
          | Infer ->
            Loc.fail loc "You cannot use %s with inferred rules."
              (String_with_vars.Var.describe pform)
          | Alias ->
            Loc.fail loc "You cannot use %s in aliases."
              (String_with_vars.Var.describe pform)
          | Static l ->
            Some (Value.L.dirs l) (* XXX hack to signal no dep *)
          end
        | Macro (Exe, s) -> Some (path_exp (map_exe (Path.relative dir s)))
        | Macro (Dep, s) -> Some (path_exp (Path.relative dir s))
        | Macro (Bin, s) -> begin
            let sctx = host sctx in
            match Artifacts.binary (artifacts sctx) s with
            | Ok path -> Some (path_exp path)
            | Error e ->
              Resolved_forms.add_fail acc
                ({ fail = fun () -> Action.Prog.Not_found.raise e })
          end
        | Macro (Lib, s) -> begin
            let lib_dep, file = parse_lib_file ~loc s in
            Resolved_forms.add_lib_dep acc lib_dep dep_kind;
            match
              Artifacts.file_of_lib (artifacts sctx) ~loc ~lib:lib_dep ~file
            with
            | Ok path -> Some (path_exp path)
            | Error fail -> Resolved_forms.add_fail acc fail
          end
        | Macro (Libexec, s) -> begin
            let sctx = host sctx in
            let lib_dep, file = parse_lib_file ~loc s in
            Resolved_forms.add_lib_dep acc lib_dep dep_kind;
            match
              Artifacts.file_of_lib (artifacts sctx) ~loc ~lib:lib_dep ~file
            with
            | Error fail -> Resolved_forms.add_fail acc fail
            | Ok path ->
              if not Sys.win32 || Filename.extension s = ".exe" then begin
                Some (path_exp path)
              end else begin
                let path_exe = Path.extend_basename path ~suffix:".exe" in
                let dep =
                  Build.if_file_exists path_exe
                    ~then_:(Build.path path_exe >>^ fun _ ->
                            path_exp path_exe)
                    ~else_:(Build.path path >>^ fun _ ->
                            path_exp path)
                in
                Resolved_forms.add_ddep acc ~key dep
              end
          end
        | Macro (Lib_available, s) -> begin
            let lib = s in
            Resolved_forms.add_lib_dep acc lib Optional;
            Some (str_exp (string_of_bool (
              Lib.DB.available (Scope.libs scope) lib)))
          end
        | Macro (Version, s) -> begin
            match Package.Name.Map.find (Scope.project scope).packages
                    (Package.Name.of_string s) with
            | Some p ->
              let x =
                Pkg_version.read sctx p >>^ function
                | None   -> [Value.String ""]
                | Some s -> [String s]
              in
              Resolved_forms.add_ddep acc ~key x
            | None ->
              Resolved_forms.add_fail acc { fail = fun () ->
                Loc.fail loc
                  "Package %S doesn't exist in the current project." s
              }
          end
        | Macro (Read, s) -> begin
            let path = Path.relative dir s in
            let data =
              Build.contents path
              >>^ fun s -> [Value.String s]
            in
            Resolved_forms.add_ddep acc ~key data
          end
        | Macro (Read_lines, s) -> begin
            let path = Path.relative dir s in
            let data =
              Build.lines_of path
              >>^ Value.L.strings
            in
            Resolved_forms.add_ddep acc ~key data
          end
        | Macro (Read_strings, s) -> begin
            let path = Path.relative dir s in
            let data =
              Build.strings path
              >>^ Value.L.strings
            in
            Resolved_forms.add_ddep acc ~key data
          end
        | Macro (Path_no_dep, s) -> Some [Value.Dir (Path.relative dir s)])
    in
    Option.iter res ~f:(fun v ->
      acc.sdeps <- Path.Set.union
                     (Path.Set.of_list (Value.L.deps_only v)) acc.sdeps
    );
    res

  let with_expander sctx ~dir ~dep_kind ~scope ~targets_written_by_user
        ~map_exe ~bindings ~f =
    let acc = Resolved_forms.empty () in
    ( f (expander ~acc sctx ~dir ~dep_kind ~scope ~targets_written_by_user ~map_exe ~bindings)
    , acc
    )
end

let expand_and_eval_set t ~scope ~dir ?bindings set ~standard =
  let open Build.O in
  let parse ~loc:_ s = s in
  let (syntax, files) =
    let f = expand_vars_path t ~scope ~dir ?bindings in
    Ordered_set_lang.Unexpanded.files set ~f in
  let f = expand_vars t ~mode:Many ~scope ~dir ?bindings in
  match Path.Set.to_list files with
  | [] ->
    let set =
      Ordered_set_lang.Unexpanded.expand set ~dir
        ~files_contents:Path.Map.empty ~f
    in
    standard >>^ fun standard ->
    Ordered_set_lang.String.eval set ~standard ~parse
  | paths ->
    Build.fanout standard (Build.all (List.map paths ~f:(fun f ->
      Build.read_sexp f syntax)))
    >>^ fun (standard, sexps) ->
    let files_contents = List.combine paths sexps |> Path.Map.of_list_exn in
    let set = Ordered_set_lang.Unexpanded.expand set ~dir ~files_contents ~f in
    Ordered_set_lang.String.eval set ~standard ~parse

module Env : sig
  val ocaml_flags : t -> dir:Path.t -> Ocaml_flags.t
  val get : t -> dir:Path.t -> Env_node.t

  val jsoo_compilation : t -> dir:Path.t -> Dune_env.Stanza.Jsoo_compilation.t
end = struct
  open Env_node

  let rec get t ~dir =
    match Hashtbl.find t.env dir with
    | Some node -> node
    | None ->
      begin match Path.parent dir with
      | None -> raise_notrace Exit
      | Some parent ->
        let node = get t ~dir:parent in
        Hashtbl.add t.env dir node;
        node
      end

  let get t ~dir =
    match get t ~dir with
    | node -> node
    | exception Exit ->
      Exn.code_error "Super_context.Env.get called on invalid directory"
        [ "dir", Path.sexp_of_t dir ]

  let ocaml_flags t ~dir =
    let rec loop t node =
      match node.ocaml_flags with
      | Some x -> x
      | None ->
        let default =
          match node.inherit_from with
          | None -> Ocaml_flags.default ~profile:(profile t)
          | Some (lazy node) -> loop t node
        in
        let flags =
          match List.find_map node.config.rules ~f:(fun (pat, cfg) ->
            match (pat : Dune_env.Stanza.pattern), profile t with
            | Any, _ -> Some cfg
            | Profile a, b -> Option.some_if (a = b) cfg)
          with
          | None -> default
          | Some cfg ->
            Ocaml_flags.make
              ~flags:cfg.flags
              ~ocamlc_flags:cfg.ocamlc_flags
              ~ocamlopt_flags:cfg.ocamlopt_flags
              ~default
              ~eval:(expand_and_eval_set t ~scope:node.scope ~dir:node.dir
                       ?bindings:None)
        in
        node.ocaml_flags <- Some flags;
        flags
    in
    loop t (get t ~dir)

  let jsoo_compilation t ~dir =
    let rec loop t node =
      match List.find_map node.config.rules ~f:(fun (pat, cfg) ->
        match (pat : Dune_env.Stanza.pattern), profile t with
        | Any, _ -> Some cfg
        | Profile a, b -> Option.some_if (a = b) cfg)
      with
      | None ->
        begin match node.inherit_from with
        | None ->
          Dune_env.Stanza.Jsoo_compilation.default ~profile:(profile t)
        | Some (lazy node) -> loop t node
        end
      | Some cfg ->
        begin match cfg.jsoo_compilation with
        | None ->
          Dune_env.Stanza.Jsoo_compilation.default ~profile:(profile t)
        | Some jc -> jc
        end
    in
    loop t (get t ~dir)
end


let ocaml_flags t ~dir ~scope (x : Buildable.t) =
  Ocaml_flags.make
    ~flags:x.flags
    ~ocamlc_flags:x.ocamlc_flags
    ~ocamlopt_flags:x.ocamlopt_flags
    ~default:(Env.ocaml_flags t ~dir)
    ~eval:(expand_and_eval_set t ~scope ~dir ?bindings:None)

let dump_env t ~dir =
  Ocaml_flags.dump (Env.ocaml_flags t ~dir)

let resolve_program t ?hint bin =
  Artifacts.binary ?hint t.artifacts bin

let create
      ~(context:Context.t)
      ?host
      ~projects
      ~file_tree
      ~packages
      ~stanzas
      ~external_lib_deps_mode
      ~build_system
  =
  let installed_libs =
    Lib.DB.create_from_findlib context.findlib ~external_lib_deps_mode
  in
  let internal_libs =
    List.concat_map stanzas ~f:(fun { Jbuild_load.Jbuild. dir; stanzas; _ } ->
      let ctx_dir = Path.append context.build_dir dir in
      List.filter_map stanzas ~f:(fun stanza ->
        match (stanza : Stanza.t) with
        | Library lib -> Some (ctx_dir, lib)
        | _ -> None))
  in
  let scopes, public_libs =
    Scope.DB.create
      ~projects
      ~context:context.name
      ~installed_libs
      internal_libs
  in
  let stanzas =
    List.map stanzas
      ~f:(fun { Jbuild_load.Jbuild. dir; project; stanzas; kind } ->
        let ctx_dir = Path.append context.build_dir dir in
        { Dir_with_jbuild.
          src_dir = dir
        ; ctx_dir
        ; stanzas
        ; scope = Scope.DB.find_by_name scopes project.Dune_project.name
        ; kind
        })
  in
  let stanzas_per_dir =
    List.map stanzas ~f:(fun stanzas ->
      (stanzas.Dir_with_jbuild.ctx_dir, stanzas))
    |> Path.Map.of_list_exn
  in
  let stanzas_to_consider_for_install =
    if not external_lib_deps_mode then
      List.concat_map stanzas ~f:(fun { ctx_dir; stanzas; scope; kind; _ } ->
        List.filter_map stanzas ~f:(fun stanza ->
          let keep =
            match (stanza : Stanza.t) with
            | Library lib -> Lib.DB.available (Scope.libs scope) lib.name
            | Documentation _
            | Install _   -> true
            | _           -> false
          in
          Option.some_if keep { Installable.
                                dir = ctx_dir
                              ; scope
                              ; stanza
                              ; kind
                              }))
    else
      List.concat_map stanzas ~f:(fun { ctx_dir; stanzas; scope; kind; _ } ->
        List.map stanzas ~f:(fun stanza ->
          { Installable.
            dir = ctx_dir
          ; scope
          ; stanza
          ; kind
          }))
  in
  let artifacts =
    Artifacts.create context ~public_libs stanzas
      ~f:(fun (d : Dir_with_jbuild.t) -> d.stanzas)
  in
  let cxx_flags =
    List.filter context.ocamlc_cflags
      ~f:(fun s -> not (String.is_prefix s ~prefix:"-std="))
  in
  let pforms = Pform.Map.create ~context ~cxx_flags in
  let ocaml_config =
    let string s = [Value.String s] in
    Ocaml_config.to_list context.ocaml_config
    |> List.map  ~f:(fun (k, v) ->
      ( k
      , match (v : Ocaml_config.Value.t) with
      | Bool          x -> string (string_of_bool x)
      | Int           x -> string (string_of_int x)
      | String        x -> string x
      | Words         x -> Value.L.strings x
      | Prog_and_args x -> Value.L.strings (x.prog :: x.args)))
    |> String.Map.of_list_exn
  in
  let t =
    { context
    ; host
    ; build_system
    ; scopes
    ; public_libs
    ; installed_libs
    ; stanzas
    ; stanzas_per_dir
    ; packages
    ; file_tree
    ; stanzas_to_consider_for_install
    ; artifacts
    ; cxx_flags
    ; pforms
    ; ocaml_config
    ; chdir = Build.arr (fun (action : Action.t) ->
        match action with
        | Chdir _ -> action
        | _ -> Chdir (context.build_dir, action))
    ; libs_by_package =
        Lib.DB.all public_libs
        |> Lib.Set.to_list
        |> List.map ~f:(fun lib ->
          (Option.value_exn (Lib.package lib), lib))
        |> Package.Name.Map.of_list_multi
        |> Package.Name.Map.merge packages ~f:(fun _name pkg libs ->
          let pkg  = Option.value_exn pkg          in
          let libs = Option.value libs ~default:[] in
          Some (pkg, Lib.Set.of_list libs))
    ; env = Hashtbl.create 128
    }
  in
  let context_env_node = lazy (
    let make ~inherit_from ~config =
      { Env_node.
        dir = context.build_dir
      ; scope = Scope.DB.find_by_dir scopes context.build_dir
      ; ocaml_flags = None
      ; inherit_from
      ; config
      }
    in
    match context.env_nodes with
    | { context = None; workspace = None } ->
      make ~config:{ loc = Loc.none; rules = [] } ~inherit_from:None
    | { context = Some config; workspace = None }
    | { context = None; workspace = Some config } ->
      make ~config ~inherit_from:None
    | { context = Some context ; workspace = Some workspace } ->
      make ~config:context
        ~inherit_from:(Some (lazy (make ~inherit_from:None ~config:workspace)))
  ) in
  List.iter stanzas
    ~f:(fun { Dir_with_jbuild. ctx_dir; scope; stanzas; _ } ->
      List.iter stanzas ~f:(function
        | Dune_env.T config ->
          let inherit_from =
            if ctx_dir = Scope.root scope then
              context_env_node
            else
              lazy (Env.get t ~dir:(Path.parent_exn ctx_dir))
          in
          Hashtbl.add t.env ctx_dir
            { dir          = ctx_dir
            ; inherit_from = Some inherit_from
            ; scope        = scope
            ; config       = config
            ; ocaml_flags  = None
            }
        | _ -> ()));
  if not (Hashtbl.mem t.env context.build_dir) then
    Hashtbl.add t.env context.build_dir (Lazy.force context_env_node);
  t
module Libs = struct
  open Build.O

  let gen_select_rules t ~dir compile_info =
    List.iter (Lib.Compile.resolved_selects compile_info) ~f:(fun rs ->
      let { Lib.Compile.Resolved_select.dst_fn; src_fn } = rs in
      let dst = Path.relative dir dst_fn in
      add_rule t
        (match src_fn with
         | Ok src_fn ->
           let src = Path.relative dir src_fn in
           Build.copy_and_add_line_directive ~src ~dst
         | Error e ->
           Build.fail ~targets:[dst]
             { fail = fun () ->
                 raise (Lib.Error (No_solution_found_for_select e))
             }))

  let with_lib_deps t compile_info ~dir ~f =
    let prefix =
      Build.record_lib_deps (Lib.Compile.user_written_deps compile_info)
        ~kind:(if Lib.Compile.optional compile_info then
                 Optional
               else
                 Required)
    in
    let prefix =
      if t.context.merlin then
        Build.path (Path.relative dir ".merlin-exists")
        >>>
        prefix
      else
        prefix
    in
    prefix_rules t prefix ~f

  let lib_files_alias ~dir ~name ~ext =
    Alias.make (sprintf "lib-%s%s-all" name ext) ~dir

  let setup_file_deps_alias t ~dir ~ext lib files =
    add_alias_deps t
      (lib_files_alias ~dir ~name:(Library.best_name lib) ~ext) files

  let setup_file_deps_group_alias t ~dir ~exts lib =
    setup_file_deps_alias t lib ~dir
      ~ext:(String.concat exts ~sep:"-and-")
      (List.map exts ~f:(fun ext ->
         Alias.stamp_file
           (lib_files_alias ~dir ~name:(Library.best_name lib) ~ext))
       |> Path.Set.of_list)

  let file_deps t libs ~ext =
    List.rev_map libs ~f:(fun (lib : Lib.t) ->
      if Lib.is_local lib then
        Alias.stamp_file
          (lib_files_alias ~dir:(Lib.src_dir lib) ~name:(Lib.name lib) ~ext)
      else
        Build_system.stamp_file_for_files_of t.build_system
          ~dir:(Lib.obj_dir lib) ~ext)
end

module Deps = struct
  open Build.O
  open Dep_conf

  let make_alias t ~scope ~dir s =
    let loc = String_with_vars.loc s in
    Alias.of_user_written_path ~loc (expand_vars_path t ~scope ~dir s)

  let dep t ~scope ~dir = function
    | File  s ->
      let path = expand_vars_path t ~scope ~dir s in
      Build.path path
      >>^ fun () -> [path]
    | Alias s ->
      Alias.dep (make_alias t ~scope ~dir s)
      >>^ fun () -> []
    | Alias_rec s ->
      Alias.dep_rec ~loc:(String_with_vars.loc s) ~file_tree:t.file_tree
        (make_alias t ~scope ~dir s)
      >>^ fun () -> []
    | Glob_files s -> begin
        let loc = String_with_vars.loc s in
        let path = expand_vars_path t ~scope ~dir s in
        match Glob_lexer.parse_string (Path.basename path) with
        | Ok re ->
          let dir = Path.parent_exn path in
          Build.paths_glob ~loc ~dir (Re.compile re)
          >>^ Path.Set.to_list
        | Error (_pos, msg) ->
          Loc.fail (String_with_vars.loc s) "invalid glob: %s" msg
      end
    | Source_tree s ->
      let path = expand_vars_path t ~scope ~dir s in
      Build.source_tree ~dir:path ~file_tree:t.file_tree
      >>^ Path.Set.to_list
    | Package p ->
      let pkg = Package.Name.of_string (expand_vars_string t ~scope ~dir p) in
      Alias.dep (Alias.package_install ~context:t.context ~pkg)
      >>^ fun () -> []
    | Universe ->
      Build.path Build_system.universe_file
      >>^ fun () -> []

  let interpret t ~scope ~dir l =
    List.map l ~f:(dep t ~scope ~dir)
    |> Build.all
    >>^ List.concat

  let interpret_named t ~scope ~dir bindings =
    List.map bindings ~f:(function
      | Jbuild.Bindings.Unnamed p ->
        dep t ~scope ~dir p >>^ fun l ->
        List.map l ~f:(fun x -> Jbuild.Bindings.Unnamed x)
      | Named (s, ps) ->
        Build.all (List.map ps ~f:(dep t ~scope ~dir)) >>^ fun l ->
        [Jbuild.Bindings.Named (s, List.concat l)])
    |> Build.all
    >>^ List.concat
end

module Scope_key = struct
  let of_string sctx key =
    match String.rsplit2 key ~on:'@' with
    | None ->
      (key, public_libs sctx)
    | Some (key, scope) ->
      ( key
      , Scope.libs (find_scope_by_name sctx (Dune_project.Name.decode scope)))

  let to_string key scope =
    sprintf "%s@%s" key (Dune_project.Name.encode scope)
end

module Action = struct
  open Build.O
  module U = Action.Unexpanded

  type nonrec targets = targets =
    | Static of Path.t list
    | Infer
    | Alias


  let map_exe sctx =
    match sctx.host with
    | None -> (fun exe -> exe)
    | Some host ->
      fun exe ->
        match Path.extract_build_context_dir exe with
        | Some (dir, exe) when dir = sctx.context.build_dir ->
          Path.append host.context.build_dir exe
        | _ -> exe

  let expand_step1 sctx ~dir ~dep_kind ~scope ~targets_written_by_user
        ~map_exe ~bindings t =
    Expander.with_expander sctx ~dir ~dep_kind ~scope ~targets_written_by_user ~map_exe ~bindings
      ~f:(fun f -> U.partial_expand t ~dir ~map_exe ~f)

  let expand_step2 ~dir ~dynamic_expansions ~bindings
        ~(deps_written_by_user : Path.t Jbuild.Bindings.t)
        ~map_exe t =
    U.Partial.expand t ~dir ~map_exe ~f:(fun pform syntax_version ->
      let key = String_with_vars.Var.full_name pform in
      let loc = String_with_vars.Var.loc pform in
      match String.Map.find dynamic_expansions key with
      | Some _ as opt -> opt
      | None ->
        Option.map (Pform.Map.expand bindings pform syntax_version) ~f:(function
          | Var Named_local ->
            begin match Jbuild.Bindings.find deps_written_by_user key with
            | None ->
              Exn.code_error "Local named variable not present in named deps"
                [ "pform", String_with_vars.Var.sexp_of_t pform
                ; "deps_written_by_user",
                  Jbuild.Bindings.sexp_of_t Path.sexp_of_t deps_written_by_user
                ]
            | Some x -> Value.L.paths x
            end
          | Var Deps ->
            deps_written_by_user
            |> Jbuild.Bindings.to_list
            |> Value.L.paths
          | Var First_dep ->
            begin match deps_written_by_user with
            | Named _ :: _ ->
              (* This case is not possible: ${<} only exist in jbuild
                 files and named dependencies are not available in
                 jbuild files *)
              assert false
            | Unnamed v :: _ -> [Path v]
            | [] ->
              Loc.warn loc "Variable '%s' used with no explicit \
                            dependencies@." key;
              [Value.String ""]
            end
          | _ ->
            Exn.code_error "Unexpected variable in step2"
              ["var", String_with_vars.Var.sexp_of_t pform]))

  let run sctx ~loc ~bindings ~dir ~dep_kind
        ~targets:targets_written_by_user ~targets_dir ~scope t
    : (Path.t Bindings.t, Action.t) Build.t =
    let bindings = Pform.Map.superpose sctx.pforms bindings in
    let map_exe = map_exe sctx in
    if targets_written_by_user = Alias then begin
      match Action.Infer.unexpanded_targets t with
      | [] -> ()
      | x :: _ ->
        let loc = String_with_vars.loc x in
        Loc.warn loc
          "Aliases must not have targets, this target will be ignored.\n\
           This will become an error in the future.";
    end;
    let t, forms =
      expand_step1 sctx t ~dir ~dep_kind ~scope
        ~targets_written_by_user ~map_exe ~bindings
    in
    let { Action.Infer.Outcome. deps; targets } =
      match targets_written_by_user with
      | Infer -> Action.Infer.partial t ~all_targets:true
      | Static targets_written_by_user ->
        let targets_written_by_user = Path.Set.of_list targets_written_by_user in
        let { Action.Infer.Outcome. deps; targets } =
          Action.Infer.partial t ~all_targets:false
        in
        { deps; targets = Path.Set.union targets targets_written_by_user }
      | Alias ->
        let { Action.Infer.Outcome. deps; targets = _ } =
          Action.Infer.partial t ~all_targets:false
        in
        { deps; targets = Path.Set.empty }
    in
    let targets = Path.Set.to_list targets in
    List.iter targets ~f:(fun target ->
      if Path.parent_exn target <> targets_dir then
        Loc.fail loc
          "This action has targets in a different directory than the current \
           one, this is not allowed by dune at the moment:\n%s"
          (List.map targets ~f:(fun target ->
             sprintf "- %s" (Utils.describe_target target))
           |> String.concat ~sep:"\n"));
    let build =
      Build.record_lib_deps_simple (Expander.Resolved_forms.lib_deps forms)
      >>>
      Build.path_set (Path.Set.union deps (Expander.Resolved_forms.sdeps forms))
      >>>
      Build.arr (fun paths -> ((), paths))
      >>>
      let ddeps = String.Map.to_list (Expander.Resolved_forms.ddeps forms) in
      Build.first (Build.all (List.map ddeps ~f:snd))
      >>^ (fun (vals, deps_written_by_user) ->
        let dynamic_expansions =
          List.fold_left2 ddeps vals ~init:String.Map.empty
            ~f:(fun acc (var, _) value -> String.Map.add acc var value)
        in
        let unresolved =
          expand_step2 t ~dir ~dynamic_expansions ~deps_written_by_user ~map_exe
            ~bindings
        in
        Action.Unresolved.resolve unresolved ~f:(fun prog ->
          let sctx = host sctx in
          match Artifacts.binary sctx.artifacts prog with
          | Ok path    -> path
          | Error fail -> Action.Prog.Not_found.raise fail))
      >>>
      Build.dyn_path_set (Build.arr (fun action ->
        let { Action.Infer.Outcome.deps; targets = _ } =
          Action.Infer.infer action
        in
        deps))
      >>>
      Build.action_dyn () ~dir ~targets
    in
    match Expander.Resolved_forms.failures forms with
    | [] -> build
    | fail :: _ -> Build.fail fail >>> build
end
