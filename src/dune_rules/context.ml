open! Dune_engine
open! Stdune
open Import
open Memo.Build.O

module Kind = struct
  module Opam = struct
    type t =
      { root : string option
      ; switch : string
      }
  end

  type t =
    | Default
    | Opam of Opam.t

  let to_dyn : t -> Dyn.t = function
    | Default -> Dyn.Encoder.string "default"
    | Opam o ->
      Dyn.Encoder.(
        record [ ("root", option string o.root); ("switch", string o.switch) ])
end

module Env_nodes = struct
  type t =
    { context : Dune_env.Stanza.t
    ; workspace : Dune_env.Stanza.t
    }

  let extra_env ~profile env_nodes =
    Env.extend_env (Dune_env.Stanza.find env_nodes.context ~profile).env_vars
      (Dune_env.Stanza.find env_nodes.workspace ~profile).env_vars
end

module Program = struct
  module Name = String

  module Which_path = struct
    type t = Path.t option

    let equal = Option.equal Path.equal

    let to_dyn = Dyn.Encoder.option Path.to_dyn
  end

  let programs_for_which_we_prefer_opt_ext =
    [ "ocamlc"; "ocamldep"; "ocamlmklib"; "ocamlobjinfo"; "ocamlopt" ]

  let best_path dir program =
    let exe_path program =
      let fn = Path.relative dir (program ^ Bin.exe) in
      Option.some_if (Bin.exists fn) fn
    in
    if List.mem program ~set:programs_for_which_we_prefer_opt_ext then
      match exe_path (program ^ ".opt") with
      | None -> exe_path program
      | Some _ as path -> path
    else
      exe_path program

  let which ~path program =
    List.find_map path ~f:(fun dir -> best_path dir program)
end

module T = struct
  type t =
    { name : Context_name.t
    ; kind : Kind.t
    ; profile : Profile.t
    ; merlin : bool
    ; fdo_target_exe : Path.t option
    ; dynamically_linked_foreign_archives : bool
    ; for_host : t option
    ; implicit : bool
    ; build_dir : Path.Build.t
    ; env_nodes : Env_nodes.t
    ; path : Path.t list
    ; toplevel_path : Path.t option
    ; ocaml_bin : Path.t
    ; ocaml : Action.Prog.t
    ; ocamlc : Path.t
    ; ocamlopt : Action.Prog.t
    ; ocamldep : Action.Prog.t
    ; ocamlmklib : Action.Prog.t
    ; ocamlobjinfo : Action.Prog.t
    ; env : Env.t
    ; findlib : Findlib.t
    ; findlib_toolchain : Context_name.t option
    ; default_ocamlpath : Path.t list
    ; arch_sixtyfour : bool
    ; ocaml_config : Ocaml_config.t
    ; ocaml_config_vars : Ocaml_config.Vars.t
    ; version : Ocaml_version.t
    ; stdlib_dir : Path.t
    ; supports_shared_libraries : Dynlink_supported.By_the_os.t
    ; which : string -> Path.t option
    ; lib_config : Lib_config.t
    ; build_context : Build_context.t
    }

  let equal x y = Context_name.equal x.name y.name

  let hash t = Context_name.hash t.name

  let build_context t = t.build_context

  let to_dyn t : Dyn.t =
    let open Dyn.Encoder in
    let path = Path.to_dyn in
    record
      [ ("name", Context_name.to_dyn t.name)
      ; ("kind", Kind.to_dyn t.kind)
      ; ("profile", Profile.to_dyn t.profile)
      ; ("merlin", Bool t.merlin)
      ; ( "for_host"
        , option Context_name.to_dyn
            (Option.map t.for_host ~f:(fun t -> t.name)) )
      ; ("fdo_target_exe", option path t.fdo_target_exe)
      ; ("build_dir", Path.Build.to_dyn t.build_dir)
      ; ("toplevel_path", option path t.toplevel_path)
      ; ("ocaml_bin", path t.ocaml_bin)
      ; ("ocaml", Action.Prog.to_dyn t.ocaml)
      ; ("ocamlc", path t.ocamlc)
      ; ("ocamlopt", Action.Prog.to_dyn t.ocamlopt)
      ; ("ocamldep", Action.Prog.to_dyn t.ocamldep)
      ; ("ocamlmklib", Action.Prog.to_dyn t.ocamlmklib)
      ; ("env", Env.to_dyn (Env.diff t.env Env.initial))
      ; ("findlib_path", list path (Findlib.paths t.findlib))
      ; ("arch_sixtyfour", Bool t.arch_sixtyfour)
      ; ( "natdynlink_supported"
        , Bool
            (Dynlink_supported.By_the_os.get t.lib_config.natdynlink_supported)
        )
      ; ( "supports_shared_libraries"
        , Bool (Dynlink_supported.By_the_os.get t.supports_shared_libraries) )
      ; ("ocaml_config", Ocaml_config.to_dyn t.ocaml_config)
      ]
end

include T

let to_dyn_concise t : Dyn.t = Context_name.to_dyn t.name

let compare a b = Poly.compare a.name b.name

(** Wrap calls to the opam binary *)
module Opam : sig
  (** Read a configuration variable via [opam config var] *)
  val config_var : env:Env.t -> string -> string option Memo.Build.t

  (** Environment for this opam switch *)
  val env :
       env:Env.t
    -> root:string option
    -> switch:string
    -> string Env.Map.t Memo.Build.t
end = struct
  let opam =
    Memo.lazy_ (fun () -> Bin.which ~path:(Env.path Env.initial) "opam")

  let opam_exn () =
    match Memo.Lazy.force opam with
    | None -> Utils.program_not_found "opam" ~loc:None
    | Some fn -> fn

  let version =
    Memo.Lazy.Async.create (fun () ->
        let opam = opam_exn () in
        let+ version =
          Memo.Build.of_fiber
            (Process.run_capture_line Strict opam
               [ "--version"; "--color=never" ])
        in
        match Scanf.sscanf version "%d.%d.%d" (fun a b c -> (a, b, c)) with
        | Ok s -> s
        | Error () ->
          User_error.raise
            [ Pp.textf "`%s config --version' returned invalid output:"
                (Path.to_string_maybe_quoted opam)
            ; Pp.verbatim version
            ])

  let config_var =
    let impl (env, var) =
      match Memo.Lazy.force opam with
      | None -> Memo.Build.return None
      | Some fn -> (
        Memo.Build.of_fiber
          (Process.run_capture (Accept Predicate_lang.any) fn ~env
             [ "config"; "var"; var ])
        >>| function
        | Ok s -> Some (String.trim s)
        | Error _ -> None)
    in
    let module Input = struct
      type t = Env.t * string

      let equal (env_a, var_a) (env_b, var_b) =
        Env.equal env_a env_b && String.equal var_a var_b

      let hash (env, var) = Hashtbl.hash (Env.hash env, var)

      let to_dyn (env, var) = Dyn.Tuple [ Env.to_dyn env; String var ]
    end in
    let memo =
      Memo.create_hidden "opam-config-var" ~input:(module Input) Async impl
    in
    fun ~env var -> Memo.exec memo (env, var)

  let env =
    let impl (env, root, switch) =
      let opam = opam_exn () in
      let* version = Memo.Lazy.Async.force version in
      let args =
        List.concat
          [ [ "config"; "env" ]
          ; (match root with
            | None -> []
            | Some root -> [ "--root"; root ])
          ; [ "--switch"; switch; "--sexp" ]
          ; (if version < (2, 0, 0) then
              []
            else
              [ "--set-switch" ])
          ]
      in
      let+ s =
        Memo.Build.of_fiber (Process.run_capture ~env Strict opam args)
      in
      Dune_lang.Parser.parse_string ~fname:"<opam output>" ~mode:Single s
      |> Dune_lang.Decoder.(
           parse (enter (repeat (pair string string))) Univ_map.empty)
      |> Env.Map.of_list_multi
      |> Env.Map.mapi ~f:(fun var values ->
             match List.rev values with
             | [] -> assert false
             | [ x ] -> x
             | x :: _ ->
               Format.eprintf
                 "@{<warning>Warning@}: variable %S present multiple times in \
                  the output of:\n\
                  @{<details>%s@}@."
                 var
                 (String.concat ~sep:" "
                    (List.map ~f:String.quote_for_shell
                       (Path.to_string opam :: args)));
               x)
    in
    let module Input = struct
      type t = Env.t * string option * string

      let equal (env_a, root_a, switch_a) (env_b, root_b, switch_b) =
        Env.equal env_a env_b
        && Option.equal String.equal root_a root_b
        && String.equal switch_a switch_b

      let hash (env, root, switch) = Hashtbl.hash (Env.hash env, root, switch)

      let to_dyn (env, root, switch) =
        Dyn.Tuple
          [ Env.to_dyn env; Dyn.Encoder.(option string root); String switch ]
    end in
    let memo = Memo.create_hidden "opam-env" ~input:(module Input) Async impl in
    fun ~env ~root ~switch -> Memo.exec memo (env, root, switch)
end

let ocamlpath_sep =
  if Sys.cygwin then
    (* because that's what ocamlfind expects *)
    ';'
  else
    Bin.path_sep

module Build_environment_kind = struct
  (* Heuristics to detect the current environment *)

  type t =
    | Cross_compilation_using_findlib_toolchain of Context_name.t
    | Hardcoded_path of string list
    | Opam2_environment of string (* opam switch prefix *)
    | Opam1_environment
    | Unknown

  let query ~(kind : Kind.t) ~findlib_toolchain ~env =
    let opam_prefix = Env.get env "OPAM_SWITCH_PREFIX" in
    match findlib_toolchain with
    | Some s -> Cross_compilation_using_findlib_toolchain s
    | None -> (
      match kind with
      | Opam _ -> (
        match opam_prefix with
        | Some s -> Opam2_environment s
        | None -> Opam1_environment)
      | Default -> (
        match Setup.library_path with
        | Some l -> Hardcoded_path l
        | None -> (
          match opam_prefix with
          | Some s -> Opam2_environment s
          | None -> Unknown)))
end

let ocamlfind_printconf_path ~env ~ocamlfind ~toolchain =
  let args =
    let args = [ "printconf"; "path" ] in
    match toolchain with
    | None -> args
    | Some s -> "-toolchain" :: Context_name.to_string s :: args
  in
  let+ l =
    Memo.Build.of_fiber (Process.run_capture_lines ~env Strict ocamlfind args)
  in
  List.map l ~f:Path.of_filename_relative_to_initial_cwd

let check_fdo_support has_native ocfg ~name =
  let version = Ocaml_version.of_ocaml_config ocfg in
  let version_string = Ocaml_config.version_string ocfg in
  let err () =
    User_error.raise
      [ Pp.textf
          "fdo requires ocamlopt version >= 4.10, current version is %s \
           (context: %s)"
          (Context_name.to_string name)
          version_string
      ]
  in
  if not has_native then err ();
  if Ocaml_config.is_dev_version ocfg then
    ( (* Allows fdo to be invoked with any dev version of the compiler. This is
         experimental and will be removed when ocamlfdo is fully integrated into
         the toolchain. When using a dev version of ocamlopt that does not
         support the required options, fdo builds will fail because the compiler
         won't recongnize the options. Normals builds won't be affected. *) )
  else if not (Ocaml_version.supports_split_at_emit version) then
    if not (Ocaml_version.supports_function_sections version) then
      err ()
    else
      User_warning.emit
        [ Pp.textf
            "fdo requires ocamlopt version >= 4.10, current version %s has \
             partial support. Some optimizations are disabled! (context: %s)"
            (Context_name.to_string name)
            version_string
        ]

(* We store this so that library such as dune-configurator can read things
   runtime. Ideally, this should be created on-demand if we run a program linked
   against configurator, however we currently don't support this kind of
   "runtime dependencies" so we just do it eagerly. *)
let write_dot_dune_dir ~build_dir ~ocamlc ~ocaml_config_vars =
  let dir = Path.build (Path.Build.relative build_dir ".dune") in
  Path.rm_rf dir;
  Path.mkdir_p dir;
  Io.write_file (Path.relative dir Config.dune_keep_fname) "";
  let ocamlc = Path.to_absolute_filename ocamlc in
  let ocaml_config_vars = Ocaml_config.Vars.to_list ocaml_config_vars in
  let () =
    let open Dune_lang.Encoder in
    Io.write_lines
      (Path.relative dir "configurator")
      (List.map ~f:Dune_lang.to_string
         (record_fields
            [ field "ocamlc" string ocamlc
            ; field_l "ocaml_config_vars" (pair string string) ocaml_config_vars
            ]))
  in
  let () =
    let csexp =
      let open Sexp in
      let ocaml_config_vars =
        Sexp.List
          (List.map ocaml_config_vars ~f:(fun (k, v) -> List [ Atom k; Atom v ]))
      in
      List
        [ List [ Atom "ocamlc"; Atom ocamlc ]
        ; List [ Atom "ocaml_config_vars"; ocaml_config_vars ]
        ]
    in
    let path = Path.relative dir "configurator.v2" in
    Io.write_file path (Csexp.to_string csexp)
  in
  ()

let init_configurator { build_dir; ocamlc; ocaml_config_vars; _ } =
  write_dot_dune_dir ~build_dir ~ocamlc ~ocaml_config_vars

let create ~(kind : Kind.t) ~path ~env ~env_nodes ~name ~merlin ~targets
    ~host_context ~host_toolchain ~profile ~fdo_target_exe
    ~dynamically_linked_foreign_archives ~instrument_with =
  let prog_not_found_in_path prog =
    Utils.program_not_found prog ~context:name ~loc:None
  in
  let which_memo =
    Memo.create
      (sprintf "which-memo-for-%s" (Context_name.to_string name))
      ~input:(module Program.Name)
      ~output:(Allow_cutoff (module Program.Which_path))
      ~visibility:Hidden Sync (Program.which ~path)
  in
  let which = Memo.exec which_memo in
  let which_exn x =
    match which x with
    | None -> prog_not_found_in_path x
    | Some x -> x
  in
  let findlib_config_path =
    Memo.lazy_async ~cutoff:Path.equal (fun () ->
        let fn = which_exn "ocamlfind" in
        (* When OCAMLFIND_CONF is set, "ocamlfind printconf" does print the
           contents of the variable, but "ocamlfind printconf conf" still prints
           the configuration file set at the configuration time of ocamlfind,
           sigh... *)
        (match Env.get env "OCAMLFIND_CONF" with
        | Some s -> Memo.Build.return s
        | None ->
          Memo.Build.of_fiber
            (Process.run_capture_line ~env Strict fn [ "printconf"; "conf" ]))
        >>| Path.of_filename_relative_to_initial_cwd)
  in
  let create_one ~(name : Context_name.t) ~implicit ~findlib_toolchain ~host
      ~merlin =
    let* findlib_config =
      match findlib_toolchain with
      | None -> Memo.Build.return None
      | Some toolchain ->
        let+ path = Memo.Lazy.Async.force findlib_config_path in
        let toolchain = Context_name.to_string toolchain in
        let context = Context_name.to_string name in
        Some (Findlib.Config.load path ~toolchain ~context)
    in
    let get_tool_using_findlib_config prog =
      let open Option.O in
      let* conf = findlib_config in
      let* s = Findlib.Config.get conf prog in
      match Filename.analyze_program_name s with
      | In_path -> which s
      | Relative_to_current_dir ->
        User_error.raise
          [ Pp.textf
              "The effective Findlib configuration specifies the relative path \
               %S for the program %S. This is currently not supported."
              s prog
          ]
      | Absolute -> Some (Path.of_filename_relative_to_initial_cwd s)
    in
    let ocamlc =
      match get_tool_using_findlib_config "ocamlc" with
      | Some x -> x
      | None -> (
        match which "ocamlc" with
        | Some x -> x
        | None -> prog_not_found_in_path "ocamlc")
    in
    let dir = Path.parent_exn ocamlc in
    let get_ocaml_tool prog =
      match get_tool_using_findlib_config prog with
      | Some x -> Ok x
      | None -> (
        match Program.best_path dir prog with
        | Some p -> Ok p
        | None ->
          let hint =
            sprintf "ocamlc found in %s, but %s/%s doesn't exist (context: %s)"
              (Path.to_string dir) (Path.to_string dir) prog
              (Context_name.to_string name)
          in
          Error
            (Action.Prog.Not_found.create ~context:name ~program:prog ~loc:None
               ~hint ()))
    in
    let build_dir = Context_name.build_dir name in
    let ocamlpath =
      match
        let var = "OCAMLPATH" in
        match (kind, findlib_toolchain) with
        | Default, None -> Env.get env var
        | _ -> (
          (* If we are not in the default context, we can only use the OCAMLPATH
             variable if it is specific to this build context *)
          (* CR-someday diml: maybe we should actually clear OCAMLPATH in other
             build contexts *)
          match (Env.get env var, Env.get Env.initial var) with
          | None, None -> None
          | Some s, None -> Some s
          | None, Some _ -> None
          | Some x, Some y -> Option.some_if (x <> y) x)
      with
      | None -> []
      | Some s -> Bin.parse_path s ~sep:ocamlpath_sep
    in
    let default_findlib_paths () =
      match Build_environment_kind.query ~kind ~findlib_toolchain ~env with
      | Cross_compilation_using_findlib_toolchain toolchain ->
        let ocamlfind = which_exn "ocamlfind" in
        let env = Env.remove env ~var:"OCAMLPATH" in
        ocamlfind_printconf_path ~env ~ocamlfind ~toolchain:(Some toolchain)
      | Hardcoded_path l ->
        Memo.Build.return
          (List.map l ~f:Path.of_filename_relative_to_initial_cwd)
      | Opam2_environment opam_prefix ->
        let p = Path.of_filename_relative_to_initial_cwd opam_prefix in
        let p = Path.relative p "lib" in
        Memo.Build.return [ p ]
      | Opam1_environment -> (
        Opam.config_var ~env "lib" >>| function
        | Some s -> [ Path.of_filename_relative_to_initial_cwd s ]
        | None -> Utils.program_not_found "opam" ~loc:None)
      | Unknown -> (
        match which "ocamlfind" with
        | Some ocamlfind ->
          let env = Env.remove env ~var:"OCAMLPATH" in
          ocamlfind_printconf_path ~env ~ocamlfind ~toolchain:None
        | None ->
          Memo.Build.return [ Path.relative (Path.parent_exn dir) "lib" ])
    in
    let ocaml_config_ok_exn = function
      | Ok x -> x
      | Error (Ocaml_config.Origin.Ocamlc_config, msg) ->
        User_error.raise
          [ Pp.textf "Failed to parse the output of '%s -config':"
              (Path.to_string ocamlc)
          ; Pp.text msg
          ]
      | Error (Makefile_config file, msg) ->
        User_error.raise ~loc:(Loc.in_file file) [ Pp.text msg ]
    in
    let* default_ocamlpath, (ocaml_config_vars, ocfg) =
      Memo.Build.fork_and_join default_findlib_paths (fun () ->
          let+ lines =
            Memo.Build.of_fiber
              (Process.run_capture_lines ~env Strict ocamlc [ "-config" ])
          in
          ocaml_config_ok_exn
            (match Ocaml_config.Vars.of_lines lines with
            | Ok vars ->
              let open Result.O in
              let+ ocfg = Ocaml_config.make vars in
              (vars, ocfg)
            | Error msg -> Error (Ocamlc_config, msg)))
    in
    let findlib_paths = ocamlpath @ default_ocamlpath in
    let version = Ocaml_version.of_ocaml_config ocfg in
    let env =
      (* See comment in ansi_color.ml for setup_env_for_colors. For versions
         where OCAML_COLOR is not supported, but 'color' is in OCAMLPARAM, use
         the latter. If 'color' is not supported, we just don't force colors
         with 4.02. *)
      if
        !Clflags.capture_outputs
        && Lazy.force Ansi_color.stderr_supports_color
        && Ocaml_version.supports_color_in_ocamlparam version
        && not (Ocaml_version.supports_ocaml_color version)
      then
        let value =
          match Env.get env "OCAMLPARAM" with
          | None -> "color=always,_"
          | Some s -> "color=always," ^ s
        in
        Env.add env ~var:"OCAMLPARAM" ~value
      else
        env
    in
    let env =
      let cwd = Sys.getcwd () in
      let extend_var var ?(path_sep = Bin.path_sep) v =
        let v = Filename.concat cwd (Path.Build.to_string v) in
        match Env.get env var with
        | None -> (var, v)
        | Some prev -> (var, sprintf "%s%c%s" v path_sep prev)
      in
      let vars =
        let local_lib_root = Config.local_install_lib_root ~context:name in
        [ extend_var "CAML_LD_LIBRARY_PATH"
            (Path.Build.relative
               (Config.local_install_dir ~context:name)
               "lib/stublibs")
        ; extend_var "OCAMLPATH" ~path_sep:ocamlpath_sep local_lib_root
        ; ("DUNE_OCAML_STDLIB", Ocaml_config.standard_library ocfg)
        ; ( "DUNE_OCAML_HARDCODED"
          , String.concat
              ~sep:(Char.escaped ocamlpath_sep)
              (List.map ~f:Path.to_string default_ocamlpath) )
        ; extend_var "OCAMLTOP_INCLUDE_PATH"
            (Path.Build.relative local_lib_root "toplevel")
        ; extend_var "OCAMLFIND_IGNORE_DUPS_IN" ~path_sep:ocamlpath_sep
            local_lib_root
        ; extend_var "MANPATH" (Config.local_install_man_dir ~context:name)
        ; ("INSIDE_DUNE", Path.to_absolute_filename (Path.build build_dir))
        ; ( "DUNE_SOURCEROOT"
          , Path.to_absolute_filename (Path.source Path.Source.root) )
        ]
      in
      Env.extend env ~vars:(Env.Map.of_list_exn vars)
      |> Env.update ~var:"PATH" ~f:(fun _ ->
             match host with
             | None ->
               let _key, path =
                 Config.local_install_bin_dir ~context:name |> extend_var "PATH"
               in
               Some path
             | Some host -> Env.get host.env "PATH")
      |> Env.extend_env
           (Option.value ~default:Env.empty
              (Option.map findlib_config ~f:Findlib.Config.env))
      |> Env.extend_env (Env_nodes.extra_env ~profile env_nodes)
    in
    let stdlib_dir = Path.of_string (Ocaml_config.standard_library ocfg) in
    let natdynlink_supported = Ocaml_config.natdynlink_supported ocfg in
    let arch_sixtyfour = Ocaml_config.word_size ocfg = 64 in
    let ocamlopt = get_ocaml_tool "ocamlopt" in
    let lib_config =
      { Lib_config.has_native = Result.is_ok ocamlopt
      ; ext_obj = Ocaml_config.ext_obj ocfg
      ; ext_lib = Ocaml_config.ext_lib ocfg
      ; os_type = Ocaml_config.os_type ocfg
      ; architecture = Ocaml_config.architecture ocfg
      ; system = Ocaml_config.system ocfg
      ; model = Ocaml_config.model ocfg
      ; ext_dll = Ocaml_config.ext_dll ocfg
      ; natdynlink_supported =
          Dynlink_supported.By_the_os.of_bool natdynlink_supported
      ; stdlib_dir
      ; ccomp_type = Ocaml_config.ccomp_type ocfg
      ; profile
      ; ocaml_version_string = Ocaml_config.version_string ocfg
      ; ocaml_version = Ocaml_version.of_ocaml_config ocfg
      ; instrument_with
      ; context_name = name
      }
    in
    if Option.is_some fdo_target_exe then
      check_fdo_support lib_config.has_native ocfg ~name;
    let ocaml =
      let program = "ocaml" in
      match which program with
      | Some s -> Ok s
      | None ->
        Error (Action.Prog.Not_found.create ~context:name ~program ~loc:None ())
    in
    let ocaml_bin = dir in
    let supports_shared_libraries =
      Ocaml_config.supports_shared_libraries ocfg
    in
    let dynamically_linked_foreign_archives =
      supports_shared_libraries && dynamically_linked_foreign_archives
    in
    let t =
      let build_context =
        Build_context.create ~name ~build_dir ~env ~stdlib_dir
          ~default_ocamlpath
          ~host:(Option.map host ~f:(fun c -> c.build_context))
      in
      { name
      ; implicit
      ; kind
      ; profile
      ; merlin
      ; fdo_target_exe
      ; dynamically_linked_foreign_archives
      ; env_nodes
      ; for_host = host
      ; build_dir
      ; path
      ; toplevel_path =
          Option.map
            (Env.get env "OCAML_TOPLEVEL_PATH")
            ~f:Path.of_filename_relative_to_initial_cwd
      ; ocaml_bin
      ; ocaml
      ; ocamlc
      ; ocamlopt
      ; ocamldep = get_ocaml_tool "ocamldep"
      ; ocamlmklib = get_ocaml_tool "ocamlmklib"
      ; ocamlobjinfo = get_ocaml_tool "ocamlobjinfo"
      ; env
      ; findlib = Findlib.create ~paths:findlib_paths ~lib_config
      ; findlib_toolchain
      ; default_ocamlpath
      ; arch_sixtyfour
      ; stdlib_dir
      ; ocaml_config = ocfg
      ; ocaml_config_vars
      ; version
      ; supports_shared_libraries =
          Dynlink_supported.By_the_os.of_bool supports_shared_libraries
      ; which
      ; lib_config
      ; build_context
      }
    in
    if Ocaml_version.supports_response_file version then (
      let set prog =
        Response_file.set ~prog (Zero_terminated_strings "-args0")
      in
      Result.iter t.ocaml ~f:set;
      set t.ocamlc;
      Result.iter t.ocamlopt ~f:set;
      Result.iter t.ocamldep ~f:set;
      if Ocaml_version.ocamlmklib_supports_response_file version then
        Result.iter ~f:set t.ocamlmklib
    );
    Memo.Build.return t
  in
  let implicit = not (List.mem ~set:targets Workspace.Context.Target.Native) in
  let* native =
    create_one ~host:host_context ~findlib_toolchain:host_toolchain ~implicit
      ~name ~merlin
  in
  let+ others =
    Memo.Build.parallel_map targets ~f:(function
      | Native -> Memo.Build.return None
      | Named findlib_toolchain ->
        let name = Context_name.target name ~toolchain:findlib_toolchain in
        create_one ~implicit:false ~name ~host:(Some native) ~merlin:false
          ~findlib_toolchain:(Some findlib_toolchain)
        >>| Option.some)
  in
  native :: List.filter_opt others

let extend_paths t ~env =
  let t =
    let f (var, t) =
      let parse ~loc:_ s = s in
      let standard = Env.path env |> List.map ~f:Path.to_string in
      (var, Ordered_set_lang.eval t ~parse ~standard ~eq:String.equal)
    in
    List.map ~f t
  in
  let vars =
    let to_absolute_filename s =
      Path.of_string s |> Path.to_absolute_filename
    in
    let sep = String.make 1 Bin.path_sep in
    let env = Env.Map.of_list_exn t in
    let f l = String.concat ~sep (List.map ~f:to_absolute_filename l) in
    Env.Map.map ~f env
  in
  Env.extend ~vars env

let default ~merlin ~env_nodes ~env ~targets ~fdo_target_exe
    ~dynamically_linked_foreign_archives ~instrument_with =
  let path = Env.path env in
  create ~kind:Default ~path ~env ~env_nodes ~merlin ~targets ~fdo_target_exe
    ~dynamically_linked_foreign_archives ~instrument_with

let create_for_opam ~root ~env ~env_nodes ~targets ~profile ~switch ~name
    ~merlin ~host_context ~host_toolchain ~fdo_target_exe
    ~dynamically_linked_foreign_archives ~instrument_with =
  let* vars = Opam.env ~env ~root ~switch in
  let path =
    match Env.Map.find vars "PATH" with
    | None -> Env.path env
    | Some s -> Bin.parse_path s
  in
  let env = Env.extend env ~vars in
  create
    ~kind:(Opam { root; switch })
    ~profile ~targets ~path ~env ~env_nodes ~name ~merlin ~host_context
    ~host_toolchain ~fdo_target_exe ~dynamically_linked_foreign_archives
    ~instrument_with

module T_list = struct
  type nonrec t = t list

  let to_dyn = Dyn.Encoder.list to_dyn
end

module rec Instantiate : sig
  val instantiate : Context_name.t -> t list Memo.Build.t
end = struct
  let instantiate_impl name : t list Memo.Build.t =
    let env = Memo.Run.Fdecl.get Global.env in
    let workspace = Workspace.workspace () in
    let context =
      List.find_exn workspace.contexts ~f:(fun ctx ->
          Context_name.equal (Workspace.Context.name ctx) name)
    in
    let* host_context =
      match Workspace.Context.host_context context with
      | None -> Memo.Build.return None
      | Some context_name -> (
        let+ contexts = Instantiate.instantiate context_name in
        match contexts with
        | [ x ] -> Some x
        | [] -> assert false (* checked by workspace *)
        | _ :: _ -> assert false)
      (* target cannot be host *)
    in
    let env_nodes =
      let context = Workspace.Context.env context in
      { Env_nodes.context; workspace = workspace.env }
    in
    match context with
    | Default
        { targets
        ; name
        ; host_context = _
        ; profile
        ; env = _
        ; toolchain
        ; paths
        ; loc = _
        ; fdo_target_exe
        ; dynamically_linked_foreign_archives
        ; instrument_with
        } ->
      let merlin =
        workspace.merlin_context = Some (Workspace.Context.name context)
      in
      let host_toolchain : Context_name.t option =
        match toolchain with
        | Some _ -> toolchain
        | None ->
          let open Option.O in
          let+ name = Env.get env "OCAMLFIND_TOOLCHAIN" in
          Context_name.parse_string_exn (Loc.none, name)
      in
      let env = extend_paths ~env paths in
      default ~env ~env_nodes ~profile ~targets ~name ~merlin ~host_context
        ~host_toolchain ~fdo_target_exe ~dynamically_linked_foreign_archives
        ~instrument_with
    | Opam
        { base =
            { targets
            ; name
            ; host_context = _
            ; profile
            ; env = _
            ; toolchain
            ; paths
            ; loc = _
            ; fdo_target_exe
            ; dynamically_linked_foreign_archives
            ; instrument_with
            }
        ; switch
        ; root
        ; merlin
        } ->
      let env = extend_paths ~env paths in
      create_for_opam ~root ~env_nodes ~env ~profile ~switch ~name ~merlin
        ~targets ~host_context ~host_toolchain:toolchain ~fdo_target_exe
        ~dynamically_linked_foreign_archives ~instrument_with

  let memo =
    Memo.create "instantiate-context" ~doc:"instantiate contexts"
      ~input:(module Context_name)
      ~output:(Simple (module T_list))
      ~visibility:Memo.Visibility.Hidden Async instantiate_impl

  let instantiate name = Memo.exec memo name
end

module Create = struct
  let call () =
    let workspace = Workspace.workspace () in
    let+ contexts =
      Memo.Build.parallel_map workspace.contexts ~f:(fun c ->
          Instantiate.instantiate (Workspace.Context.name c))
    in
    List.concat contexts

  let memo =
    Memo.create "create-contexts" ~doc:"create contexts"
      ~input:(module Unit)
      ~output:(Simple (module T_list))
      ~visibility:Memo.Visibility.Hidden Async call
end

module DB = struct
  let all = Memo.exec Create.memo

  let get =
    let memo =
      Memo.create "context-db-get" ~doc:"get context from db"
        ~input:(module Context_name)
        ~output:(Simple (module T))
        ~visibility:Hidden Sync
        (fun name ->
          (* CR-someday amokhov: Here we assume that [get] is called after the
             asynchronously running function [Create.memo] has completed. It
             would be better to statically guarantee the completion. Note that
             moving this code into a fiber was ruled out because it would
             require any functions that need the context to go inside the fiber
             too. @rgrinberg and @diml decided that it would be too large and
             possibly undesirable refactoring. Any other options? *)
          let contexts = Memo.peek_exn Create.memo () in
          List.find_exn contexts ~f:(fun c -> Context_name.equal name c.name))
    in
    fun dir ->
      match Path.Build.extract_build_context dir with
      | None ->
        Code_error.raise "Context.DB.get: invalid dir"
          [ ("dir", Path.Build.to_dyn dir) ]
      | Some (name, _) ->
        let name = Context_name.of_string name in
        Memo.exec memo name
end

let install_ocaml_libdir t =
  match (t.kind, t.findlib_toolchain, Setup.library_destdir) with
  | Default, None, Some d ->
    Memo.Build.return (Some (Path.of_filename_relative_to_initial_cwd d))
  | _ -> (
    (* If ocamlfind is present, it has precedence over everything else. *)
    match t.which "ocamlfind" with
    | Some fn ->
      let+ s =
        Memo.Build.of_fiber
          (Process.run_capture_line ~env:t.env Strict fn
             [ "printconf"; "destdir" ])
      in
      Some (Path.of_filename_relative_to_initial_cwd s)
    | None -> Memo.Build.return None)

let compiler t (mode : Mode.t) =
  match mode with
  | Byte -> Ok t.ocamlc
  | Native -> t.ocamlopt

let best_mode t : Mode.t =
  match t.ocamlopt with
  | Ok _ -> Native
  | Error _ -> Byte

let cc_g (ctx : t) =
  match ctx.lib_config.ccomp_type with
  | Msvc -> []
  | Other _ -> [ "-g" ]

let name t = t.name

let has_native t = Result.is_ok t.ocamlopt

let lib_config t = t.lib_config

let map_exe (context : t) =
  match context.for_host with
  | None -> fun exe -> exe
  | Some (host : t) -> (
    fun exe ->
      match Path.extract_build_context_dir exe with
      | Some (dir, exe) when Path.equal dir (Path.build context.build_dir) ->
        Path.append_source (Path.build host.build_dir) exe
      | _ -> exe)

let install_prefix t =
  Memo.Build.map (Opam.config_var ~env:t.env "prefix") ~f:(function
    | Some x -> Path.of_filename_relative_to_initial_cwd x
    | None -> Path.parent_exn t.ocaml_bin)
