open! Stdune
open Import
open Fiber.O

module Kind = struct
  module Opam = struct
    type t =
      { root   : string option
      ; switch : string
      }
  end
  type t = Default | Opam of Opam.t

  let to_dyn : t -> Dyn.t =
    function
    | Default -> Dyn.Encoder.string "default"
    | Opam o  ->
      Dyn.Encoder.(record
                     [ "root"  , option string o.root
                     ; "switch", string o.switch
                     ])
end

module Env_nodes = struct
  type t =
    { context: Dune_env.Stanza.t
    ; workspace: Dune_env.Stanza.t
    }

  let extra_env ~profile env_nodes =
    Env.extend_env
      (Dune_env.Stanza.find env_nodes.context ~profile).env_vars
      (Dune_env.Stanza.find env_nodes.workspace ~profile).env_vars
end

type t =
  { name                    : string
  ; kind                    : Kind.t
  ; profile                 : Profile.t
  ; merlin                  : bool
  ; for_host                : t option
  ; implicit                : bool
  ; build_dir               : Path.Build.t
  ; env_nodes               : Env_nodes.t
  ; path                    : Path.t list
  ; toplevel_path           : Path.t option
  ; ocaml_bin               : Path.t
  ; ocaml                   : Path.t
  ; ocamlc                  : Path.t
  ; ocamlopt                : Path.t option
  ; ocamldep                : Path.t
  ; ocamlmklib              : Path.t
  ; ocamlobjinfo            : Path.t option
  ; env                     : Env.t
  ; findlib                 : Findlib.t
  ; findlib_toolchain       : string option
  ; arch_sixtyfour          : bool
  ; opam_var_cache          : (string, string) Table.t
  ; ocaml_config            : Ocaml_config.t
  ; version_string          : string
  ; version                 : Ocaml_version.t
  ; stdlib_dir              : Path.t
  ; ccomp_type              : string
  ; c_compiler              : string
  ; ocamlc_cflags           : string list
  ; ocamlopt_cflags         : string list
  ; bytecomp_c_libraries    : string list
  ; native_c_libraries      : string list
  ; cc_profile              : string list
  ; architecture            : string
  ; system                  : string
  ; ext_asm                 : string
  ; ext_exe                 : string
  ; os_type                 : string
  ; model                   : string
  ; default_executable_name : string
  ; host                    : string
  ; target                  : string
  ; flambda                 : bool
  ; exec_magic_number       : string
  ; cmi_magic_number        : string
  ; cmo_magic_number        : string
  ; cma_magic_number        : string
  ; cmx_magic_number        : string
  ; cmxa_magic_number       : string
  ; ast_impl_magic_number   : string
  ; ast_intf_magic_number   : string
  ; cmxs_magic_number       : string
  ; cmt_magic_number        : string
  ; supports_shared_libraries : Dynlink_supported.By_the_os.t
  ; which_cache             : (string, Path.t option) Table.t
  ; lib_config : Lib_config.t
  }

let equal x y = String.equal x.name y.name
let hash t = String.hash t.name

let to_dyn t : Dyn.t =
  let open Dyn.Encoder in
  let path = Path.to_dyn in
  record
    [ "name", String t.name
    ; "kind", Kind.to_dyn t.kind
    ; "profile", Profile.to_dyn t.profile
    ; "merlin", Bool t.merlin
    ; "for_host",
      option string (Option.map t.for_host ~f:(fun t -> t.name))
    ; "build_dir", Path.Build.to_dyn t.build_dir
    ; "toplevel_path", option path t.toplevel_path
    ; "ocaml_bin", path t.ocaml_bin
    ; "ocaml", path t.ocaml
    ; "ocamlc", path t.ocamlc
    ; "ocamlopt", option path t.ocamlopt
    ; "ocamldep", path t.ocamldep
    ; "ocamlmklib", path t.ocamlmklib
    ; "env", Env.to_dyn (Env.diff t.env Env.initial)
    ; "findlib_path", list path (Findlib.paths t.findlib)
    ; "arch_sixtyfour", Bool t.arch_sixtyfour
    ; "natdynlink_supported",
      Bool (Dynlink_supported.By_the_os.get t.lib_config.natdynlink_supported)
    ; "supports_shared_libraries",
      Bool (Dynlink_supported.By_the_os.get t.supports_shared_libraries)
    ; "opam_vars", Table.to_dyn string t.opam_var_cache
    ; "ocaml_config", Ocaml_config.to_dyn t.ocaml_config
    ; "which", Table.to_dyn (option path) t.which_cache
    ]

let to_dyn_concise t : Dyn.t = String t.name

let compare a b = compare a.name b.name

let opam = lazy (Bin.which ~path:(Env.path Env.initial) "opam")

let opam_config_var ~env ~cache var =
  match Table.find cache var with
  | Some _ as x -> Fiber.return x
  | None ->
    match Lazy.force opam with
    | None -> Fiber.return None
    | Some fn ->
      Process.run_capture (Accept All) fn ~env
        ["config"; "var"; var]
      >>| function
      | Ok s ->
        let s = String.trim s in
        Table.set cache var s;
        Some s
      | Error _ -> None

let which ~cache ~path x =
  Table.find_or_add cache x ~f:(Bin.which ~path)

let ocamlpath_sep =
  if Sys.cygwin then
    (* because that's what ocamlfind expects *)
    ';'
  else
    Bin.path_sep

module Build_environment_kind = struct
  (* Heuristics to detect the current environment *)

  type t =
    | Cross_compilation_using_findlib_toolchain of string
    | Hardcoded_path of string list
    | Opam2_environment of string (* opam switch prefix *)
    | Opam1_environment
    | Unknown

  let query ~(kind : Kind.t) ~findlib_toolchain ~env =
    let opam_prefix = Env.get env "OPAM_SWITCH_PREFIX" in
    match findlib_toolchain with
    | Some s -> Cross_compilation_using_findlib_toolchain s
    | None ->
      match kind with
      | Opam _ -> begin
          match opam_prefix with
          | Some s -> Opam2_environment s
          | None -> Opam1_environment
        end
      | Default ->
        match Setup.library_path with
        | Some l -> Hardcoded_path l
        | None ->
          match opam_prefix with
          | Some s -> Opam2_environment s
          | None -> Unknown
end

let ocamlfind_printconf_path ~env ~ocamlfind ~toolchain =
  let args =
    let args = ["printconf"; "path"] in
    match toolchain with
    | None -> args
    | Some s -> "-toolchain" :: s :: args
  in
  let+ l = Process.run_capture_lines ~env Strict ocamlfind args in
  List.map l ~f:Path.of_filename_relative_to_initial_cwd

let create ~(kind : Kind.t) ~path ~env ~env_nodes ~name ~merlin ~targets
      ~host_context ~host_toolchain ~profile =
  let opam_var_cache = Table.create (module String) 128 in
  (match kind with
   | Opam { root = Some root; _ } ->
     Table.set opam_var_cache "root" root
   | _ -> ());
  let prog_not_found_in_path prog =
    Utils.program_not_found prog ~context:name ~loc:None
  in
  let which_cache = Table.create (module String) 128 in
  let which x = which ~cache:which_cache ~path x in
  let which_exn x =
    match which x with
    | None -> prog_not_found_in_path x
    | Some x -> x
  in
  let findlib_config_path = lazy (
    let fn = which_exn "ocamlfind" in
    (* When OCAMLFIND_CONF is set, "ocamlfind printconf" does print
       the contents of the variable, but "ocamlfind printconf conf"
       still prints the configuration file set at the configuration
       time of ocamlfind, sigh... *)
    (match Env.get env "OCAMLFIND_CONF" with
     | Some s -> Fiber.return s
     | None -> Process.run_capture_line ~env Strict fn ["printconf"; "conf"])
    >>| Path.of_filename_relative_to_initial_cwd)
  in

  let create_one ~name ~implicit ~findlib_toolchain ~host ~merlin =
    let* findlib_config =
      match findlib_toolchain with
      | None -> Fiber.return None
      | Some toolchain ->
        let+ path = Lazy.force findlib_config_path in
        Some (Findlib.Config.load path ~toolchain ~context:name)
    in
    let get_tool_using_findlib_config prog =
      let open Option.O in
      let* conf = findlib_config in
      let* s = Findlib.Config.get conf prog in
      match Filename.analyze_program_name s with
      | In_path | Relative_to_current_dir -> which s
      | Absolute -> Some (Path.of_filename_relative_to_initial_cwd s)
    in

    let ocamlc =
      match get_tool_using_findlib_config "ocamlc" with
      | Some x -> x
      | None ->
        match which "ocamlc" with
        | Some x -> x
        | None -> prog_not_found_in_path "ocamlc"
    in
    let dir = Path.parent_exn ocamlc in
    let ocaml_tool_not_found prog =
      User_error.raise
        [ Pp.textf "ocamlc found in %s, but %s/%s doesn't exist \
                    (context: %s)"
            (Path.to_string dir) (Path.to_string dir) prog name
        ]
    in
    let get_ocaml_tool prog =
      match get_tool_using_findlib_config prog with
      | None -> Bin.best_prog dir prog
      | Some _ as x -> x
    in
    let get_ocaml_tool_exn prog =
      match get_ocaml_tool prog with
      | None -> ocaml_tool_not_found prog
      | Some fn -> fn
    in

    let build_dir = Path.Build.relative Path.Build.root name in
    let ocamlpath =
      match
        let var = "OCAMLPATH" in
        match kind, findlib_toolchain with
        | Default, None -> Env.get env var
        | _ ->
          (* If we are not in the default context, we can only use the
             OCAMLPATH variable if it is specific to this build
             context *)
          (* CR-someday diml: maybe we should actually clear OCAMLPATH
             in other build contexts *)
          match Env.get env var, Env.get Env.initial var with
          | None  , None   -> None
          | Some s, None   -> Some s
          | None  , Some _ -> None
          | Some x, Some y -> Option.some_if (x <> y) x
      with
      | None -> []
      | Some s -> Bin.parse_path s ~sep:ocamlpath_sep
    in
    let findlib_paths () =
      match Build_environment_kind.query ~kind ~findlib_toolchain ~env with
      | Cross_compilation_using_findlib_toolchain toolchain ->
        let ocamlfind = which_exn "ocamlfind" in
        ocamlfind_printconf_path ~env ~ocamlfind ~toolchain:(Some toolchain)

      | Hardcoded_path l ->
        Fiber.return
          (ocamlpath @ List.map l ~f:Path.of_filename_relative_to_initial_cwd)

      | Opam2_environment opam_prefix ->
        let p = Path.of_filename_relative_to_initial_cwd opam_prefix in
        let p = Path.relative p "lib" in
        Fiber.return (ocamlpath @ [p])

      | Opam1_environment -> begin
          opam_config_var ~env ~cache:opam_var_cache "lib"
          >>| function
          | Some s -> ocamlpath @ [Path.of_filename_relative_to_initial_cwd s]
          | None -> Utils.program_not_found "opam" ~loc:None
        end

      | Unknown ->
        match which "ocamlfind" with
        | Some ocamlfind ->
          ocamlfind_printconf_path ~env ~ocamlfind ~toolchain:None

        | None ->
          Fiber.return
            (ocamlpath @ [ Path.relative (Path.parent_exn dir) "lib" ])
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
        User_error.raise ~loc:(Loc.in_file file)
          [ Pp.text msg ]
    in
    let* (findlib_paths, ocfg) =
      Fiber.fork_and_join
        findlib_paths
        (fun () ->
          let+ lines =
            Process.run_capture_lines ~env Strict ocamlc ["-config"] in
          ocaml_config_ok_exn
            (match Ocaml_config.Vars.of_lines lines with
              | Ok vars -> Ocaml_config.make vars
              | Error msg -> Error (Ocamlc_config, msg)))
    in
    let version = Ocaml_version.of_ocaml_config ocfg in
    let env =
      (* See comment in ansi_color.ml for setup_env_for_colors.
         For versions where OCAML_COLOR is not supported, but 'color' is in
         OCAMLPARAM, use the latter.
         If 'color' is not supported, we just don't force colors with 4.02. *)
      if !Clflags.capture_outputs
      && Lazy.force Ansi_color.stderr_supports_color
      && Ocaml_version.supports_color_in_ocamlparam version
      && not (Ocaml_version.supports_ocaml_color version) then
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
      let extend_var var ?(path_sep=Bin.path_sep) v =
        let v = Filename.concat cwd (Path.to_string v) in
        match Env.get env var with
        | None -> (var, v)
        | Some prev -> (var, sprintf "%s%c%s" v path_sep prev)
      in
      let vars =
        let local_lib_path =
          Path.Build.relative
             (Config.local_install_dir ~context:name)
             "lib"
          |> Path.build
        in
        [ extend_var "CAML_LD_LIBRARY_PATH"
            (Path.build (Path.Build.relative
                           (Config.local_install_dir ~context:name)
                           "lib/stublibs"))
        ; extend_var "OCAMLPATH" ~path_sep:ocamlpath_sep
            local_lib_path
        ; extend_var "OCAMLFIND_IGNORE_DUPS_IN" ~path_sep:ocamlpath_sep
            local_lib_path
        ; extend_var "MANPATH"
            (Path.build (Config.local_install_man_dir ~context:name))
        ; "DUNE_CONFIGURATOR", (Path.to_string ocamlc)
        ]
      in
      Env.extend env ~vars:(Env.Map.of_list_exn vars)
      |> Env.update ~var:"PATH" ~f:(fun _ ->
        match host with
        | None ->
          let _key, path =
            Path.build (Config.local_install_bin_dir ~context:name)
            |> extend_var "PATH" in
          Some path
        | Some host ->
          Env.get host.env "PATH"
      )
      |> Env.extend_env (
        Option.value ~default:Env.empty
          (Option.map findlib_config ~f:Findlib.Config.env)
      )
    |> Env.extend_env (Env_nodes.extra_env ~profile env_nodes)
    in
    let stdlib_dir = Path.of_string (Ocaml_config.standard_library ocfg) in
    let natdynlink_supported = Ocaml_config.natdynlink_supported ocfg in
    let version_string = Ocaml_config.version_string ocfg in
    let version        = Ocaml_version.of_ocaml_config ocfg in
    let arch_sixtyfour = Ocaml_config.word_size ocfg = 64 in
    let ocamlopt = get_ocaml_tool "ocamlopt" in
    let lib_config     =
      { Lib_config.
        has_native = Option.is_some ocamlopt
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
      }
    in
    let t =
      { name
      ; implicit
      ; kind
      ; profile
      ; merlin
      ; env_nodes
      ; for_host = host
      ; build_dir
      ; path
      ; toplevel_path =
          Option.map (Env.get env "OCAML_TOPLEVEL_PATH")
            ~f:Path.of_filename_relative_to_initial_cwd

      ; ocaml_bin  = dir
      ; ocaml      =
          (match which "ocaml" with
          | Some p -> p
          | None -> prog_not_found_in_path "ocaml")
      ; ocamlc
      ; ocamlopt
      ; ocamldep     = get_ocaml_tool_exn "ocamldep"
      ; ocamlmklib   = get_ocaml_tool_exn "ocamlmklib"
      ; ocamlobjinfo = which "ocamlobjinfo"

      ; env
      ; findlib = Findlib.create ~stdlib_dir ~paths:findlib_paths ~version
      ; findlib_toolchain
      ; arch_sixtyfour

      ; opam_var_cache

      ; stdlib_dir
      ; ocaml_config = ocfg
      ; version_string
      ; version
      ; ccomp_type              = Ocaml_config.ccomp_type              ocfg
      ; c_compiler              = Ocaml_config.c_compiler              ocfg
      ; ocamlc_cflags           = Ocaml_config.ocamlc_cflags           ocfg
      ; ocamlopt_cflags         = Ocaml_config.ocamlopt_cflags         ocfg
      ; bytecomp_c_libraries    = Ocaml_config.bytecomp_c_libraries    ocfg
      ; native_c_libraries      = Ocaml_config.native_c_libraries      ocfg
      ; cc_profile              = Ocaml_config.cc_profile              ocfg
      ; architecture            = Ocaml_config.architecture            ocfg
      ; system                  = Ocaml_config.system                  ocfg
      ; ext_asm                 = Ocaml_config.ext_asm                 ocfg
      ; ext_exe                 = Ocaml_config.ext_exe                 ocfg
      ; os_type                 = Ocaml_config.os_type                 ocfg
      ; model                   = Ocaml_config.model                   ocfg
      ; default_executable_name = Ocaml_config.default_executable_name ocfg
      ; host                    = Ocaml_config.host                    ocfg
      ; target                  = Ocaml_config.target                  ocfg
      ; flambda                 = Ocaml_config.flambda                 ocfg
      ; exec_magic_number       = Ocaml_config.exec_magic_number       ocfg
      ; cmi_magic_number        = Ocaml_config.cmi_magic_number        ocfg
      ; cmo_magic_number        = Ocaml_config.cmo_magic_number        ocfg
      ; cma_magic_number        = Ocaml_config.cma_magic_number        ocfg
      ; cmx_magic_number        = Ocaml_config.cmx_magic_number        ocfg
      ; cmxa_magic_number       = Ocaml_config.cmxa_magic_number       ocfg
      ; ast_impl_magic_number   = Ocaml_config.ast_impl_magic_number   ocfg
      ; ast_intf_magic_number   = Ocaml_config.ast_intf_magic_number   ocfg
      ; cmxs_magic_number       = Ocaml_config.cmxs_magic_number       ocfg
      ; cmt_magic_number        = Ocaml_config.cmt_magic_number        ocfg
      ; supports_shared_libraries =
          Dynlink_supported.By_the_os.of_bool
            (Ocaml_config.supports_shared_libraries ocfg)

      ; which_cache
      ; lib_config
      }
    in
    if Ocaml_version.supports_response_file version then begin
      let set prog =
        Response_file.set ~prog (Zero_terminated_strings "-args0")
      in
      set t.ocaml;
      set t.ocamlc;
      Option.iter t.ocamlopt ~f:set;
      set t.ocamldep;
      if Ocaml_version.ocamlmklib_supports_response_file version then begin
        set t.ocamlmklib;
      end;
    end;
    Fiber.return t
  in
  let implicit = not (List.mem ~set:targets Workspace.Context.Target.Native) in
  let* native =
    create_one ~host:host_context ~findlib_toolchain:host_toolchain
      ~implicit ~name ~merlin
  in
  let+ others =
    Fiber.parallel_map targets ~f:(function
      | Native -> Fiber.return None
      | Named findlib_toolchain ->
        let name = sprintf "%s.%s" name findlib_toolchain in
        create_one ~implicit:false ~name ~host:(Some native) ~merlin:false
          ~findlib_toolchain:(Some findlib_toolchain)
        >>| Option.some)
  in
  native :: List.filter_opt others

let extend_paths ~env ~context =
  let t = Workspace.Context.paths context in
  let expand (var,t) =
    let _, _ =
      Ordered_set_lang.Unexpanded.files t
        ~f:(fun incl ->
          User_error.raise ~loc:(String_with_vars.loc incl)
            [ Pp.text "The :include are not accepted in workspace file"]
        )
    in
    let t =
      Ordered_set_lang.Unexpanded.expand t
        ~dir:Path.root
        ~files_contents:Path.Map.empty
        ~f:(fun s ->
          String_with_vars.expand s
            ~mode:String_with_vars.Mode.Many
            ~dir:Path.root
            ~f:(fun var _ ->
              match String_with_vars.Var.name var with
              | "workspace_root" ->
                (* We use String otherwise it doesn't stay absolute *)
                Some [Value.String (Path.to_absolute_filename Path.root)]
              | _ -> None
            )
           )
    in
    let parse ~loc:_ s = s in
    let standard = Env.path env |> List.map ~f:Path.to_string in
    var, Ordered_set_lang.String.eval t ~parse ~standard
  in
  let t = List.map ~f:expand t in
  let vars =
    let to_absolute_filename s =
      Path.of_string s |> Path.to_absolute_filename in
    let sep = String.make 1 Bin.path_sep in
    let env = Env.Map.of_list_exn t in
    let f l = String.concat ~sep (List.map ~f:to_absolute_filename l) in
    Env.Map.map ~f env
  in
  Env.extend ~vars env

let opam_config_var t var =
  opam_config_var ~env:t.env ~cache:t.opam_var_cache var

let default ~merlin ~env_nodes ~env ~targets =
  let path = Env.path env in
  create ~kind:Default ~path ~env ~env_nodes ~merlin ~targets

let opam_version =
  let res = ref None in
  fun opam env ->
    match !res with
    | Some future -> Fiber.Future.wait future
    | None ->
      let* future =
        Fiber.fork (fun () ->
          let+ version =
            Process.run_capture_line Strict ~env opam ["--version"]
          in
          try
            Scanf.sscanf version "%d.%d.%d" (fun a b c -> a, b, c)
          with _ ->
            User_error.raise
              [ Pp.textf "`%s config --version' returned invalid \
                          output:"
                  (Path.to_string_maybe_quoted opam)
              ; Pp.verbatim version
              ])
      in
      res := Some future;
      Fiber.Future.wait future

let create_for_opam ~root ~env ~env_nodes ~targets ~profile
      ~switch ~name ~merlin ~host_context ~host_toolchain =
  let opam =
    match Lazy.force opam with
    | None -> Utils.program_not_found "opam" ~loc:None
    | Some fn -> fn
  in
  let* version = opam_version opam env in
  let args =
    List.concat
      [ [ "config"; "env" ]
      ; (match root with
         | None -> []
         | Some root -> [ "--root"; root ])
      ; [ "--switch"; switch; "--sexp" ]
      ; if version < (2, 0, 0) then [] else ["--set-switch"]
      ]
  in
  let* s = Process.run_capture ~env Strict opam args in
  let vars =
    Dune_lang.parse_string ~fname:"<opam output>" ~mode:Single s
    |> Dune_lang.Decoder.(parse (enter (repeat (pair string string)))
                            Univ_map.empty)
    |> Env.Map.of_list_multi
    |> Env.Map.mapi ~f:(fun var values ->
      match List.rev values with
      | [] -> assert false
      | [x] -> x
      | x :: _ ->
        Format.eprintf
          "@{<warning>Warning@}: variable %S present multiple times in the \
           output of:\n\
           @{<details>%s@}@."
          var
          (String.concat ~sep:" "
             (List.map ~f:String.quote_for_shell
                (Path.to_string opam :: args)));
        x)
  in
  let path =
    match Env.Map.find vars "PATH" with
    | None   -> Env.path env
    | Some s -> Bin.parse_path s
  in
  let env = Env.extend env ~vars in
  create ~kind:(Opam { root; switch }) ~profile ~targets ~path ~env ~env_nodes
    ~name ~merlin ~host_context ~host_toolchain

let instantiate_context env (workspace : Workspace.t)
      ~(context : Workspace.Context.t) ~host_context =
  let env_nodes =
    let context = Workspace.Context.env context in
    { Env_nodes.
      context
    ; workspace = workspace.env
    }
  in
  let env =
    Env.extend_env env
      (Env_nodes.extra_env
         ~profile:(Workspace.Context.profile context) env_nodes)
  in
  let env = extend_paths ~env ~context in
  match context with
  | Default { targets; name; host_context = _; profile; env = _
            ; toolchain ; paths = _; loc = _ } ->
    let merlin =
      workspace.merlin_context = Some (Workspace.Context.name context)
    in
    let host_toolchain =
      match toolchain with
      | Some _ -> toolchain
      | None -> Env.get env "OCAMLFIND_TOOLCHAIN"
    in
    default ~env ~env_nodes ~profile ~targets ~name ~merlin ~host_context
      ~host_toolchain
  | Opam { base = { targets; name; host_context = _; profile; env = _
                  ; toolchain; paths = _; loc = _ }
         ; switch; root; merlin } ->
    create_for_opam ~root ~env_nodes ~env ~profile ~switch ~name ~merlin
      ~targets ~host_context ~host_toolchain:toolchain

let create ~env (workspace : Workspace.t) =
  let rec contexts : t list Fiber.Once.t String.Map.t Lazy.t = lazy (
    List.map workspace.contexts ~f:(fun context ->
      let contexts = Fiber.Once.create (fun () ->
        let* host_context =
          match Workspace.Context.host_context context with
          | None -> Fiber.return None
          | Some context ->
            let+ contexts =
              String.Map.find_exn (Lazy.force contexts) context
              |> Fiber.Once.get
            in
            match contexts with
            | [x] -> Some x
            | [] -> assert false (* checked by workspace *)
            | _ :: _ -> assert false (* target cannot be host *)
        in
        instantiate_context env workspace ~context ~host_context
      ) in
      let name = Workspace.Context.name context in
      (name, contexts))
    |> String.Map.of_list_exn
  ) in
  Lazy.force contexts
  |> String.Map.values
  |> Fiber.parallel_map ~f:Fiber.Once.get
  |> Fiber.map ~f:List.concat

let which t s = which ~cache:t.which_cache ~path:t.path s

let install_prefix t =
  opam_config_var t "prefix" >>| function
  | Some x -> Path.of_filename_relative_to_initial_cwd x
  | None   -> Path.parent_exn t.ocaml_bin

let install_ocaml_libdir t =
  match t.kind, t.findlib_toolchain, Setup.library_destdir with
  | Default, None, Some d ->
    Fiber.return (Some (Path.of_filename_relative_to_initial_cwd d))
  | _ ->
    (* If ocamlfind is present, it has precedence over everything else. *)
    match which t "ocamlfind" with
    | Some fn ->
      let+ s =
        Process.run_capture_line ~env:t.env Strict fn ["printconf"; "destdir"]
      in
      Some (Path.of_filename_relative_to_initial_cwd s)
    | None ->
      Fiber.return None

let compiler t (mode : Mode.t) =
  match mode with
  | Byte   -> Some t.ocamlc
  | Native -> t.ocamlopt

let best_mode t : Mode.t =
  match t.ocamlopt with
  | Some _ -> Native
  | None   -> Byte

let cc_g (ctx : t) =
  if ctx.ccomp_type <> "msvc" then
    ["-g"]
  else
    []

let name t = t.name

let has_native t = Option.is_some t.ocamlopt

let lib_config t = t.lib_config
