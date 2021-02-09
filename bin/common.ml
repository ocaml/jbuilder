open Stdune
module Config = Dune_engine.Config
module Colors = Dune_rules.Colors
module Clflags = Dune_engine.Clflags
module Package = Dune_engine.Package
module Profile = Dune_rules.Profile
module Term = Cmdliner.Term
module Manpage = Cmdliner.Manpage

module Let_syntax = struct
  let ( let+ ) t f = Term.(const f $ t)

  let ( and+ ) a b = Term.(const (fun x y -> (x, y)) $ a $ b)
end

open Let_syntax

module Only_packages : sig
  type t = private
    { names : Dune_engine.Package.Name.Set.t
    ; command_line_option : string
    }

  val create :
    names:Dune_engine.Package.Name.Set.t -> command_line_option:string -> t
end = struct
  type t =
    { names : Dune_engine.Package.Name.Set.t
    ; command_line_option : string
    }

  let create ~names ~command_line_option =
    Clflags.only_packages := Some names;
    { names; command_line_option }
end

type t =
  { debug_dep_path : bool
  ; debug_findlib : bool
  ; debug_backtraces : bool
  ; debug_artifact_substitution : bool
  ; profile : Profile.t option
  ; workspace_file : Arg.Path.t option
  ; root : Workspace_root.t
  ; target_prefix : string
  ; only_packages : Only_packages.t option
  ; capture_outputs : bool
  ; x : Dune_engine.Context_name.t option
  ; diff_command : string option
  ; promote : Clflags.Promote.t option
  ; force : bool
  ; ignore_promoted_rules : bool
  ; build_dir : string
  ; no_print_directory : bool
  ; store_orig_src_dir : bool
  ; (* Original arguments for the external-lib-deps hint *)
    orig_args : string list
  ; config : Dune_engine.Config.t
  ; default_target : Arg.Dep.t (* For build & runtest only *)
  ; watch : bool
  ; stats_trace_file : string option
  ; always_show_command_line : bool
  ; promote_install_files : bool
  ; instrument_with : Dune_engine.Lib_name.t list option
  }

let workspace_file t = t.workspace_file

let x t = t.x

let profile t = t.profile

let capture_outputs t = t.capture_outputs

let root t = t.root

let config t = t.config

let only_packages t = t.only_packages

let watch t = t.watch

let default_target t = t.default_target

let prefix_target common s = common.target_prefix ^ s

let instrument_with t = t.instrument_with

(* To avoid needless recompilations under Windows, where the case of
   [Sys.getcwd] can vary between different invocations of [dune], normalize to
   lowercase. *)
let normalize_path p =
  if Sys.win32 then
    Path.External.lowercase_ascii p
  else
    p

let set_dirs c =
  if c.root.dir <> Filename.current_dir_name then Sys.chdir c.root.dir;
  Path.set_root (normalize_path (Path.External.cwd ()));
  Path.Build.set_build_dir (Path.Build.Kind.of_string c.build_dir)

let set_common_other ?log_file c ~targets =
  Config.init c.config;
  Dune_util.Log.init () ?file:log_file;
  Clflags.debug_dep_path := c.debug_dep_path;
  Clflags.debug_findlib := c.debug_findlib;
  Clflags.debug_backtraces c.debug_backtraces;
  Clflags.debug_artifact_substitution := c.debug_artifact_substitution;
  Clflags.capture_outputs := c.capture_outputs;
  Clflags.diff_command := c.diff_command;
  Clflags.promote := c.promote;
  Clflags.force := c.force;
  Clflags.watch := c.watch;
  Clflags.no_print_directory := c.no_print_directory;
  Clflags.store_orig_src_dir := c.store_orig_src_dir;
  Clflags.promote_install_files := c.promote_install_files;
  Clflags.external_lib_deps_hint :=
    List.concat
      [ [ "dune"; "external-lib-deps"; "--missing" ]
      ; c.orig_args
      ; List.map ~f:Arg.Dep.to_string_maybe_quoted targets
      ];
  Clflags.always_show_command_line := c.always_show_command_line;
  Clflags.ignore_promoted_rules := c.ignore_promoted_rules;
  Option.iter ~f:Dune_engine.Stats.enable c.stats_trace_file

let set_common ?log_file ?external_lib_deps_mode c ~targets =
  Option.iter external_lib_deps_mode ~f:(fun x ->
      Clflags.external_lib_deps_mode := x);
  set_dirs c;
  set_common_other ?log_file c ~targets

let footer =
  `Blocks
    [ `S "BUGS"
    ; `P "Check bug reports at https://github.com/ocaml/dune/issues"
    ]

let copts_sect = "COMMON OPTIONS"

let debug_backtraces =
  Arg.(
    value & flag
    & info [ "debug-backtraces" ] ~docs:copts_sect
        ~doc:{|Always print exception backtraces.|})

let examples = function
  | [] -> `Blocks []
  | _ :: _ as examples ->
    let block_of_example index (intro, ex) =
      let prose = `I (Int.to_string (index + 1) ^ ".", String.trim intro ^ ":")
      and code_lines =
        ex |> String.trim |> String.split_lines
        |> List.concat_map ~f:(fun codeline ->
               [ `Noblank; `Pre ("      " ^ codeline) ])
        (* suppress initial blank *)
        |> List.tl
      in
      `Blocks (prose :: code_lines)
    in
    let example_blocks = examples |> List.mapi ~f:block_of_example in
    `Blocks (`S Cmdliner.Manpage.s_examples :: example_blocks)

let help_secs =
  [ `S copts_sect
  ; `P "These options are common to all commands."
  ; `S "MORE HELP"
  ; `P "Use `$(mname) $(i,COMMAND) --help' for help on a single command."
  ; footer
  ]

type config_file =
  | No_config
  | Default
  | This of Path.t

let default_build_dir = "_build"

(* Allow options from term1 or exclusively options from term2. If the user
   passes options from both terms, an error is reported. *)
let one_of term1 term2 =
  Term.ret
  @@ let+ x, args1 = Term.with_used_args term1
     and+ y, args2 = Term.with_used_args term2 in
     match (args1, args2) with
     | _, [] -> `Ok x
     | [], _ -> `Ok y
     | arg1 :: _, arg2 :: _ ->
       `Error (true, sprintf "Cannot use %s and %s simultaneously" arg1 arg2)

let build_info =
  let+ build_info =
    Arg.(
      value & flag
      & info [ "build-info" ] ~docs:"OPTIONS" ~doc:"Show build information.")
  in
  if build_info then (
    let module B = Build_info.V1 in
    let pr fmt = Printf.printf (fmt ^^ "\n") in
    let ver_string v =
      match v with
      | None -> "n/a"
      | Some v -> B.Version.to_string v
    in
    pr "version: %s" (ver_string (B.version ()));
    let libs =
      B.Statically_linked_libraries.to_list ()
      |> List.map ~f:(fun lib ->
             ( B.Statically_linked_library.name lib
             , ver_string (B.Statically_linked_library.version lib) ))
      |> List.sort ~compare:Poly.compare
    in
    (match libs with
    | [] -> ()
    | _ ->
      pr "statically linked libraries:";
      let longest = String.longest_map libs ~f:fst in
      List.iter libs ~f:(fun (name, v) -> pr "- %-*s %s" longest name v));
    exit 0
  )

module Options_implied_by_dash_p = struct
  type t =
    { root : string option
    ; only_packages : Only_packages.t option
    ; ignore_promoted_rules : bool
    ; config_file : config_file
    ; profile : Profile.t option
    ; default_target : Arg.Dep.t
    ; always_show_command_line : bool
    ; promote_install_files : bool
    }

  let docs = copts_sect

  let config_file_term =
    let+ x =
      one_of
        (let+ fn =
           Arg.(
             value
             & opt (some path) None
             & info [ "config-file" ] ~docs ~docv:"FILE"
                 ~doc:"Load this configuration file instead of the default one.")
         in
         Option.map fn ~f:(fun fn -> This (Arg.Path.path fn)))
        (let+ x =
           Arg.(
             value & flag
             & info [ "no-config" ] ~docs
                 ~doc:"Do not load the configuration file")
         in
         Option.some_if x No_config)
    in
    Option.value x ~default:Default

  let packages =
    let parser s =
      let parse_one s =
        match Package.Name.of_string_opt s with
        | Some x -> Ok x
        | None ->
          ksprintf (fun s -> Error (`Msg s)) "Invalid package name: %S" s
      in
      String.split s ~on:',' |> List.map ~f:parse_one |> Result.List.all
      |> Result.map ~f:Package.Name.Set.of_list
    in
    let printer ppf set =
      Format.pp_print_string ppf
        (String.concat ~sep:","
           (Package.Name.Set.to_list set |> List.map ~f:Package.Name.to_string))
    in
    Arg.conv ~docv:"PACKAGES" (parser, printer)

  let options =
    let+ root =
      Arg.(
        value
        & opt (some dir) None
        & info [ "root" ] ~docs ~docv:"DIR"
            ~doc:
              "Use this directory as workspace root instead of guessing it. \
               Note that this option doesn't change the interpretation of \
               targets given on the command line. It is only intended for \
               scripts.")
    and+ ignore_promoted_rules =
      Arg.(
        value & flag
        & info
            [ "ignore-promoted-rules" ]
            ~docs
            ~doc:
              "Ignore rules with (mode promote), except ones with (only ...). \
               The variable %{ignoring_promoted_rules} in dune files reflects \
               whether this option was passed or not.")
    and+ config_file = config_file_term
    and+ default_target =
      Arg.(
        value
        & opt dep
            (Dep.alias ~dir:Stdune.Path.Local.root
               Dune_engine.Alias.Name.default)
        & info [ "default-target" ] ~docs ~docv:"TARGET"
            ~doc:
              {|Set the default target that when none is specified to
                      $(b,dune build).|})
    and+ always_show_command_line =
      let doc =
        "Always show the full command lines of programs executed by dune"
      in
      Arg.(value & flag & info [ "always-show-command-line" ] ~docs ~doc)
    and+ promote_install_files =
      let doc =
        "Promote the generated <package>.install files to the source tree"
      in
      Arg.(value & flag & info [ "promote-install-files" ] ~docs ~doc)
    in
    { root
    ; only_packages = None
    ; ignore_promoted_rules
    ; config_file
    ; profile = None
    ; default_target
    ; always_show_command_line
    ; promote_install_files
    }

  let release_options =
    { root = Some "."
    ; only_packages = None
    ; ignore_promoted_rules = true
    ; config_file = No_config
    ; profile = Some Profile.Release
    ; default_target =
        Arg.Dep.alias_rec ~dir:Path.Local.root Dune_engine.Alias.Name.install
    ; always_show_command_line = true
    ; promote_install_files = true
    }

  let dash_dash_release =
    let+ (_ : bool) =
      Arg.(
        value & flag
        & info [ "release" ] ~docs ~docv:"PACKAGES"
            ~doc:
              "Put $(b,dune) into a reproducible $(i,release) mode. This is in \
               fact a shorthand for $(b,--root . --ignore-promoted-rules \
               --no-config --profile release --always-show-command-line \
               --promote-install-files --default-target @install). You should \
               use this option for release builds. For instance, you must use \
               this option in your $(i,<package>.opam) files. Except if you \
               already use $(b,-p), as $(b,-p) implies this option.")
    in
    release_options

  let options =
    let+ t = one_of options dash_dash_release
    and+ only_packages =
      let+ names =
        Arg.(
          value
          & opt (some packages) None
          & info [ "only-packages" ] ~docs ~docv:"PACKAGES"
              ~doc:
                {|Ignore stanzas referring to a package that is not in
                      $(b,PACKAGES). $(b,PACKAGES) is a comma-separated list
                      of package names. Note that this has the same effect
                      as deleting the relevant stanzas from dune files.
                      It is mostly meant for releases. During development,
                      it is likely that what you want instead is to
                      build a particular $(b,<package>.install) target.|})
      in
      Option.map names ~f:(fun names ->
          Only_packages.create ~names ~command_line_option:"only-packages")
    in
    { t with only_packages }

  let dash_p =
    let+ pkgs, args =
      Term.with_used_args
        Arg.(
          value
          & opt (some packages) None
          & info
              [ "p"; "for-release-of-packages" ]
              ~docs ~docv:"PACKAGES"
              ~doc:
                "Shorthand for $(b,--release --only-packages PACKAGE). You \
                 must use this option in your $(i,<package>.opam) files, in \
                 order to build only what's necessary when your project \
                 contains multiple packages as well as getting reproducible \
                 builds.")
    in
    { release_options with
      only_packages =
        Option.map pkgs ~f:(fun names ->
            Only_packages.create ~names ~command_line_option:(List.hd args))
    }

  let term =
    let+ t = one_of options dash_p
    and+ profile =
      let doc =
        "Build profile. $(b,dev) if unspecified or $(b,release) if -p is set."
      in
      Arg.(
        value
        & opt (some profile) None
        & info [ "profile" ] ~docs
            ~env:(Arg.env_var ~doc "DUNE_PROFILE")
            ~doc:
              (Printf.sprintf
                 "Select the build profile, for instance $(b,dev) or \
                  $(b,release). The default is $(b,%s)."
                 (Profile.to_string Dune_rules.Profile.default)))
    in
    match profile with
    | None -> t
    | Some _ -> { t with profile }
end

let display_term =
  one_of
    (let+ verbose =
       Arg.(
         value & flag
         & info [ "verbose" ] ~docs:copts_sect
             ~doc:"Same as $(b,--display verbose)")
     in
     Option.some_if verbose Config.Display.Verbose)
    Arg.(
      value
      & opt (some (enum Config.Display.all)) None
      & info [ "display" ] ~docs:copts_sect ~docv:"MODE"
          ~doc:
            {|Control the display mode of Dune.
         See $(b,dune-config\(5\)) for more details.|})

let config_of_file = function
  | No_config -> Config.default
  | This fname -> Config.load_config_file fname
  | Default ->
    if Config.inside_dune then
      Config.default
    else
      Config.load_user_config_file ()

let term =
  let docs = copts_sect in
  let+ concurrency =
    let arg =
      Arg.conv
        ( (fun s ->
            Result.map_error (Dune_engine.Config.Concurrency.of_string s)
              ~f:(fun s -> `Msg s))
        , fun pp x ->
            Format.pp_print_string pp
              (Dune_engine.Config.Concurrency.to_string x) )
    in
    Arg.(
      value
      & opt (some arg) None
      & info [ "j" ] ~docs ~docv:"JOBS"
          ~doc:{|Run no more than $(i,JOBS) commands simultaneously.|})
  and+ sandboxing_preference =
    let arg =
      Arg.conv
        ( (fun s ->
            Result.map_error (Dune_engine.Sandbox_mode.of_string s) ~f:(fun s ->
                `Msg s))
        , fun pp x ->
            Format.pp_print_string pp (Dune_engine.Sandbox_mode.to_string x) )
    in
    Arg.(
      value
      & opt (some arg) None
      & info [ "sandbox" ]
          ~env:
            (Arg.env_var
               ~doc:"Sandboxing mode to use by default. (see --sandbox)"
               "DUNE_SANDBOX")
          ~doc:
            (Printf.sprintf
               "Sandboxing mode to use by default. Some actions require a \
                certain sandboxing mode, so they will ignore this setting. The \
                allowed values are: %s."
               (String.concat ~sep:", "
                  (List.map Dune_engine.Sandbox_mode.all
                     ~f:Dune_engine.Sandbox_mode.to_string))))
  and+ debug_dep_path =
    Arg.(
      value & flag
      & info
          [ "debug-dependency-path" ]
          ~docs
          ~doc:
            {|In case of error, print the dependency path from
                    the targets on the command line to the rule that failed.
                  |})
  and+ debug_findlib =
    Arg.(
      value & flag
      & info [ "debug-findlib" ] ~docs ~doc:{|Debug the findlib sub-system.|})
  and+ debug_backtraces = debug_backtraces
  and+ debug_artifact_substitution =
    Arg.(
      value & flag
      & info
          [ "debug-artifact-substitution" ]
          ~docs ~doc:"Print debugging info about artifact substitution")
  and+ terminal_persistence =
    let modes = Config.Terminal_persistence.all in
    let doc =
      let f s = fst s |> Printf.sprintf "$(b,%s)" in
      Printf.sprintf
        {|Changes how the log of build results are displayed to the
          console between rebuilds while in $(b,--watch) mode. Supported modes:
          %s.|}
        (List.map ~f modes |> String.concat ~sep:", ")
    in
    Arg.(
      value
      & opt (some (enum modes)) None
      & info [ "terminal-persistence" ] ~docs ~docv:"MODE" ~doc)
  and+ display = display_term
  and+ no_buffer =
    let doc =
      {|Do not buffer the output of commands executed by dune. By default dune
        buffers the output of subcommands, in order to prevent interleaving when
        multiple commands are executed in parallel. However, this can be an
        issue when debugging long running tests. With $(b,--no-buffer), commands
        have direct access to the terminal. Note that as a result their output
        won't be captured in the log file.

        You should use this option in conjunction with $(b,-j 1), to avoid
        interleaving. Additionally you should use $(b,--verbose) as well, to
        make sure that commands are printed before they are being executed.|}
    in
    Arg.(value & flag & info [ "no-buffer" ] ~docs ~docv:"DIR" ~doc)
  and+ workspace_file =
    let doc = "Use this specific workspace file instead of looking it up." in
    Arg.(
      value
      & opt (some path) None
      & info [ "workspace" ] ~docs ~docv:"FILE" ~doc
          ~env:(Arg.env_var ~doc "DUNE_WORKSPACE"))
  and+ promote =
    one_of
      (let+ auto =
         Arg.(
           value & flag
           & info [ "auto-promote" ] ~docs
               ~doc:
                 "Automatically promote files. This is similar to running\n\
                 \                   $(b,dune promote) after the build.")
       in
       Option.some_if auto Clflags.Promote.Automatically)
      (let+ disable =
         let doc = "Disable all promotion rules" in
         let env = Arg.env_var ~doc "DUNE_DISABLE_PROMOTION" in
         Arg.(value & flag & info [ "disable-promotion" ] ~docs ~env ~doc)
       in
       Option.some_if disable Clflags.Promote.Never)
  and+ force =
    Arg.(
      value & flag
      & info [ "force"; "f" ]
          ~doc:
            "Force actions associated to aliases to be re-executed even\n\
            \                   if their dependencies haven't changed.")
  and+ watch =
    Arg.(
      value & flag
      & info [ "watch"; "w" ]
          ~doc:
            "Instead of terminating build after completion, wait continuously \
             for file changes.")
  and+ { Options_implied_by_dash_p.root
       ; only_packages
       ; ignore_promoted_rules
       ; config_file
       ; profile
       ; default_target
       ; always_show_command_line
       ; promote_install_files
       } =
    Options_implied_by_dash_p.term
  and+ x =
    Arg.(
      value
      & opt (some Arg.context_name) None
      & info [ "x" ] ~docs ~doc:{|Cross-compile using this toolchain.|})
  and+ build_dir =
    let doc = "Specified build directory. _build if unspecified" in
    Arg.(
      value
      & opt (some string) None
      & info [ "build-dir" ] ~docs ~docv:"FILE"
          ~env:(Arg.env_var ~doc "DUNE_BUILD_DIR")
          ~doc)
  and+ diff_command =
    Arg.(
      value
      & opt (some string) None
      & info [ "diff-command" ] ~docs
          ~doc:
            "Shell command to use to diff files.\n\
            \                   Use - to disable printing the diff.")
  and+ stats_trace_file =
    Arg.(
      value
      & opt (some string) None
      & info [ "trace-file" ] ~docs ~docv:"FILE"
          ~doc:
            "Output trace data in catapult format\n\
            \                   (compatible with chrome://tracing)")
  and+ no_print_directory =
    Arg.(
      value & flag
      & info [ "no-print-directory" ] ~docs
          ~doc:"Suppress \"Entering directory\" messages")
  and+ store_orig_src_dir =
    let doc = "Store original source location in dune-package metadata" in
    Arg.(
      value & flag
      & info
          [ "store-orig-source-dir" ]
          ~docs
          ~env:(Arg.env_var ~doc "DUNE_STORE_ORIG_SOURCE_DIR")
          ~doc)
  and+ cache_mode =
    let doc =
      Printf.sprintf "Activate binary cache (%s). Default is `%s'."
        (Arg.doc_alts_enum Config.Caching.Mode.all)
        (Config.Caching.Mode.to_string Config.default.cache_mode)
    in
    Arg.(
      value
      & opt (some (enum Config.Caching.Mode.all)) None
      & info [ "cache" ] ~docs ~env:(Arg.env_var ~doc "DUNE_CACHE") ~doc)
  and+ cache_transport =
    let doc = "Binary cache protocol" in
    Arg.(
      value
      & opt (some (enum Config.Caching.Transport.all)) None
      & info [ "cache-transport" ] ~docs
          ~env:(Arg.env_var ~doc "DUNE_CACHE_TRANSPORT")
          ~doc)
  and+ cache_duplication =
    let doc = "Binary cache duplication mode" in
    Arg.(
      value
      & opt (some (enum Config.Caching.Duplication.all)) None
      & info [ "cache-duplication" ] ~docs
          ~env:(Arg.env_var ~doc "DUNE_CACHE_DUPLICATION")
          ~doc)
  and+ cache_check_probability =
    let doc =
      "Probability cached rules are rerun to check for reproducibility"
    in
    Arg.(
      value
      & opt float Config.default.cache_check_probability
      & info
          [ "cache-check-probability" ]
          ~docs
          ~env:(Arg.env_var ~doc "DUNE_CACHE_CHECK_PROBABILITY")
          ~doc)
  and+ () = build_info
  and+ instrument_with =
    let doc =
      {|"Enable instrumentation by $(b,BACKENDS).
        $(b,BACKENDS) is a comma-separated list of library names,
        each one of which must declare an instrumentation backend.|}
    in
    Arg.(
      value
      & opt (some (list lib_name)) None
      & info [ "instrument-with" ] ~docs
          ~env:(Arg.env_var ~doc "DUNE_INSTRUMENT_WITH")
          ~docv:"BACKENDS" ~doc)
  in
  let build_dir = Option.value ~default:default_build_dir build_dir in
  let root = Workspace_root.create ~specified_by_user:root in
  let config = config_of_file config_file in
  let config =
    Config.merge config
      { display
      ; concurrency
      ; sandboxing_preference =
          Option.map sandboxing_preference ~f:(fun x -> [ x ])
      ; terminal_persistence
      ; cache_mode
      ; cache_transport
      ; cache_check_probability = Some cache_check_probability
      ; cache_duplication
      ; cache_trim_period = None
      ; cache_trim_size = None
      }
  in
  let config =
    Config.adapt_display config
      ~output_is_a_tty:(Lazy.force Ansi_color.stderr_supports_color)
  in
  { debug_dep_path
  ; debug_findlib
  ; debug_backtraces
  ; debug_artifact_substitution
  ; profile
  ; capture_outputs = not no_buffer
  ; workspace_file
  ; root
  ; orig_args = []
  ; target_prefix =
      String.concat ~sep:"" (List.map root.to_cwd ~f:(sprintf "%s/"))
  ; diff_command
  ; promote
  ; force
  ; ignore_promoted_rules
  ; only_packages
  ; x
  ; config
  ; build_dir
  ; no_print_directory
  ; store_orig_src_dir
  ; default_target
  ; watch
  ; stats_trace_file
  ; always_show_command_line
  ; promote_install_files
  ; instrument_with
  }

let term =
  let+ t, orig_args = Term.with_used_args term in
  { t with orig_args }

let config_term =
  let+ config_file = Options_implied_by_dash_p.config_file_term in
  config_of_file config_file

let context_arg ~doc =
  Arg.(
    value
    & opt Arg.context_name Dune_engine.Context_name.default
    & info [ "context" ] ~docv:"CONTEXT" ~doc)
