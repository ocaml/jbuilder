open! Stdune
open! Import

let local_install_dir ~context =
  let context = Context_name.to_string context in
  Path.Build.relative Dpath.Build.install_dir context

let local_install_lib_root ~context =
  Path.Build.relative (local_install_dir ~context) "lib"

let local_install_bin_dir ~context =
  Path.Build.relative (local_install_dir ~context) "bin"

let local_install_man_dir ~context =
  Path.Build.relative (local_install_dir ~context) "bin"

let local_install_lib_dir ~context ~package =
  Path.Build.relative
    (local_install_lib_root ~context)
    (Package.Name.to_string package)

let dev_null_fn =
  if Sys.win32 then
    "nul"
  else
    "/dev/null"

let dev_null = Path.of_filename_relative_to_initial_cwd dev_null_fn

let open_null flags = lazy (Unix.openfile dev_null_fn flags 0o666)

let dev_null_in = open_null [ Unix.O_RDONLY ]

let dev_null_out = open_null [ Unix.O_WRONLY ]

let dune_keep_fname = ".dune-keep"

let inside_emacs = Option.is_some (Env.get Env.initial "INSIDE_EMACS")

let inside_dune = Option.is_some (Env.get Env.initial "INSIDE_DUNE")

let inside_ci = Option.is_some (Env.get Env.initial "CI")

let show_full_command_on_error () =
  !Clflags.always_show_command_line
  || (* We want to show command lines in the CI, but not when running inside
        dune. Otherwise tests would yield different result whether they are
        executed locally or in the CI. *)
  (inside_ci && not inside_dune)

open Dune_lang.Decoder

(* the configuration file use the same version numbers as dune-project files for
   simplicity *)
let syntax = Stanza.syntax

module Terminal_persistence = struct
  type t =
    | Preserve
    | Clear_on_rebuild

  let all = [ ("preserve", Preserve); ("clear-on-rebuild", Clear_on_rebuild) ]

  let of_string = function
    | "preserve" -> Ok Preserve
    | "clear-on-rebuild" -> Ok Clear_on_rebuild
    | _ ->
      Error
        "invalid terminal-persistence value, must be 'preserve' or \
         'clear-on-rebuild'"

  let to_string = function
    | Preserve -> "preserve"
    | Clear_on_rebuild -> "clear-on-rebuild"

  let decode =
    plain_string (fun ~loc s ->
        match of_string s with
        | Error m -> User_error.raise ~loc [ Pp.text m ]
        | Ok s -> s)
end

module Display = struct
  type t =
    | Progress
    | Short
    | Verbose
    | Quiet

  let all =
    [ ("progress", Progress)
    ; ("verbose", Verbose)
    ; ("short", Short)
    ; ("quiet", Quiet)
    ]

  let decode = enum all

  let to_string = function
    | Progress -> "progress"
    | Quiet -> "quiet"
    | Short -> "short"
    | Verbose -> "verbose"

  let console_backend = function
    | Progress -> Console.Backend.progress
    | Short
    | Verbose
    | Quiet ->
      Console.Backend.dumb
end

module Concurrency = struct
  type t =
    | Fixed of int
    | Auto

  let error =
    Error "invalid concurrency value, must be 'auto' or a positive number"

  let of_string = function
    | "auto" -> Ok Auto
    | s -> (
      match int_of_string s with
      | exception _ -> error
      | n ->
        if n >= 1 then
          Ok (Fixed n)
        else
          error)

  let decode =
    plain_string (fun ~loc s ->
        match of_string s with
        | Error m -> User_error.raise ~loc [ Pp.text m ]
        | Ok s -> s)

  let to_string = function
    | Auto -> "auto"
    | Fixed n -> string_of_int n
end

module Sandboxing_preference = struct
  type t = Sandbox_mode.t list

  let decode =
    repeat
      (plain_string (fun ~loc s ->
           match Sandbox_mode.of_string s with
           | Error m -> User_error.raise ~loc [ Pp.text m ]
           | Ok s -> s))
end

module Caching = struct
  module Mode = struct
    type t =
      | Disabled
      | Enabled

    let all = [ ("disabled", Disabled); ("enabled", Enabled) ]

    let to_string = function
      | Enabled -> "enabled"
      | Disabled -> "disabled"

    let decode = enum all
  end

  module Transport = struct
    type t =
      | Daemon
      | Direct

    let to_string = function
      | Daemon -> "daemon"
      | Direct -> "direct"

    let all = [ ("daemon", Daemon); ("direct", Direct) ]

    let decode = enum all
  end

  module Duplication = struct
    type t = Cache.Duplication_mode.t option

    let all =
      ("auto", None)
      ::
      List.map
        ~f:(fun (name, mode) -> (name, Some mode))
        Cache.Duplication_mode.all

    let decode = enum all
  end
end

module type S = sig
  type 'a field

  type t =
    { display : Display.t field
    ; concurrency : Concurrency.t field
    ; terminal_persistence : Terminal_persistence.t field
    ; sandboxing_preference : Sandboxing_preference.t field
    ; cache_mode : Caching.Mode.t field
    ; cache_transport : Caching.Transport.t field
    ; cache_check_probability : float field
    ; cache_duplication : Caching.Duplication.t field
    ; cache_trim_period : int field
    ; cache_trim_size : int64 field
    }
end

module rec M : (S with type 'a field = 'a) = M

include M

module rec Partial : (S with type 'a field := 'a option) = Partial

let merge t (partial : Partial.t) =
  let field from_t from_partial = Option.value from_partial ~default:from_t in
  { display = field t.display partial.display
  ; concurrency = field t.concurrency partial.concurrency
  ; terminal_persistence =
      field t.terminal_persistence partial.terminal_persistence
  ; sandboxing_preference =
      field t.sandboxing_preference partial.sandboxing_preference
  ; cache_mode = field t.cache_mode partial.cache_mode
  ; cache_transport = field t.cache_transport partial.cache_transport
  ; cache_check_probability =
      field t.cache_check_probability partial.cache_check_probability
  ; cache_duplication = field t.cache_duplication partial.cache_duplication
  ; cache_trim_period = field t.cache_trim_period partial.cache_trim_period
  ; cache_trim_size = field t.cache_trim_size partial.cache_trim_size
  }

let default =
  { display =
      (if inside_dune then
        Quiet
      else
        Progress)
  ; concurrency =
      (if inside_dune then
        Fixed 1
      else
        Auto)
  ; terminal_persistence = Terminal_persistence.Preserve
  ; sandboxing_preference = []
  ; cache_mode = Disabled
  ; cache_transport = Daemon
  ; cache_check_probability = 0.
  ; cache_trim_period = 10 * 60
  ; cache_trim_size = 10_000_000_000L
  ; cache_duplication = None
  }

let decode =
  let+ display = field "display" Display.decode ~default:default.display
  and+ concurrency =
    field "jobs" Concurrency.decode ~default:default.concurrency
  and+ terminal_persistence =
    field "terminal-persistence" Terminal_persistence.decode
      ~default:default.terminal_persistence
  and+ sandboxing_preference =
    field "sandboxing_preference" Sandboxing_preference.decode
      ~default:default.sandboxing_preference
  and+ cache_mode =
    field "cache"
      (Dune_lang.Syntax.since Stanza.syntax (2, 0) >>> Caching.Mode.decode)
      ~default:default.cache_mode
  and+ cache_transport =
    field "cache-transport"
      (Dune_lang.Syntax.since Stanza.syntax (2, 0) >>> Caching.Transport.decode)
      ~default:default.cache_transport
  and+ cache_check_probability =
    field "cache-check-probability"
      (Dune_lang.Syntax.since Stanza.syntax (2, 7) >>> Dune_lang.Decoder.float)
      ~default:default.cache_check_probability
  and+ cache_duplication =
    field "cache-duplication"
      (Dune_lang.Syntax.since Stanza.syntax (2, 1)
      >>> Caching.Duplication.decode)
      ~default:default.cache_duplication
  and+ cache_trim_period =
    field "cache-trim-period" Dune_lang.Decoder.duration
      ~default:default.cache_trim_period
  and+ cache_trim_size =
    field "cache-trim-size" Dune_lang.Decoder.bytes_unit
      ~default:default.cache_trim_size
  and+ () = Dune_lang.Versioned_file.no_more_lang in
  { display
  ; concurrency
  ; terminal_persistence
  ; sandboxing_preference
  ; cache_mode
  ; cache_transport
  ; cache_check_probability
  ; cache_duplication
  ; cache_trim_period
  ; cache_trim_size
  }

let decode = fields decode

let user_config_file =
  Path.relative
    (Path.of_filename_relative_to_initial_cwd Xdg.config_dir)
    "dune/config"

include Dune_lang.Versioned_file.Make (struct
  type t = unit
end)

let () = Lang.register syntax ()

let load_config_file p =
  load_exn p ~f:(fun lang ->
      String_with_vars.set_decoding_env (Pform.Env.initial lang.version) decode)

let load_user_config_file () =
  if Path.exists user_config_file then
    load_config_file user_config_file
  else
    default

let adapt_display config ~output_is_a_tty =
  (* Progress isn't meaningful if inside a terminal (or emacs), so reset the
     display to Quiet if the output is getting piped to a file or something. *)
  let config =
    if config.display = Progress && (not output_is_a_tty) && not inside_emacs
    then
      { config with display = Quiet }
    else
      config
  in
  (* Similarly, terminal clearing is meaningless if stderr doesn't support ANSI
     codes, so revert-back to Preserve in that case *)
  if config.terminal_persistence = Clear_on_rebuild && not output_is_a_tty then
    { config with terminal_persistence = Terminal_persistence.Preserve }
  else
    config

let to_dyn config =
  Dyn.Encoder.record
    [ ("display", Dyn.Encoder.string (Display.to_string config.display))
    ; ( "concurrency"
      , Dyn.Encoder.string (Concurrency.to_string config.concurrency) )
    ; ( "terminal_persistence"
      , Dyn.Encoder.string
          (Terminal_persistence.to_string config.terminal_persistence) )
    ; ( "sandboxing_preference"
      , (Dyn.Encoder.list Dyn.Encoder.string)
          (List.map ~f:Sandbox_mode.to_string config.sandboxing_preference) )
    ; ( "cache_mode"
      , Dyn.Encoder.string (Caching.Mode.to_string config.cache_mode) )
    ; ( "cache_transport"
      , Dyn.Encoder.string (Caching.Transport.to_string config.cache_transport)
      )
    ; ( "cache_check_probability"
      , Dyn.Encoder.float config.cache_check_probability )
    ; ("cache_trim_period", Dyn.Encoder.int config.cache_trim_period)
    ; ("cache_trim_size", Dyn.Encoder.int64 config.cache_trim_size)
    ]

let global = Fdecl.create to_dyn

let t () = Fdecl.get global

let init t =
  Fdecl.set global t;
  Console.Backend.set (Display.console_backend t.display);
  Log.verbose := t.display = Verbose
