open! Stdune
open Import
open Fiber.O

type ('a, 'b) failure_mode =
  | Strict : ('a, 'a) failure_mode
  | Accept : int Predicate_lang.t -> ('a, ('a, int) result) failure_mode

let accepted_codes : type a b. (a, b) failure_mode -> int -> bool = function
  | Strict -> Int.equal 0
  | Accept exit_codes ->
    fun i ->
      Predicate_lang.exec exit_codes ~standard:(Predicate_lang.Element 0)
        (Int.equal i)

let map_result : type a b. (a, b) failure_mode -> int -> f:(unit -> a) -> b =
 fun mode t ~f ->
  match mode with
  | Strict -> f ()
  | Accept _ -> (
    match t with
    | 0 -> Ok (f ())
    | n -> Error n)

module Io = struct
  type input = Input

  type output = Output

  type 'a mode =
    | In : input mode
    | Out : output mode

  type kind =
    | File of Path.t
    | Null
    | Terminal

  type status =
    | Keep_open
    | Close_after_exec
    | Closed

  type 'a channel =
    | In_chan : in_channel -> input channel
    | Out_chan : out_channel -> output channel

  let descr_of_channel : type a. a channel -> _ = function
    | In_chan ic -> Unix.descr_of_in_channel ic
    | Out_chan oc -> Unix.descr_of_out_channel oc

  let mode_of_channel : type a. a channel -> a mode = function
    | In_chan _ -> In
    | Out_chan _ -> Out

  let channel_of_descr : type a. _ -> a mode -> a channel =
   fun fd mode ->
    match mode with
    | In -> In_chan (Unix.in_channel_of_descr fd)
    | Out -> Out_chan (Unix.out_channel_of_descr fd)

  let close_channel : type a. a channel -> unit = function
    | Out_chan ch -> close_out ch
    | In_chan ch -> close_in ch

  type 'a t =
    { kind : kind
    ; mode : 'a mode
    ; fd : Unix.file_descr Lazy.t
    ; channel : 'a channel Lazy.t
    ; mutable status : status
    }

  let terminal ch =
    let fd = descr_of_channel ch in
    { kind = Terminal
    ; mode = mode_of_channel ch
    ; fd = lazy fd
    ; channel = lazy ch
    ; status = Keep_open
    }

  let stdout = terminal (Out_chan stdout)

  let stderr = terminal (Out_chan stderr)

  let stdin = terminal (In_chan stdin)

  let null (type a) (mode : a mode) : a t =
    let fd =
      match mode with
      | In -> Config.dev_null_in
      | Out -> Config.dev_null_out
    in
    let channel = lazy (channel_of_descr (Lazy.force fd) mode) in
    { kind = Null; mode; fd; channel; status = Close_after_exec }

  let file : type a. _ -> a mode -> a t =
   fun fn mode ->
    let flags =
      match mode with
      | Out -> [ Unix.O_WRONLY; O_CREAT; O_TRUNC; O_SHARE_DELETE ]
      | In -> [ O_RDONLY; O_SHARE_DELETE ]
    in
    let fd = lazy (Unix.openfile (Path.to_string fn) flags 0o666) in
    let channel = lazy (channel_of_descr (Lazy.force fd) mode) in
    { kind = File fn; mode; fd; channel; status = Close_after_exec }

  let flush : type a. a t -> unit =
   fun t ->
    if Lazy.is_val t.channel then
      match Lazy.force t.channel with
      | Out_chan oc -> flush oc
      | In_chan _ -> ()

  let fd t =
    flush t;
    Lazy.force t.fd

  let out_channel = function
    | { channel = (lazy (Out_chan oc)); _ } -> oc

  let release t =
    match t.status with
    | Closed -> ()
    | Keep_open -> flush t
    | Close_after_exec ->
      t.status <- Closed;
      if Lazy.is_val t.channel then
        close_channel (Lazy.force t.channel)
      else
        Unix.close (Lazy.force t.fd)

  let multi_use t = { t with status = Keep_open }
end

type purpose =
  | Internal_job
  | Build_job of Path.Build.Set.t

let io_to_redirection_path (kind : Io.kind) =
  match kind with
  | Terminal -> None
  | Null -> Some (Path.to_string Config.dev_null)
  | File fn -> Some (Path.to_string fn)

let command_line_enclosers ~dir ~(stdout_to : Io.output Io.t)
    ~(stderr_to : Io.output Io.t) ~(stdin_from : Io.input Io.t) =
  let quote fn = String.quote_for_shell (Path.to_string fn) in
  let prefix, suffix =
    match dir with
    | None -> ("", "")
    | Some dir -> (sprintf "(cd %s && " (quote dir), ")")
  in
  let suffix =
    match stdin_from.kind with
    | Null
    | Terminal ->
      suffix
    | File fn -> suffix ^ " < " ^ quote fn
  in
  let suffix =
    match
      ( io_to_redirection_path stdout_to.kind
      , io_to_redirection_path stderr_to.kind )
    with
    | Some fn1, Some fn2 when String.equal fn1 fn2 ->
      " &> " ^ String.quote_for_shell fn1
    | path_out, path_err ->
      let add_to_suffix suffix path redirect =
        match path with
        | None -> suffix
        | Some path -> suffix ^ redirect ^ String.quote_for_shell path
      in
      let suffix = add_to_suffix suffix path_out " > " in
      add_to_suffix suffix path_err " 2> "
  in
  (prefix, suffix)

let command_line ~prog ~args ~dir ~stdout_to ~stderr_to ~stdin_from =
  let s =
    List.map (prog :: args) ~f:String.quote_for_shell |> String.concat ~sep:" "
  in
  let prefix, suffix =
    command_line_enclosers ~dir ~stdout_to ~stderr_to ~stdin_from
  in
  prefix ^ s ^ suffix

module Fancy = struct
  let split_prog s =
    let len = String.length s in
    if len = 0 then
      ("", "", "")
    else
      let rec find_prog_start i =
        if i < 0 then
          0
        else
          match s.[i] with
          | '\\'
          | '/' ->
            i + 1
          | _ -> find_prog_start (i - 1)
      in
      let prog_end =
        match s.[len - 1] with
        | '"' -> len - 1
        | _ -> len
      in
      let prog_start = find_prog_start (prog_end - 1) in
      let prog_end =
        match String.index_from s prog_start '.' with
        | None -> prog_end
        | Some i -> i
      in
      let before = String.take s prog_start in
      let after = String.drop s prog_end in
      let prog = String.sub s ~pos:prog_start ~len:(prog_end - prog_start) in
      (before, prog, after)

  let color_combos =
    let open Ansi_color.Style in
    [| [ fg_blue; bg_bright_green ]
     ; [ fg_red; bg_bright_yellow ]
     ; [ fg_yellow; bg_blue ]
     ; [ fg_magenta; bg_bright_cyan ]
     ; [ fg_bright_green; bg_blue ]
     ; [ fg_bright_yellow; bg_red ]
     ; [ fg_blue; bg_yellow ]
     ; [ fg_bright_cyan; bg_magenta ]
    |]

  let colorize_prog s =
    let len = String.length s in
    if len = 0 then
      Pp.verbatim s
    else
      let before, prog, after = split_prog s in
      let styles =
        let hash = Hashtbl.hash prog in
        let styles = color_combos.(hash mod Array.length color_combos) in
        User_message.Style.Ansi_styles styles
      in
      Pp.seq (Pp.verbatim before)
        (Pp.seq (Pp.tag styles (Pp.verbatim prog)) (Pp.verbatim after))

  let rec colorize_args = function
    | [] -> []
    | "-o" :: fn :: rest ->
      Pp.verbatim "-o"
      ::
      Pp.tag
        (User_message.Style.Ansi_styles Ansi_color.Style.[ bold; fg_green ])
        (Pp.verbatim (String.quote_for_shell fn))
      :: colorize_args rest
    | x :: rest -> Pp.verbatim (String.quote_for_shell x) :: colorize_args rest

  let command_line ~prog ~args ~dir ~stdout_to ~stderr_to ~stdin_from =
    let open Pp.O in
    let prog = colorize_prog (String.quote_for_shell prog) in
    let pp = Pp.concat ~sep:(Pp.char ' ') (prog :: colorize_args args) in
    let prefix, suffix =
      command_line_enclosers ~dir ~stdout_to ~stderr_to ~stdin_from
    in
    Pp.verbatim prefix ++ pp ++ Pp.verbatim suffix

  let pp_purpose = function
    | Internal_job -> Pp.verbatim "(internal)"
    | Build_job targets -> (
      let rec split_paths targets_acc ctxs_acc = function
        | [] -> (List.rev targets_acc, Context_name.Set.to_list ctxs_acc)
        | path :: rest -> (
          let add_ctx ctx acc =
            if Context_name.is_default ctx then
              acc
            else
              Context_name.Set.add acc ctx
          in
          match Dpath.analyse_target path with
          | Other path ->
            split_paths (Path.Build.to_string path :: targets_acc) ctxs_acc rest
          | Regular (ctx, filename) ->
            split_paths
              (Path.Source.to_string filename :: targets_acc)
              (add_ctx ctx ctxs_acc) rest
          | Alias (ctx, name) ->
            split_paths
              (("alias " ^ Path.Source.to_string name) :: targets_acc)
              (add_ctx ctx ctxs_acc) rest
          | Install (ctx, name) ->
            split_paths
              (("install " ^ Path.Source.to_string name) :: targets_acc)
              (add_ctx ctx ctxs_acc) rest)
      in
      let targets = Path.Build.Set.to_list targets in
      let target_names, contexts =
        split_paths [] Context_name.Set.empty targets
      in
      let targets =
        List.map target_names ~f:Filename.split_extension_after_dot
        |> String.Map.of_list_multi |> String.Map.to_list
        |> List.map ~f:(fun (prefix, suffixes) ->
               match suffixes with
               | [] -> assert false
               | [ suffix ] -> prefix ^ suffix
               | _ -> sprintf "%s{%s}" prefix (String.concat ~sep:"," suffixes))
        |> String.concat ~sep:","
      in
      let pp = Pp.verbatim targets in
      match contexts with
      | [] -> pp
      | l ->
        let open Pp.O in
        pp ++ Pp.char ' '
        ++ Pp.tag User_message.Style.Details
             (Pp.char '['
             ++ Pp.concat_map l ~sep:(Pp.char ',') ~f:(fun ctx ->
                    Pp.verbatim (Context_name.to_string ctx))
             ++ Pp.char ']'))
end

let gen_id =
  let next = ref (-1) in
  fun () ->
    incr next;
    !next

let cmdline_approximate_length prog args =
  List.fold_left args ~init:(String.length prog) ~f:(fun acc arg ->
      acc + String.length arg)

let pp_id id =
  let open Pp.O in
  Pp.char '[' ++ Pp.tag User_message.Style.Id (Pp.textf "%d" id) ++ Pp.char ']'

module Exit_status = struct
  type error =
    | Failed of int
    | Signaled of string

  type t = (int, error) result

  let parse_output = function
    | "" -> None
    | s ->
      Some
        (Pp.map_tags (Ansi_color.parse s) ~f:(fun styles ->
             User_message.Style.Ansi_styles styles))

  (* In this module, we don't need the "Error: " prefix given that it is already
     included in the error message from the command. *)
  let fail paragraphs = raise (User_error.E (User_message.make paragraphs))

  let handle_verbose t ~ok_codes ~id ~output ~command_line =
    let open Pp.O in
    let output = parse_output output in
    match t with
    | Ok n ->
      Option.iter output ~f:(fun output ->
          Console.print_user_message
            (User_message.make
               [ Pp.tag User_message.Style.Kwd (Pp.verbatim "Output")
                 ++ pp_id id ++ Pp.char ':'
               ; output
               ]));
      if not (ok_codes n) then
        User_warning.emit
          [ Pp.tag User_message.Style.Kwd (Pp.verbatim "Command")
            ++ Pp.space ++ pp_id id
            ++ Pp.textf
                 " exited with code %d, but I'm ignoring it, hope that's OK." n
          ];
      n
    | Error err ->
      let msg =
        match err with
        | Failed n -> sprintf "exited with code %d" n
        | Signaled signame -> sprintf "got signal %s" signame
      in
      fail
        (Pp.tag User_message.Style.Kwd (Pp.verbatim "Command")
         ++ Pp.space ++ pp_id id ++ Pp.space ++ Pp.text msg ++ Pp.char ':'
         ::
         Pp.tag User_message.Style.Prompt (Pp.char '$')
         ++ Pp.char ' ' ++ command_line
         :: Option.to_list output)

  (* Check if the command output starts with a location, ignoring ansi escape
     sequences *)
  let outputs_starts_with_location =
    let rec loop s pos len prefix =
      match prefix with
      | [] -> true
      | c :: rest -> (
        pos < len
        &&
        match s.[pos] with
        | '\027' -> (
          match String.index_from s pos 'm' with
          | None -> false
          | Some pos -> loop s (pos + 1) len prefix)
        | c' -> c = c' && loop s (pos + 1) len rest)
    in
    fun output ->
      loop output 0 (String.length output) [ 'F'; 'i'; 'l'; 'e'; ' ' ]

  let handle_non_verbose t ~display ~purpose ~output ~prog ~command_line =
    let open Pp.O in
    let show_command =
      Config.show_full_command_on_error ()
      || not (outputs_starts_with_location output)
    in
    let output = parse_output output in
    let _, progname, _ = Fancy.split_prog prog in
    let progname_and_purpose tag =
      let progname = sprintf "%12s" progname in
      Pp.tag tag (Pp.verbatim progname)
      ++ Pp.char ' ' ++ Fancy.pp_purpose purpose
    in
    match t with
    | Ok n ->
      if
        Option.is_some output
        || (display = Config.Display.Short && purpose <> Internal_job)
      then
        Console.print_user_message
          (User_message.make
             (if show_command then
               progname_and_purpose Ok :: Option.to_list output
             else
               Option.to_list output));
      n
    | Error err ->
      let msg =
        match err with
        | Failed n ->
          if show_command then
            sprintf "(exit %d)" n
          else
            fail (Option.to_list output)
        | Signaled signame -> sprintf "(got signal %s)" signame
      in
      fail
        (progname_and_purpose Error ++ Pp.char ' '
         ++ Pp.tag User_message.Style.Error (Pp.verbatim msg)
         ::
         Pp.tag User_message.Style.Details (Pp.verbatim command_line)
         :: Option.to_list output)
end

let default_env = lazy (Dtemp.add_to_env Env.initial)

let run_internal ?dir ?(stdout_to = Io.stdout) ?(stderr_to = Io.stderr)
    ?(stdin_from = Io.null In) ~env ~purpose fail_mode prog args =
  Scheduler.with_job_slot (fun () ->
      let display = (Config.t ()).display in
      let dir =
        match dir with
        | None -> dir
        | Some p ->
          if Path.is_root p then
            None
          else
            Some p
      in
      let id = gen_id () in
      let ok_codes = accepted_codes fail_mode in
      let prog_str = Path.reach_for_running ?from:dir prog in
      let command_line =
        command_line ~prog:prog_str ~args ~dir ~stdout_to ~stderr_to ~stdin_from
      in
      let fancy_command_line =
        match display with
        | Verbose ->
          let open Pp.O in
          let cmdline =
            Fancy.command_line ~prog:prog_str ~args ~dir ~stdout_to ~stderr_to
              ~stdin_from
          in
          Console.print_user_message
            (User_message.make
               [ Pp.tag User_message.Style.Kwd (Pp.verbatim "Running")
                 ++ pp_id id ++ Pp.verbatim ": " ++ cmdline
               ]);
          cmdline
        | _ -> Pp.nop
      in
      let args, response_file =
        if Sys.win32 && cmdline_approximate_length prog_str args >= 1024 then (
          match Response_file.get ~prog with
          | Not_supported -> (args, None)
          | Zero_terminated_strings arg ->
            let fn = Temp.create File ~prefix:"responsefile" ~suffix:".data" in
            Stdune.Io.with_file_out fn ~f:(fun oc ->
                List.iter args ~f:(fun arg ->
                    output_string oc arg;
                    output_char oc '\000'));
            ([ arg; Path.to_string fn ], Some fn)
        ) else
          (args, None)
      in
      let argv = prog_str :: args in
      let output_filename, stdout_to, stderr_to =
        match (stdout_to.kind, stderr_to.kind) with
        | Terminal, _
        | _, Terminal
          when !Clflags.capture_outputs ->
          let fn = Temp.create File ~prefix:"dune" ~suffix:".output" in
          let terminal = Io.file fn Io.Out in
          let get (out : Io.output Io.t) =
            if out.kind = Terminal then (
              Io.flush out;
              terminal
            ) else
              out
          in
          (Some fn, get stdout_to, get stderr_to)
        | _ -> (None, stdout_to, stderr_to)
      in
      let run =
        (* Output.fd might create the file with Unix.openfile. We need to make
           sure to call it before doing the chdir as the path might be relative. *)
        let stdout = Io.fd stdout_to in
        let stderr = Io.fd stderr_to in
        let stdin = Io.fd stdin_from in
        let env =
          match env with
          | None -> Lazy.force default_env
          | Some env -> Dtemp.add_to_env env
        in
        fun () ->
          Spawn.spawn () ~prog:prog_str ~argv ~env ~stdout ~stderr ~stdin
      in
      let pid =
        match dir with
        | None -> run ()
        | Some dir -> Scheduler.with_chdir ~dir ~f:run
      in
      Io.release stdout_to;
      Io.release stderr_to;
      let+ exit_status =
        Stats.with_process ~program:prog_str ~args
          (Scheduler.wait_for_process pid)
      in
      Option.iter response_file ~f:Path.unlink;
      let output =
        match output_filename with
        | None -> ""
        | Some fn ->
          let s = Stdune.Io.read_file fn in
          Temp.destroy File fn;
          s
      in
      Log.command ~command_line ~output ~exit_status;
      let exit_status : Exit_status.t =
        match exit_status with
        | WEXITED n when ok_codes n -> Ok n
        | WEXITED n -> Error (Failed n)
        | WSIGNALED n -> Error (Signaled (Signal.name n))
        | WSTOPPED _ -> assert false
      in
      match (display, exit_status, output) with
      | (Quiet | Progress), Ok n, "" -> n (* Optimisation for the common case *)
      | Verbose, _, _ ->
        Exit_status.handle_verbose exit_status ~ok_codes ~id
          ~command_line:fancy_command_line ~output
      | _ ->
        Exit_status.handle_non_verbose exit_status ~prog:prog_str ~command_line
          ~output ~purpose ~display)

let run ?dir ?stdout_to ?stderr_to ?stdin_from ?env ?(purpose = Internal_job)
    fail_mode prog args =
  let+ run =
    run_internal ?dir ?stdout_to ?stderr_to ?stdin_from ~env ~purpose fail_mode
      prog args
  in
  map_result fail_mode run ~f:ignore

let run_capture_gen ?dir ?stderr_to ?stdin_from ?env ?(purpose = Internal_job)
    fail_mode prog args ~f =
  let fn = Temp.create File ~prefix:"dune" ~suffix:".output" in
  let+ run =
    run_internal ?dir ~stdout_to:(Io.file fn Io.Out) ?stderr_to ?stdin_from ~env
      ~purpose fail_mode prog args
  in
  map_result fail_mode run ~f:(fun () ->
      let x = f fn in
      Temp.destroy File fn;
      x)

let run_capture = run_capture_gen ~f:Stdune.Io.read_file

let run_capture_lines = run_capture_gen ~f:Stdune.Io.lines_of_file

let run_capture_zero_separated =
  run_capture_gen ~f:Stdune.Io.zero_strings_of_file

let run_capture_line ?dir ?stderr_to ?stdin_from ?env ?(purpose = Internal_job)
    fail_mode prog args =
  run_capture_gen ?dir ?stderr_to ?stdin_from ?env ~purpose fail_mode prog args
    ~f:(fun fn ->
      match Stdune.Io.lines_of_file fn with
      | [ x ] -> x
      | l -> (
        let cmdline =
          let prog = Path.reach_for_running ?from:dir prog in
          let prog_display = String.concat (prog :: args) ~sep:" " in
          match dir with
          | None -> prog_display
          | Some dir -> sprintf "cd %s && %s" (Path.to_string dir) prog_display
        in
        match l with
        | [] ->
          User_error.raise [ Pp.textf "Command returned nothing: %s" cmdline ]
        | _ ->
          User_error.raise
            [ Pp.textf "command returned too many lines: %s" cmdline
            ; Pp.vbox
                (Pp.concat_map l ~sep:Pp.cut ~f:(fun line ->
                     Pp.seq (Pp.verbatim "> ") (Pp.verbatim line)))
            ]))
