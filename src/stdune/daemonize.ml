type status =
  | Started of
      { daemon_info : string
      ; pid : Pid.t
      }
  | Already_running of
      { daemon_info : string
      ; pid : Pid.t
      }
  | Finished

let retry ?message ?(count = 100) f =
  let rec loop = function
    | x when x >= count ->
      Result.Error
        (Printf.sprintf "too many retries (%i)" x
        ^
        match message with
        | None -> ""
        | Some msg -> ": " ^ msg)
    | x -> (
      match f () with
      | Some v -> Result.Ok v
      | None ->
        Unix.sleepf 0.1;
        loop (x + 1))
  in
  loop 0

let make_beacon path =
  Option.iter ~f:Path.mkdir_p (Path.parent path);
  let p = Path.to_string path in
  let fd = Unix.openfile p [ Unix.O_RDWR; Unix.O_CREAT ] 0o600 in
  if Fcntl.lock_try fd Fcntl.Write then
    Result.Ok fd
  else
    Result.Error "already running"

let seal_beacon path fd contents =
  let p = Path.to_string path
  and length = String.length contents in
  if Unix.write fd (Bytes.of_string contents) 0 length <> length then (
    Unix.close fd;
    Result.Error
      (Printf.sprintf "couldn't write whole endpoint to port file \"%s\"" p)
  ) else (
    Fcntl.lock fd Fcntl.Read;
    Result.Ok fd
  )

let check_beacon ?(close = true) p =
  match Result.try_with (fun () -> Unix.openfile p [ Unix.O_RDONLY ] 0o600) with
  | Result.Ok fd ->
    let f () =
      let open Result.O in
      retry (fun () ->
          match Fcntl.lock_get fd Fcntl.Write with
          | None -> Some None
          | Some (Fcntl.Read, pid) -> Some (Some pid)
          | Some (Fcntl.Write, _) -> None)
      >>| Option.map ~f:(fun pid ->
              (Io.read_all (Unix.in_channel_of_descr fd), pid, fd))
    and finally () = if close then Unix.close fd in
    Exn.protect ~f ~finally
  | Result.Error (Unix.Unix_error (Unix.ENOENT, _, _)) -> Result.Ok None
  | Result.Error (Unix.Unix_error (c, _, _)) ->
    Result.Error
      (Printf.sprintf "unable to open %s: %s" p (Unix.error_message c))
  | Result.Error _ -> Result.Error (Printf.sprintf "unable to open %s" p)

let daemonize ?workdir ?(foreground = false) beacon
    (f : (daemon_info:string -> unit) -> unit) =
  let f fd =
    let f () =
      f (fun ~daemon_info -> ignore (seal_beacon beacon fd daemon_info))
    and finally () = Unix.truncate (Path.to_string beacon) 0 in
    Exn.protect ~f ~finally
  in
  let path = Path.to_string beacon in
  let path =
    match workdir with
    | Some workdir when Filename.is_relative path ->
      Filename.concat (Path.to_string workdir) path
    | _ -> path
  in
  let open Result.O in
  check_beacon path >>= function
  | Some (daemon_info, pid, _) ->
    Result.Ok (Already_running { daemon_info; pid = Pid.of_int pid })
  | None ->
    if foreground then (
      let+ fd = make_beacon beacon in
      f fd;
      Finished
    ) else if Unix.fork () = 0 then (
      ignore (Unix.setsid ());
      Sys.set_signal Sys.sighup Sys.Signal_ignore;
      Sys.set_signal Sys.sigpipe Sys.Signal_ignore;
      if Unix.fork () = 0 then (
        Option.iter
          ~f:(fun p ->
            Path.mkdir_p p;
            Unix.chdir (Path.to_string p))
          workdir;
        let null = open_in "/dev/null"
        and out = open_out "stdout"
        and err = open_out "stderr" in
        Unix.dup2
          (Unix.descr_of_in_channel null)
          (Unix.descr_of_in_channel stdin);
        Unix.dup2
          (Unix.descr_of_out_channel out)
          (Unix.descr_of_out_channel stdout);
        Unix.dup2
          (Unix.descr_of_out_channel err)
          (Unix.descr_of_out_channel stderr);
        close_in null;
        close_out out;
        close_out err;
        ignore (Unix.umask 0);
        ignore
          (let+ fd = make_beacon beacon in
           f fd)
      );
      exit 0
    ) else
      let open Result.O in
      let* fd =
        retry
          ~message:
            (Printf.sprintf "waiting for beacon file \"%s\" to be created" path)
          (fun () ->
            try Some (Unix.openfile path [ Unix.O_RDONLY ] 0o600) with
            | Unix.Unix_error (Unix.ENOENT, _, _) -> None)
      in
      let+ daemon_info, pid =
        retry
          ~message:
            (Printf.sprintf "waiting for beacon file \"%s\" to be locked" path)
          (fun () ->
            match Fcntl.lock_get fd Fcntl.Write with
            | Some (Fcntl.Read, pid) ->
              Some (Io.read_all (Unix.in_channel_of_descr fd), pid)
            | _ -> None)
      in
      Started { daemon_info; pid = Pid.of_int pid }

let stop beacon =
  let open Result.O in
  check_beacon ~close:false (Path.to_string beacon) >>= function
  | None -> Result.Error "not running"
  | Some (_, pid, fd) -> (
    let kill signal =
      Unix.kill pid signal;
      retry ~message:(Printf.sprintf "waiting for daemon to stop (PID %i)" pid)
        (fun () -> Option.some_if (Fcntl.lock_get fd Fcntl.Write = None) ())
    in
    match kill Sys.sigterm with
    | Error _ ->
      (* Unfortunately the logger may not be set. Print on stderr directly? *)
      (* Log.info "unable to terminate daemon with SIGTERM, using SIGKILL"; *)
      kill Sys.sigkill
    | ok -> ok)
