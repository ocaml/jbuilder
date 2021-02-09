module Evt = Event
module Utils = Utils
open Stdune
module Log = Dune_util.Log
open Utils
open Cache.Messages
open Result.O

type client =
  { fd : Unix.file_descr
  ; peer : Unix.sockaddr
  ; input : in_channel
  ; output : out_channel
  ; common_metadata : Sexp.t list
  ; cache : Cache.Local.t
  ; version : version
  }

let default_port_file () =
  let runtime_dir =
    match Xdg.runtime_dir with
    | Some p -> Path.relative (Path.of_string p) "dune-cache-daemon"
    | None ->
      (* The runtime directory is 0700 owned by the user for security reasons.
         Defaulting to a directory in the dune cache root makes sense in that
         regard, since if someone has access to this directory, it has access to
         the cache content, and having access to the socket does not make a
         difference. *)
      Path.relative (Cache.Local.default_root ()) "runtime"
  in
  Path.L.relative runtime_dir [ "dune-cache-daemon"; "port" ]

let max_port_size = 1024

let check_port_file ?(close = true) p =
  let p = Path.to_string p in
  match Result.try_with (fun () -> Unix.openfile p [ Unix.O_RDONLY ] 0o600) with
  | Result.Ok fd ->
    let f () =
      Daemonize.retry (fun () ->
          match Fcntl.lock_get fd Fcntl.Write with
          | None -> Some None
          | Some (Fcntl.Read, pid) -> Some (Some pid)
          | Some (Fcntl.Write, _) -> None)
      |> Result.map_error ~f:(fun m -> Failure m)
      >>| Option.map ~f:(fun pid ->
              let buf = Bytes.make max_port_size ' ' in
              let read = Unix.read fd buf 0 max_port_size in
              (Bytes.sub_string buf ~pos:0 ~len:read, pid, fd))
    and finally () = if close then Unix.close fd in
    Exn.protect ~f ~finally
  | Result.Error (Unix.Unix_error (Unix.ENOENT, _, _)) -> Result.Ok None
  | Result.Error e -> Result.Error e

let send_sexp output sexp =
  Csexp.to_channel output sexp;
  flush output

let send version output message =
  send_sexp output (sexp_of_message version message)

module ClientsKey = struct
  type t = Unix.file_descr

  let compare a b = Ordering.of_int (Stdlib.compare a b)

  let to_dyn _ = Dyn.Opaque
end

module Clients = Map.Make (ClientsKey)

type config =
  { exit_no_client : bool
  ; duplication_mode : Cache.Duplication_mode.t option
  }

type event =
  | Stop
  | New_client of Unix.file_descr * Unix.sockaddr
  | Client_left of Unix.file_descr

type t =
  { root : Path.t option
  ; mutable socket : Unix.file_descr option
  ; mutable clients : (client * Thread.t) Clients.t
  ; mutable endpoint : string option
  ; mutable accept_thread : Thread.t option
  ; mutable trim_thread : Thread.t option
  ; config : config
  ; events : event Evt.channel
  ; cache : Cache.Local.t
  }

exception Error of string

let make ?root ~config () : t =
  match
    Cache.Local.make ?root ~duplication_mode:Cache.Duplication_mode.Hardlink
      ~command_handler:ignore ()
  with
  | Result.Error msg -> User_error.raise [ Pp.text msg ]
  | Result.Ok cache ->
    { root
    ; socket = None
    ; clients = Clients.empty
    ; endpoint = None
    ; accept_thread = None
    ; trim_thread = None
    ; config
    ; events = Evt.new_channel ()
    ; cache
    }

let getsockname = function
  | Unix.ADDR_UNIX _ ->
    User_error.raise
      [ Pp.textf "got a Unix socket connection on our TCP socket ?" ]
  | Unix.ADDR_INET (addr, port) -> (addr, port)

let peer_name s =
  let addr, port = getsockname s in
  Printf.sprintf "%s:%d" (Unix.string_of_inet_addr addr) port

let stop daemon = Evt.sync (Evt.send daemon.events Stop)

let versions_supported_by_dune : version list = [ { major = 1; minor = 2 } ]

let endpoint m = m.endpoint

let client_handle version output = function
  | Cache.Dedup f -> send version output (Dedup f)

let client_thread (events, (client : client)) =
  try
    let handle_cmd (client : client) sexp =
      let* msg = outgoing_message_of_sexp client.version sexp in
      match msg with
      | Hint _ -> Result.Ok client
      | Promote { duplication; repository; files; key; metadata } ->
        let+ () =
          Cache.Local.promote client.cache files key
            (metadata @ client.common_metadata)
            ~repository ~duplication
        in
        client
      | SetBuildRoot root ->
        let+ cache = Cache.Local.set_build_dir client.cache root in
        { client with cache }
      | SetCommonMetadata metadata ->
        Result.ok { client with common_metadata = metadata }
      | SetRepos repositories ->
        let+ cache = Cache.Local.with_repositories client.cache repositories in
        { client with cache }
    in
    let input = client.input in
    let f () =
      Log.info [ Pp.textf "accept client: %s" (peer_name client.peer) ];
      let rec handle client =
        match Csexp.input_opt input with
        | Error msg ->
          Log.info
            [ Pp.textf "%s: parse error: %s" (peer_name client.peer) msg ]
        | Ok None -> Log.info [ Pp.textf "%s: ended" (peer_name client.peer) ]
        | Ok (Some cmd) -> (
          Log.info
            [ Pp.textf "%s: received command: %s" (peer_name client.peer)
                (Sexp.to_string cmd)
            ];
          match handle_cmd client cmd with
          | Result.Error e ->
            Log.info
              [ Pp.textf "%s: command error: %s" (peer_name client.peer) e ];
            handle client
          | Result.Ok client -> handle client)
      in
      handle client
    and finally () =
      (try Unix.shutdown client.fd Unix.SHUTDOWN_ALL with
      | Unix.Unix_error (Unix.ENOTCONN, _, _) -> ());
      Unix.close client.fd;
      Evt.sync (Evt.send events (Client_left client.fd))
    in
    try Exn.protect ~f ~finally with
    | Unix.Unix_error (Unix.EBADF, _, _) ->
      Log.info [ Pp.textf "%s: ended" (peer_name client.peer) ]
    | Sys_error msg ->
      Log.info [ Pp.textf "%s: ended: %s" (peer_name client.peer) msg ]
  with
  | Code_error.E e as exn ->
    Log.info
      [ (let open Pp.O in
        Pp.textf "%s: fatal error: " (peer_name client.peer)
        ++ Dyn.pp (Code_error.to_dyn e))
      ];
    raise exn

let run ?(port_f = ignore) ?(port = 0) daemon =
  let trim_thread ~max_overhead_size period cache =
    let rec trim () =
      Unix.sleep period;
      let () =
        match
          let overhead_size = Cache.Local.overhead_size cache in
          if overhead_size > max_overhead_size then (
            let goal = Int64.sub overhead_size max_overhead_size in
            Log.info [ Pp.textf "trimming %Li bytes" goal ];
            Some (Cache.Local.trim cache ~goal)
          ) else
            None
        with
        | Some { trimmed_bytes } ->
          Log.info [ Pp.textf "trimming freed %Li bytes" trimmed_bytes ]
        | None -> Log.info [ Pp.text "skip trimming" ]
      in
      trim ()
    in
    trim ()
  in
  let rec accept_thread sock =
    let rec accept () =
      try Unix.accept sock with
      | Unix.Unix_error (Unix.EINTR, _, _) -> (accept [@tailcall]) ()
    in
    let fd, peer = accept () in
    (try Evt.sync (Evt.send daemon.events (New_client (fd, peer))) with
    | Unix.Unix_error (Unix.EBADF, _, _) -> ());
    (accept_thread [@tailcall]) sock
  in
  let f () =
    let+ trim_period =
      Option.value
        ~default:(Result.Ok (10 * 60))
        (Option.map ~f:int_of_string
           (Env.get Env.initial "DUNE_CACHE_TRIM_PERIOD"))
    and+ max_overhead_size =
      Option.value ~default:(Result.Ok 10_000_000_000L)
        (Option.map ~f:int64_of_string
           (* CR-someday amokhov: the term "size" is ambiguous, it would be
              better to switch to a more precise one, e.g. "max overhead size". *)
           (Env.get Env.initial "DUNE_CACHE_TRIM_SIZE"))
    in
    let sock = Unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in
    daemon.socket <- Some sock;
    Unix.bind sock (Unix.ADDR_INET (Unix.inet_addr_of_string "127.0.0.1", port));
    let addr, port = getsockname (Unix.getsockname sock) in
    let endpoint =
      Printf.sprintf "%s:%i" (Unix.string_of_inet_addr addr) port
    in
    daemon.endpoint <- Some endpoint;
    port_f endpoint;
    Unix.listen sock 1024;
    daemon.accept_thread <- Some (Thread.create accept_thread sock);
    daemon.trim_thread <-
      Some
        (Thread.create
           (trim_thread ~max_overhead_size trim_period)
           daemon.cache);
    let rec handle () =
      let stop () =
        match daemon.socket with
        | Some fd ->
          daemon.socket <- None;
          let clean f = ignore (Clients.iter ~f daemon.clients) in
          clean (fun (client, _) -> Unix.shutdown client.fd Unix.SHUTDOWN_ALL);
          clean (fun (_, tid) -> Thread.join tid);
          clean (fun (client, _) -> Unix.close client.fd);
          Unix.close fd
        | _ -> Log.info [ Pp.text "stop" ]
      in
      (match Evt.sync (Evt.receive daemon.events) with
      | Stop -> stop ()
      | New_client (fd, peer) -> (
        let output = Unix.out_channel_of_descr fd
        and input = Unix.in_channel_of_descr fd in
        match
          let* version =
            negotiate_version ~versions_supported_by_dune fd input output
          in
          let client =
            { fd
            ; peer
            ; input
            ; output
            ; version
            ; common_metadata = []
            ; cache =
                (match
                   Cache.Local.make ?root:daemon.root
                     ~duplication_mode:Cache.Duplication_mode.Hardlink
                     ~command_handler:(client_handle version output)
                     ()
                 with
                | Result.Ok m -> m
                | Result.Error e -> User_error.raise [ Pp.textf "%s" e ])
            }
          in
          let tid = Thread.create client_thread (daemon.events, client) in
          let+ clients =
            Result.map_error
              ~f:(fun _ -> "duplicate socket")
              (Clients.add daemon.clients client.fd (client, tid))
          in
          daemon.clients <- clients
        with
        | Result.Ok () -> ()
        | Result.Error msg -> Log.info [ Pp.textf "reject client: %s" msg ])
      | Client_left fd ->
        daemon.clients <- Clients.remove daemon.clients fd;
        if daemon.config.exit_no_client && Clients.is_empty daemon.clients then
          stop ());
      if Option.is_some daemon.socket then (handle [@tailcall]) ()
    in
    handle ()
  in
  match f () with
  | Result.Ok () -> ()
  | Result.Error msg -> User_error.raise [ Pp.text msg ]
  | exception Unix.Unix_error (errno, f, _) ->
    User_error.raise
      [ Pp.textf "unable to %s: %s\n" f (Unix.error_message errno) ]

let daemon ~root ~config started =
  Path.mkdir_p root;
  let log_file = Path.relative root "log" in
  Log.init ~file:(This log_file) ();
  let daemon = make ~root ~config () in
  (* Event blocks signals when waiting. Use a separate thread to catch signals. *)
  let signal_handler s =
    Log.info [ Pp.textf "caught signal %i, exiting" s ];
    ignore (Thread.create stop daemon)
  and signals = [ Sys.sigint; Sys.sigterm ] in
  let rec signals_handler () =
    signal_handler (Thread.wait_signal signals);
    signals_handler ()
  in
  ignore (Thread.sigmask Unix.SIG_BLOCK signals);
  ignore (Thread.create signals_handler ());
  try run ~port_f:started daemon with
  | Error s ->
    Printf.fprintf stderr "%s: fatal error: %s\n%!" Sys.argv.(0) s;
    exit 1
