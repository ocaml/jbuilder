open Stdune
open Dune_util
open Result.O
open Cache_intf

type t =
  { socket : out_channel
  ; fd : Unix.file_descr
  ; input : in_channel
  ; cache : Local.t
  ; thread : Thread.t
  ; finally : (unit -> unit) option
  ; version : Messages.version
  }

let versions_supported_by_dune : Messages.version list =
  [ { major = 1; minor = 2 } ]

let err msg = User_error.E (User_error.make [ Pp.text msg ])

let errf msg = User_error.E (User_error.make msg)

let read version input =
  let* sexp = Csexp.input input in
  let+ (Dedup v) = Messages.incoming_message_of_sexp version sexp in
  Dedup v

let make ?finally ?duplication_mode ~command_handler () =
  (* This is a bit ugly as it is global, but flushing a closed socket will nuke
     the program if we don't. *)
  let () = Sys.set_signal Sys.sigpipe Sys.Signal_ignore in
  let* cache =
    Result.map_error ~f:err
      (Local.make ?duplication_mode ~command_handler:ignore ())
  in
  let* port =
    let cmd =
      Format.sprintf "%s cache start --display progress --exit-no-client"
        Sys.executable_name
    and f stdout =
      match Io.input_lines stdout with
      | [] -> Result.Error (err "empty output starting cache")
      | [ line ] -> Result.Ok line
      | _ -> Result.Error (err "unrecognized output starting cache")
    and finally stdout = ignore (Unix.close_process_in stdout) (* FIXME *) in
    Exn.protectx (Unix.open_process_in cmd) ~finally ~f
  in
  let* addr, port =
    match String.split_on_char ~sep:':' port with
    | [ addr; port ] -> (
      match Int.of_string port with
      | Some i -> (
        try Result.Ok (Unix.inet_addr_of_string addr, i) with
        | Failure _ ->
          Result.Error (errf [ Pp.textf "invalid address: %s" addr ]))
      | None -> Result.Error (errf [ Pp.textf "invalid port: %s" port ]))
    | _ -> Result.Error (errf [ Pp.textf "invalid endpoint: %s" port ])
  in
  let fd = Unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in
  let* _ =
    Result.try_with (fun () -> Unix.connect fd (Unix.ADDR_INET (addr, port)))
  in
  let socket = Unix.out_channel_of_descr fd in
  let input = Unix.in_channel_of_descr fd in
  let+ version =
    Result.map_error ~f:err
      (Messages.negotiate_version ~versions_supported_by_dune fd input socket)
  in
  Log.info
    [ Pp.textf "negotiated version: %s" (Messages.string_of_version version) ];
  let rec thread input =
    match
      let+ command = read version input in
      Log.info
        [ (let open Pp.O in
          Pp.text "dune-cache command: " ++ Dyn.pp (command_to_dyn command))
        ];
      command_handler command
    with
    | Result.Error e ->
      Log.info [ Pp.textf "dune-cache read error: %s" e ];
      Option.iter ~f:(fun f -> f ()) finally
    | Result.Ok () -> (thread [@tailcall]) input
  in
  let thread = Thread.create thread input in
  { socket; fd; input; cache; thread; finally; version }

let send client message =
  try Result.Ok (Messages.send client.version client.socket message) with
  | Sys_error (* "Broken_pipe" *) _ ->
    Result.Error "lost connection to cache daemon"

let with_repositories client repositories =
  let+ () = send client (SetRepos repositories) in
  client

let promote (client : t) files key metadata ~repository ~duplication =
  let duplication =
    Some
      (Option.value ~default:(Local.duplication_mode client.cache) duplication)
  in
  send client
    (Messages.Promote { key; files; metadata; repository; duplication })

let set_build_dir client path =
  let+ () = send client (Messages.SetBuildRoot path) in
  client

let search client key = Local.search client.cache key

let retrieve client file = Local.retrieve client.cache file

let deduplicate client file = Local.deduplicate client.cache file

let teardown client =
  (try Unix.shutdown client.fd Unix.SHUTDOWN_SEND with
  | Unix.Unix_error (Unix.ENOTCONN, _, _) -> ());
  Thread.join client.thread;
  Local.teardown client.cache

let hint client keys =
  if Messages.hint_supported client.version then
    send client (Messages.Hint keys)
  else
    User_warning.emit
      ~hints:
        [ Pp.textf "update sietch to version %s at least"
            (Messages.string_of_version Messages.hint_min_version)
        ]
      [ Pp.textf "not hinting the cache as sietch version is too old: %s"
          (Messages.string_of_version client.version)
      ]
    |> Result.ok
