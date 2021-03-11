open Stdune
open Fiber.O

module Session_id = Id.Make ()

module Scheduler = Task_queue.Scheduler

let debug = Option.is_some (Env.get Env.initial "DUNE_RPC_DEBUG")

module Session = struct
  module Id = Session_id

  type kind =
    | Socket
    | Channel

  type t =
    { out_channel : out_channel
    ; in_channel : in_channel
    ; id : Id.t
    ; writer : Task_queue.t
    ; reader : Task_queue.t
    ; scheduler : Scheduler.t
    ; kind : kind
    }

  let create_full kind in_channel out_channel scheduler =
    if debug then Format.eprintf ">> NEW SESSION@.";
    let reader_ref = ref None in
    let t =
      let id = Id.gen () in
      { in_channel
      ; out_channel
      ; id
      ; reader = Task_queue.create scheduler
      ; writer = Task_queue.create scheduler
      ; scheduler
      ; kind
      }
    in
    reader_ref := Some t.reader;
    t

  let create in_channel out_channel scheduler =
    create_full Channel in_channel out_channel scheduler

  let string_of_packet = function
    | None -> "EOF"
    | Some csexp -> Csexp.to_string csexp

  let read t =
    let rec read () =
      try Csexp.input_opt t.in_channel with
      | Unix.Unix_error (EBADF, _, _) -> Ok None
      | Sys_error _ -> Ok None
      | Sys_blocked_io -> read ()
      | e -> reraise e
    in
    let+ res = Task_queue.task t.reader ~f:read in
    let res =
      match res with
      | Error exn ->
        Task_queue.stop t.reader;
        raise exn
      | Ok res -> (
        match res with
        | Ok (Some _ as s) -> s
        | Error _
        | Ok None ->
          Task_queue.stop t.reader;
          None)
    in
    if debug then Format.eprintf "<< %s@." (string_of_packet res);
    res

  let write t sexp =
    if debug then Format.eprintf ">> %s@." (string_of_packet sexp);
    Task_queue.task_exn t.writer
      ~f:
        (match sexp with
        | Some sexp ->
          fun () ->
            Csexp.to_channel t.out_channel sexp;
            flush t.out_channel
        | None -> (
          match t.kind with
          | Channel -> fun () -> close_out_noerr t.out_channel
          | Socket ->
            fun () ->
              let fd = Unix.descr_of_out_channel t.out_channel in
              Unix.shutdown fd Unix.SHUTDOWN_SEND))
end

let close_fd_no_error fd =
  try Unix.close fd with
  | _ -> ()

module Server = struct
  module Transport = struct
    type t =
      { fd : Unix.file_descr
      ; sockaddr : Unix.sockaddr
      ; r_interrupt_accept : Unix.file_descr
      ; w_interrupt_accept : Unix.file_descr
      ; buf : Bytes.t
      }

    let create sockaddr ~backlog =
      let fd =
        Unix.socket (Unix.domain_of_sockaddr sockaddr) Unix.SOCK_STREAM 0
      in
      Unix.setsockopt fd Unix.SO_REUSEADDR true;
      Unix.set_nonblock fd;
      (match sockaddr with
      | ADDR_UNIX p ->
        let p = Path.of_string p in
        Path.unlink_no_err p;
        Path.mkdir_p (Path.parent_exn p)
      | _ -> ());
      Unix.bind fd sockaddr;
      Unix.listen fd backlog;
      let r_interrupt_accept, w_interrupt_accept = Unix.pipe () in
      Unix.set_nonblock r_interrupt_accept;
      let buf = Bytes.make 1 '0' in
      { fd; sockaddr; r_interrupt_accept; w_interrupt_accept; buf }

    let rec accept t =
      match Unix.select [ t.r_interrupt_accept; t.fd ] [] [] (-1.0) with
      | r, [], [] ->
        let inter, accept =
          List.fold_left r ~init:(false, false) ~f:(fun (i, a) fd ->
              if fd = t.fd then
                (i, true)
              else if fd = t.r_interrupt_accept then
                (true, a)
              else
                assert false)
        in
        if inter then
          None
        else if accept then
          let fd, _ = Unix.accept t.fd in
          Some fd
        else
          assert false
      | _, _, _ -> assert false
      | exception Unix.Unix_error (Unix.EAGAIN, _, _) -> accept t
      | exception Unix.Unix_error (Unix.EBADF, _, _) -> None

    let stop t =
      let _ = Unix.write t.w_interrupt_accept t.buf 0 1 in
      close_fd_no_error t.fd;
      match t.sockaddr with
      | ADDR_UNIX p -> Fpath.unlink_no_err p
      | _ -> ()
  end

  type t =
    { mutable transport : Transport.t option
    ; backlog : int
    ; scheduler : Scheduler.t
    ; sockaddr : Unix.sockaddr
    }

  let create sockaddr ~backlog scheduler =
    { sockaddr; backlog; scheduler; transport = None }

  let serve (t : t) =
    let async = Task_queue.create t.scheduler in
    let+ transport =
      Task_queue.task_exn async ~f:(fun () ->
          Transport.create t.sockaddr ~backlog:t.backlog)
    in
    t.transport <- Some transport;
    let accept () =
      Task_queue.task async ~f:(fun () ->
          Transport.accept transport
          |> Option.map ~f:(fun client ->
                 let in_ = Unix.in_channel_of_descr client in
                 let out = Unix.out_channel_of_descr client in
                 (in_, out)))
    in
    let loop () =
      let+ accept = accept () in
      match accept with
      | Error _
      | Ok None ->
        None
      | Ok (Some (in_, out)) ->
        let session = Session.create_full Socket in_ out t.scheduler in
        Some session
    in
    Fiber.Stream.In.create loop

  let stop t =
    match t.transport with
    | None -> Code_error.raise "server not running" []
    | Some t -> Transport.stop t

  let listening_address t =
    match t.transport with
    | None -> Code_error.raise "server not running" []
    | Some t -> Unix.getsockname t.fd
end

module Client = struct
  module Transport = struct
    type t =
      { fd : Unix.file_descr
      ; sockaddr : Unix.sockaddr
      }

    let close t = close_fd_no_error t.fd

    let create sockaddr =
      let fd =
        Unix.socket (Unix.domain_of_sockaddr sockaddr) Unix.SOCK_STREAM 0
      in
      { sockaddr; fd }

    let connect t =
      let () = Unix.connect t.fd t.sockaddr in
      t.fd
  end

  type t =
    { mutable transport : Transport.t option
    ; async : Task_queue.t
    ; scheduler : Scheduler.t
    ; sockaddr : Unix.sockaddr
    }

  let create sockaddr scheduler =
    let async = Task_queue.create scheduler in
    { sockaddr; scheduler; async; transport = None }

  let connect t =
    Task_queue.task_exn t.async ~f:(fun () ->
        let transport = Transport.create t.sockaddr in
        t.transport <- Some transport;
        let client = Transport.connect transport in
        let out = Unix.out_channel_of_descr client in
        let in_ = Unix.in_channel_of_descr client in
        Session.create in_ out t.scheduler)

  let stop t = Option.iter t.transport ~f:Transport.close
end
