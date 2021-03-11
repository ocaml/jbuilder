open Stdune

let with_mutex t ~f =
  Mutex.lock t;
  let res = f () in
  Mutex.unlock t;
  res

type state =
  | Running
  | Stopped
  | Finished

type t =
  { work : (unit -> unit) Queue.t
  ; mutable state : state
  ; mutex : Mutex.t
  ; work_available : Condition.t
  }

let is_running t =
  match t.state with
  | Running -> true
  | Stopped
  | Finished ->
    false

let run t =
  let rec loop () =
    match t.state with
    | Stopped -> (
      match Queue.pop t.work with
      | None -> t.state <- Finished
      | Some job -> do_work job)
    | Finished -> ()
    | Running -> (
      match Queue.pop t.work with
      | Some job -> do_work job
      | None ->
        while Queue.is_empty t.work && is_running t do
          Condition.wait t.work_available t.mutex
        done;
        loop ())
  and do_work job =
    Mutex.unlock t.mutex;
    job ();
    Mutex.lock t.mutex;
    loop ()
  in
  Mutex.lock t.mutex;
  loop ();
  Mutex.unlock t.mutex

let create ~spawn_thread =
  let t =
    { work = Queue.create ()
    ; state = Finished
    ; mutex = Mutex.create ()
    ; work_available = Condition.create ()
    }
  in
  t.state <- Running;
  spawn_thread (fun () -> run t);
  t

let add_work t ~f =
  with_mutex t.mutex ~f:(fun () ->
      if is_running t then (
        Queue.push t.work f;
        Condition.signal t.work_available;
        Ok ()
      ) else
        Error `Stopped)

let stop t =
  with_mutex t.mutex ~f:(fun () ->
      match t.state with
      | Running ->
        t.state <- Stopped;
        Condition.signal t.work_available
      | Stopped
      | Finished ->
        ())
