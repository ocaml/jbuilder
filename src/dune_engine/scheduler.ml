open! Stdune
open Import
open Fiber.O

module Config = struct
  include Config

  module Display = struct
    type verbosity =
      | Quiet
      | Short
      | Verbose

    type t =
      { status_line : bool
      ; verbosity : verbosity
      }

    (* Even though [status_line] is true by default in most of these, the status
       line is actually not shown if the output is redirected to a file or a
       pipe. *)
    let all =
      [ ("progress", { verbosity = Quiet; status_line = true })
      ; ("verbose", { verbosity = Verbose; status_line = true })
      ; ("short", { verbosity = Short; status_line = true })
      ; ("quiet", { verbosity = Quiet; status_line = false })
      ]

    let verbosity_to_dyn : verbosity -> Dyn.t = function
      | Quiet -> Variant ("Quiet", [])
      | Short -> Variant ("Short", [])
      | Verbose -> Variant ("Verbose", [])

    let to_dyn { status_line; verbosity } : Dyn.t =
      Record
        [ ("status_line", Dyn.Bool status_line)
        ; ("verbosity", verbosity_to_dyn verbosity)
        ]

    let console_backend t =
      match t.status_line with
      | false -> Console.Backend.dumb
      | true -> Console.Backend.progress
  end

  type t =
    { concurrency : int
    ; display : Display.t
    ; rpc : Dune_rpc.Where.t option
    ; stats : Dune_stats.t option
    }

  let add_to_env t env =
    match t.rpc with
    | None -> env
    | Some where -> Dune_rpc.Where.add_to_env where env
end

type job =
  { pid : Pid.t
  ; ivar : Proc.Process_info.t Fiber.Ivar.t
  }

module Signal = struct
  type t =
    | Int
    | Quit
    | Term

  let compare : t -> t -> Ordering.t = Poly.compare

  include Comparable.Make (struct
    type nonrec t = t

    let compare = compare

    let to_dyn _ = Dyn.opaque
  end)

  let all = [ Int; Quit; Term ]

  let to_int = function
    | Int -> Sys.sigint
    | Quit -> Sys.sigquit
    | Term -> Sys.sigterm

  let of_int =
    List.map all ~f:(fun t -> (to_int t, t))
    |> Int.Map.of_list_reduce ~f:(fun _ t -> t)
    |> Int.Map.find

  let name t = Signal.name (to_int t)
end

module Thread = struct
  include Thread

  let block_signals =
    lazy
      (let signos = List.map Signal.all ~f:Signal.to_int in
       ignore (Unix.sigprocmask SIG_BLOCK signos : int list))

  let create =
    if Sys.win32 then
      Thread.create
    else
      (* On unix, we make sure to block signals globally before starting a
         thread so that only the signal watcher thread can receive signals. *)
      fun f x ->
    Lazy.force block_signals;
    Thread.create f x

  let spawn f =
    let (_ : Thread.t) = create f () in
    ()
end

(** The event queue *)
module Event : sig
  type t =
    | File_system_changed of Fs_memo.Event.t Nonempty_list.t
    | File_system_watcher_terminated
    | Job_completed of job * Proc.Process_info.t
    | Signal of Signal.t
    | Worker_task of Fiber.fill

  module Queue : sig
    type t

    type event

    val create : Dune_stats.t option -> t

    (** Return the next event. File changes event are always flattened and
        returned first. *)
    val next : t -> event

    (** Ignore the ne next file change event about this file. *)
    val ignore_next_file_change_event : t -> Path.t -> unit

    (** Pending worker tasks *)
    val pending_worker_tasks : t -> int

    (** Register the fact that a job was started. *)
    val register_job_started : t -> unit

    (** Number of jobs for which the status hasn't been reported yet .*)
    val pending_jobs : t -> int

    val send_worker_task_completed : t -> Fiber.fill -> unit

    val register_worker_task_started : t -> unit

    (** Send an event to the main thread. *)
    val send_file_watcher_events : t -> Dune_file_watcher.Event.t list -> unit

    val send_job_completed : t -> job -> Proc.Process_info.t -> unit

    val send_signal : t -> Signal.t -> unit
  end
  with type event := t
end = struct
  type t =
    | File_system_changed of Fs_memo.Event.t Nonempty_list.t
    | File_system_watcher_terminated
    | Job_completed of job * Proc.Process_info.t
    | Signal of Signal.t
    | Worker_task of Fiber.fill

  module Queue = struct
    type t =
      { jobs_completed : (job * Proc.Process_info.t) Queue.t
      ; mutable file_watcher_events : Dune_file_watcher.Event.t list
      ; mutable signals : Signal.Set.t
      ; mutex : Mutex.t
      ; cond : Condition.t
            (* CR-soon amokhov: The way we handle "ignored files" using this
               mutable table is fragile and also wrong. We use [ignored_files]
               for the [(mode promote)] feature: if a file is promoted, we call
               [ignore_next_file_change_event] so that the upcoming file-change
               event does not invalidate the current build. However, instead of
               ignoring the events, we should merely postpone them and restart
               the build to take the promoted files into account if need be. *)
      ; ignored_files : (string, unit) Table.t
      ; mutable pending_jobs : int
      ; mutable pending_worker_tasks : int
      ; worker_tasks_completed : Fiber.fill Queue.t
      ; stats : Dune_stats.t option
      }

    let create stats =
      let jobs_completed = Queue.create () in
      let worker_tasks_completed = Queue.create () in
      let file_watcher_events = [] in
      let signals = Signal.Set.empty in
      let mutex = Mutex.create () in
      let cond = Condition.create () in
      let ignored_files = Table.create (module String) 64 in
      let pending_jobs = 0 in
      let pending_worker_tasks = 0 in
      { jobs_completed
      ; file_watcher_events
      ; signals
      ; mutex
      ; cond
      ; ignored_files
      ; pending_jobs
      ; worker_tasks_completed
      ; pending_worker_tasks
      ; stats
      }

    let register_job_started q = q.pending_jobs <- q.pending_jobs + 1

    let register_worker_task_started q =
      q.pending_worker_tasks <- q.pending_worker_tasks + 1

    let ignore_next_file_change_event q path =
      assert (Path.is_in_source_tree path);
      Table.set q.ignored_files (Path.to_absolute_filename path) ()

    let available q =
      not
        (List.is_empty q.file_watcher_events
        && Queue.is_empty q.jobs_completed
        && Signal.Set.is_empty q.signals
        && Queue.is_empty q.worker_tasks_completed)

    let next q =
      Option.iter q.stats ~f:Dune_stats.record_gc_and_fd;
      Mutex.lock q.mutex;
      let rec loop () =
        while not (available q) do
          Condition.wait q.cond q.mutex
        done;
        match Signal.Set.choose q.signals with
        | Some signal ->
          q.signals <- Signal.Set.remove q.signals signal;
          Signal signal
        | None -> (
          match q.file_watcher_events with
          | [] -> (
            match Queue.pop q.jobs_completed with
            | None ->
              q.pending_worker_tasks <- q.pending_worker_tasks - 1;
              Worker_task (Queue.pop_exn q.worker_tasks_completed)
            | Some (job, proc_info) ->
              q.pending_jobs <- q.pending_jobs - 1;
              assert (q.pending_jobs >= 0);
              Job_completed (job, proc_info))
          | events -> (
            q.file_watcher_events <- [];
            let terminated = ref false in
            let events =
              List.filter_map events ~f:(function
                | Watcher_terminated ->
                  terminated := true;
                  None
                | File_changed path ->
                  let abs_path = Path.to_absolute_filename path in
                  if Table.mem q.ignored_files abs_path then (
                    (* only use ignored record once *)
                    Table.remove q.ignored_files abs_path;
                    None
                  ) else
                    (* CR-soon amokhov: Generate more precise events. *)
                    Some (Fs_memo.Event.create ~kind:Unknown ~path))
            in
            match !terminated with
            | true -> File_system_watcher_terminated
            | false -> (
              match Nonempty_list.of_list events with
              | None -> loop ()
              | Some events -> File_system_changed events)))
      in
      let ev = loop () in
      Mutex.unlock q.mutex;
      ev

    let send_worker_task_completed q event =
      Mutex.lock q.mutex;
      let avail = available q in
      Queue.push q.worker_tasks_completed event;
      if not avail then Condition.signal q.cond;
      Mutex.unlock q.mutex

    let send_file_watcher_events q files =
      Mutex.lock q.mutex;
      let avail = available q in
      q.file_watcher_events <- List.rev_append files q.file_watcher_events;
      if not avail then Condition.signal q.cond;
      Mutex.unlock q.mutex

    let send_job_completed q job proc_info =
      Mutex.lock q.mutex;
      let avail = available q in
      Queue.push q.jobs_completed (job, proc_info);
      if not avail then Condition.signal q.cond;
      Mutex.unlock q.mutex

    let send_signal q signal =
      Mutex.lock q.mutex;
      let avail = available q in
      q.signals <- Signal.Set.add q.signals signal;
      if not avail then Condition.signal q.cond;
      Mutex.unlock q.mutex

    let pending_jobs q = q.pending_jobs

    let pending_worker_tasks q = q.pending_worker_tasks
  end
end

module Process_watcher : sig
  (** Initialize the process watcher thread. *)
  type t

  val init : Event.Queue.t -> t

  (** Register a new running job. *)
  val register_job : t -> job -> unit

  (** Send the following signal to all running processes. *)
  val killall : t -> int -> unit
end = struct
  type process_state =
    | Running of job
    | Zombie of Proc.Process_info.t

  (* This mutable table is safe: it does not interact with the state we track in
     the build system. *)
  type t =
    { mutex : Mutex.t
    ; something_is_running : Condition.t
    ; table : (Pid.t, process_state) Table.t
    ; events : Event.Queue.t
    ; mutable running_count : int
    }

  module Process_table : sig
    val add : t -> job -> unit

    val remove : t -> Proc.Process_info.t -> unit

    val running_count : t -> int

    val iter : t -> f:(job -> unit) -> unit
  end = struct
    let add t job =
      match Table.find t.table job.pid with
      | None ->
        Table.set t.table job.pid (Running job);
        t.running_count <- t.running_count + 1;
        if t.running_count = 1 then Condition.signal t.something_is_running
      | Some (Zombie proc_info) ->
        Table.remove t.table job.pid;
        Event.Queue.send_job_completed t.events job proc_info
      | Some (Running _) -> assert false

    let remove t (proc_info : Proc.Process_info.t) =
      match Table.find t.table proc_info.pid with
      | None -> Table.set t.table proc_info.pid (Zombie proc_info)
      | Some (Running job) ->
        t.running_count <- t.running_count - 1;
        Table.remove t.table proc_info.pid;
        Event.Queue.send_job_completed t.events job proc_info
      | Some (Zombie _) -> assert false

    let iter t ~f =
      Table.iter t.table ~f:(fun data ->
          match data with
          | Running job -> f job
          | Zombie _ -> ())

    let running_count t = t.running_count
  end

  let register_job t job =
    Event.Queue.register_job_started t.events;
    Mutex.lock t.mutex;
    Process_table.add t job;
    Mutex.unlock t.mutex

  let killall t signal =
    Mutex.lock t.mutex;
    Process_table.iter t ~f:(fun job ->
        try Unix.kill (Pid.to_int job.pid) signal with
        | Unix.Unix_error _ -> ());
    Mutex.unlock t.mutex

  exception Finished of Proc.Process_info.t

  let wait_nonblocking_win32 t =
    try
      Process_table.iter t ~f:(fun job ->
          let pid, status = Unix.waitpid [ WNOHANG ] (Pid.to_int job.pid) in
          if pid <> 0 then
            let now = Unix.gettimeofday () in
            let info : Proc.Process_info.t =
              { pid = Pid.of_int pid
              ; status
              ; end_time = now
              ; resource_usage = None
              }
            in
            raise_notrace (Finished info));
      false
    with
    | Finished proc_info ->
      (* We need to do the [Unix.waitpid] and remove the process while holding
         the lock, otherwise the pid might be reused in between. *)
      Process_table.remove t proc_info;
      true

  let wait_win32 t =
    while not (wait_nonblocking_win32 t) do
      Mutex.unlock t.mutex;
      Thread.delay 0.001;
      Mutex.lock t.mutex
    done

  let wait_unix t =
    Mutex.unlock t.mutex;
    let proc_info = Proc.wait [] in
    Mutex.lock t.mutex;
    Process_table.remove t proc_info

  let wait =
    if Sys.win32 then
      wait_win32
    else
      wait_unix

  let run t =
    Mutex.lock t.mutex;
    while true do
      while Process_table.running_count t = 0 do
        Condition.wait t.something_is_running t.mutex
      done;
      wait t
    done

  let init events =
    let t =
      { mutex = Mutex.create ()
      ; something_is_running = Condition.create ()
      ; table = Table.create (module Pid) 128
      ; events
      ; running_count = 0
      }
    in
    ignore (Thread.create run t : Thread.t);
    t
end

module Signal_watcher : sig
  val init : Event.Queue.t -> unit
end = struct
  let signos = List.map Signal.all ~f:Signal.to_int

  let warning =
    {|

**************************************************************
* Press Control+C again quickly to perform an emergency exit *
**************************************************************

|}

  external sys_exit : int -> _ = "caml_sys_exit"

  let signal_waiter () =
    if Sys.win32 then (
      let r, w = Unix.pipe () in
      let buf = Bytes.create 1 in
      Sys.set_signal Sys.sigint
        (Signal_handle (fun _ -> assert (Unix.write w buf 0 1 = 1)));
      Staged.stage (fun () ->
          assert (Unix.read r buf 0 1 = 1);
          Signal.Int)
    ) else
      Staged.stage (fun () ->
          Thread.wait_signal signos |> Signal.of_int |> Option.value_exn)

  let run q =
    let last_exit_signals = Queue.create () in
    let wait_signal = Staged.unstage (signal_waiter ()) in
    while true do
      let signal = wait_signal () in
      Event.Queue.send_signal q signal;
      match signal with
      | Int
      | Quit
      | Term ->
        let now = Unix.gettimeofday () in
        Queue.push last_exit_signals now;
        (* Discard old signals *)
        while
          Queue.length last_exit_signals >= 0
          && now -. Queue.peek_exn last_exit_signals > 1.
        do
          ignore (Queue.pop_exn last_exit_signals : float)
        done;
        let n = Queue.length last_exit_signals in
        if n = 2 then prerr_endline warning;
        if n = 3 then sys_exit 1
    done

  let init q = ignore (Thread.create run q : Thread.t)
end

type waiting_for_file_changes =
  | Shutdown_requested
  | File_system_changed

type status =
  (* Waiting for file changes to start a new a build *)
  | Waiting_for_file_changes of waiting_for_file_changes Fiber.Ivar.t
  (* Running a build *)
  | Building
  (* Cancellation requested. Build jobs are immediately rejected in this state *)
  | Restarting_build
  (* Shut down requested. No new new builds will start *)
  | Shutting_down

module Handler = struct
  module Event = struct
    type build_result =
      | Success
      | Failure

    type t =
      | Tick
      | Source_files_changed
      | Build_interrupted
      | Build_finish of build_result
  end

  type t = Config.t -> Event.t -> unit
end

type t =
  { config : Config.t
  ; mutable status : status
  ; handler : Handler.t
  ; job_throttle : Fiber.Throttle.t
  ; events : Event.Queue.t
  ; process_watcher : Process_watcher.t
  }

let t : t Fiber.Var.t = Fiber.Var.create ()

let set x f = Fiber.Var.set t x f

let t () = Fiber.Var.get_exn t

let running_jobs_count t = Event.Queue.pending_jobs t.events

let ignore_for_watch p =
  let+ t = t () in
  Event.Queue.ignore_next_file_change_event t.events p

let with_job_slot f =
  let* t = t () in
  let raise_if_cancelled () =
    match t.status with
    | Restarting_build
    | Shutting_down ->
      raise (Memo.Non_reproducible (Failure "Build cancelled"))
    | Building -> ()
    | Waiting_for_file_changes _ ->
      (* At this stage, we're not running a build, so we shouldn't be running
         tasks here. *)
      assert false
  in
  Fiber.Throttle.run t.job_throttle ~f:(fun () ->
      raise_if_cancelled ();
      Fiber.collect_errors (fun () -> f t.config) >>= function
      | Ok res -> Fiber.return res
      | Error exns ->
        raise_if_cancelled ();
        Fiber.reraise_all exns)

(* We use this version privately in this module whenever we can pass the
   scheduler explicitly *)
let wait_for_process t pid =
  let ivar = Fiber.Ivar.create () in
  Process_watcher.register_job t.process_watcher { pid; ivar };
  Fiber.Ivar.read ivar

let global = ref None

let got_signal signal =
  if !Log.verbose then
    Log.info [ Pp.textf "Got signal %s, exiting." (Signal.name signal) ]

let filesystem_watcher_terminated () =
  Log.info [ Pp.textf "Filesystem watcher terminated, exiting." ]

type saw_signal =
  | Ok
  | Got_signal

let kill_and_wait_for_all_processes t =
  Process_watcher.killall t.process_watcher Sys.sigkill;
  let saw_signal = ref Ok in
  while Event.Queue.pending_jobs t.events > 0 do
    match Event.Queue.next t.events with
    | Signal signal ->
      got_signal signal;
      saw_signal := Got_signal
    | _ -> ()
  done;
  !saw_signal

let prepare (config : Config.t) ~(handler : Handler.t) =
  let events = Event.Queue.create config.stats in
  (* The signal watcher must be initialized first so that signals are blocked in
     all threads. *)
  Signal_watcher.init events;
  let process_watcher = Process_watcher.init events in
  let t =
    { status = Building
    ; job_throttle = Fiber.Throttle.create config.concurrency
    ; process_watcher
    ; events
    ; config
    ; handler
    }
  in
  global := Some t;
  t

module Run_once : sig
  type run_error =
    | Already_reported
    | Exn of Exn_with_backtrace.t

  (** Run the build and clean up after it (kill any stray processes etc). *)
  val run_and_cleanup : t -> (unit -> 'a Fiber.t) -> ('a, run_error) Result.t
end = struct
  type run_error =
    | Already_reported
    | Exn of Exn_with_backtrace.t

  exception Abort of run_error

  (** This function is the heart of the scheduler. It makes progress in
      executing fibers by doing the following:

      - notifying completed jobs
      - starting cancellations
      - terminating the scheduler on signals *)
  let rec iter (t : t) =
    if
      (match t.status with
      | Waiting_for_file_changes _ ->
        (* In polling mode, there are no pending jobs while we are waiting for
           file changes *)
        false
      | _ -> true)
      && Event.Queue.pending_jobs t.events = 0
      && (* CR-someday aalekseyev: Deadlock detection is pretty much useless
            when RPC is in use because there's pretty much always a "pending"
            call to [accept] that makes this condition false. We should probably
            make deadlock detection more aggressive by not taking into account
            the Ivars waiting for user input (such as [accept]) if the build is
            in progress. *)
      Event.Queue.pending_worker_tasks t.events = 0
    then
      raise
        (Abort
           (Exn
              (Exn_with_backtrace.try_with_never_returns (fun () ->
                   Code_error.raise "Deadlock" []))))
    else (
      t.handler t.config Tick;
      match Event.Queue.next t.events with
      | Job_completed (job, proc_info) -> Fiber.Fill (job.ivar, proc_info)
      | File_system_changed events -> (
        match (Fs_memo.handle events : Fs_memo.Rebuild_required.t) with
        | No -> iter t (* Ignore the event *)
        | Yes -> (
          match t.status with
          | Shutting_down
          | Restarting_build ->
            (* We're already cancelling build, so file change events don't
               matter *)
            iter t
          | Building ->
            t.handler t.config Build_interrupted;
            t.status <- Restarting_build;
            Process_watcher.killall t.process_watcher Sys.sigkill;
            iter t
          | Waiting_for_file_changes ivar -> Fill (ivar, File_system_changed)))
      | Worker_task fill -> fill
      | File_system_watcher_terminated ->
        filesystem_watcher_terminated ();
        raise (Abort Already_reported)
      | Signal signal ->
        got_signal signal;
        raise (Abort Already_reported)
    )

  let run t f : _ result =
    let fiber =
      set t (fun () ->
          Fiber.map_reduce_errors
            (module Monoid.Unit)
            f
            ~on_error:(fun e ->
              Dune_util.Report_error.report e;
              Fiber.return ()))
    in
    match Fiber.run fiber ~iter:(fun () -> iter t) with
    | Ok res ->
      assert (Event.Queue.pending_jobs t.events = 0);
      assert (Event.Queue.pending_worker_tasks t.events = 0);
      Ok res
    | Error () -> Error Already_reported
    | exception Abort err -> Error err
    | exception exn -> Error (Exn (Exn_with_backtrace.capture exn))

  let run_and_cleanup t f =
    let res = run t f in
    Console.Status_line.set_constant None;
    match kill_and_wait_for_all_processes t with
    | Got_signal -> Error Already_reported
    | Ok -> res
end

module Worker = struct
  type t =
    { worker : Thread_worker.t
    ; events : Event.Queue.t
    }

  let stop t = Thread_worker.stop t.worker

  let create () =
    let worker = Thread_worker.create ~spawn_thread:Thread.spawn in
    let+ scheduler = t () in
    { worker; events = scheduler.events }

  let task (t : t) ~f =
    let ivar = Fiber.Ivar.create () in
    let f () =
      let res = Exn_with_backtrace.try_with f in
      Event.Queue.send_worker_task_completed t.events (Fiber.Fill (ivar, res))
    in
    match Thread_worker.add_work t.worker ~f with
    | Error `Stopped -> Fiber.return (Error `Stopped)
    | Ok () -> (
      Event.Queue.register_worker_task_started t.events;
      let+ res = Fiber.Ivar.read ivar in
      match res with
      | Error exn -> Error (`Exn exn)
      | Ok e -> Ok e)
end

module Run = struct
  type file_watcher =
    | Detect_external
    | No_watcher

  module Event_queue = Event.Queue
  module Event = Handler.Event

  let poll step =
    let* t = t () in
    let rec loop () : unit Fiber.t =
      t.status <- Building;
      let* res =
        let on_error exn =
          (match t.status with
          | Building -> Dune_util.Report_error.report exn
          | Shutting_down
          | Restarting_build ->
            ()
          | Waiting_for_file_changes _ ->
            (* We are inside a build, so we aren't waiting for a file change
               event *)
            assert false);
          Fiber.return ()
        in
        Fiber.map_reduce_errors (module Monoid.Unit) ~on_error step
      in
      match t.status with
      | Waiting_for_file_changes _ ->
        (* We just finished a build, so there's no way this was set *)
        assert false
      | Shutting_down -> Fiber.return ()
      | Restarting_build ->
        Memo.reset ();
        loop ()
      | Building -> (
        let build_result : Handler.Event.build_result =
          match res with
          | Error _ -> Failure
          | Ok _ -> Success
        in
        t.handler t.config (Build_finish build_result);
        let ivar = Fiber.Ivar.create () in
        t.status <- Waiting_for_file_changes ivar;
        let* next = Fiber.Ivar.read ivar in
        match next with
        | Shutdown_requested -> Fiber.return ()
        | File_system_changed -> (
          Memo.reset ();
          t.handler t.config Source_files_changed;
          match res with
          | Error _
          | Ok `Continue ->
            loop ()
          | Ok `Stop -> Fiber.return ()))
    in
    loop ()

  exception Shutdown_requested

  let go config ?(file_watcher = No_watcher)
      ~(on_event : Config.t -> Handler.Event.t -> unit) run =
    let t = prepare config ~handler:on_event in
    let watcher =
      match file_watcher with
      | No_watcher -> None
      | Detect_external ->
        Some
          (Dune_file_watcher.create_default
             ~scheduler:
               { spawn_thread = Thread.spawn
               ; thread_safe_send_events =
                   (fun files_changed ->
                     Event_queue.send_file_watcher_events t.events files_changed)
               })
    in
    let result =
      match Run_once.run_and_cleanup t run with
      | Ok a -> Result.Ok a
      | Error Already_reported ->
        let exn =
          if t.status = Shutting_down then
            Shutdown_requested
          else
            Dune_util.Report_error.Already_reported
        in
        Error (exn, None)
      | Error (Exn exn_with_bt) ->
        Error (exn_with_bt.exn, Some exn_with_bt.backtrace)
    in
    Option.iter watcher ~f:(fun watcher ->
        ignore (wait_for_process t (Dune_file_watcher.pid watcher) : _ Fiber.t));
    ignore (kill_and_wait_for_all_processes t : saw_signal);
    match result with
    | Ok a -> a
    | Error (exn, None) -> Exn.raise exn
    | Error (exn, Some bt) -> Exn.raise_with_backtrace exn bt
end

let wait_for_process pid =
  let* t = t () in
  wait_for_process t pid

let shutdown () =
  let* t = t () in
  let fill_file_changes =
    match t.status with
    | Waiting_for_file_changes ivar -> Fiber.Ivar.fill ivar Shutdown_requested
    | _ -> Fiber.return ()
  in
  t.status <- Shutting_down;
  fill_file_changes
