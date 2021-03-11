let on_error = Report_error.report

open! Stdune
open Import

module Config = struct
  include Config

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

  module Rpc = struct
    type t =
      | Client
      | Server of
          { handler : Dune_rpc_server.t
          ; backlog : int
          }
  end

  type t =
    { concurrency : int
    ; display : Display.t
    ; rpc : Rpc.t option
    }
end

type job =
  { pid : Pid.t
  ; ivar : Unix.process_status Fiber.Ivar.t
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
    | Files_changed of Path.t list
    | Job_completed of job * Unix.process_status
    | Signal of Signal.t
    | Rpc of Fiber.fill
    | Jobserver of Fiber.fill

  module Queue : sig
    type t

    type event

    val create : unit -> t

    (** Return the next event. File changes event are always flattened and
        returned first. *)
    val next : t -> event

    (** Handle all enqueued deduplications. *)
    val flush_sync_tasks : t -> unit

    (** Ignore the ne next file change event about this file. *)
    val ignore_next_file_change_event : t -> Path.t -> unit

    (** Pending rpc events *)
    val pending_rpc : t -> int

    (** Register the fact that a job was started. *)
    val register_job_started : t -> unit

    (** Number of jobs for which the status hasn't been reported yet .*)
    val pending_jobs : t -> int

    val send_rpc_completed : t -> Fiber.fill -> unit

    val register_rpc_started : t -> unit

    val send_jobserver_completed : t -> Fiber.fill -> unit

    val register_jobserver_started : t -> unit

    (** Send an event to the main thread. *)
    val send_files_changed : t -> Path.t list -> unit

    val send_job_completed : t -> job -> Unix.process_status -> unit

    val send_signal : t -> Signal.t -> unit

    val send_sync_task : t -> (unit -> unit) -> unit
  end
  with type event := t
end = struct
  type t =
    | Files_changed of Path.t list
    | Job_completed of job * Unix.process_status
    | Signal of Signal.t
    | Rpc of Fiber.fill
    | Jobserver of Fiber.fill

  module Queue = struct
    type t =
      { jobs_completed : (job * Unix.process_status) Queue.t
      ; sync_tasks : (unit -> unit) Queue.t
      ; mutable files_changed : Path.t list
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
      ; mutable pending_rpc : int
      ; mutable pending_jobserver : int
      ; rpc_completed : Fiber.fill Queue.t
      ; jobserver_completed : Fiber.fill Queue.t
      }

    let create () =
      let jobs_completed = Queue.create () in
      let rpc_completed = Queue.create () in
      let jobserver_completed = Queue.create () in
      let sync_tasks = Queue.create () in
      let files_changed = [] in
      let signals = Signal.Set.empty in
      let mutex = Mutex.create () in
      let cond = Condition.create () in
      let ignored_files = Table.create (module String) 64 in
      let pending_jobs = 0 in
      let pending_rpc = 0 in
      let pending_jobserver = 0 in
      { jobs_completed
      ; sync_tasks
      ; files_changed
      ; signals
      ; mutex
      ; cond
      ; ignored_files
      ; pending_jobs
      ; rpc_completed
      ; pending_rpc
      ; jobserver_completed
      ; pending_jobserver
      }

    let register_job_started q = q.pending_jobs <- q.pending_jobs + 1

    let register_rpc_started q = q.pending_rpc <- q.pending_rpc + 1

    let register_jobserver_started q =
      q.pending_jobserver <- q.pending_jobserver + 1

    let ignore_next_file_change_event q path =
      assert (Path.is_in_source_tree path);
      Table.set q.ignored_files (Path.to_absolute_filename path) ()

    let available q =
      not
        (List.is_empty q.files_changed
        && Queue.is_empty q.jobs_completed
        && Signal.Set.is_empty q.signals
        && Queue.is_empty q.sync_tasks
        && Queue.is_empty q.rpc_completed
        && Queue.is_empty q.jobserver_completed)

    let sync_task q =
      match Queue.pop q.sync_tasks with
      | None -> false
      | Some task ->
        task ();
        true

    let rec flush_sync_tasks q = if sync_task q then flush_sync_tasks q

    let next q =
      Stats.record ();
      Mutex.lock q.mutex;
      let rec loop () =
        while not (available q) do
          Condition.wait q.cond q.mutex
        done;
        if sync_task q then
          loop ()
        else
          match Signal.Set.choose q.signals with
          | Some signal ->
            q.signals <- Signal.Set.remove q.signals signal;
            Signal signal
          | None -> (
            match q.files_changed with
            | [] -> (
              match Queue.pop q.jobs_completed with
              | None -> (
                match Queue.pop q.rpc_completed with
                | Some rpc ->
                  q.pending_rpc <- q.pending_rpc - 1;
                  Rpc rpc
                | None ->
                  q.pending_jobserver <- q.pending_jobserver - 1;
                  Jobserver (Queue.pop_exn q.jobserver_completed))
              | Some (job, status) ->
                q.pending_jobs <- q.pending_jobs - 1;
                assert (q.pending_jobs >= 0);
                Job_completed (job, status))
            | fns -> (
              q.files_changed <- [];
              let files =
                List.filter fns ~f:(fun fn ->
                    let fn = Path.to_absolute_filename fn in
                    if Table.mem q.ignored_files fn then (
                      (* only use ignored record once *)
                      Table.remove q.ignored_files fn;
                      false
                    ) else
                      true)
              in
              match files with
              | [] -> loop ()
              | _ :: _ -> Files_changed files))
      in
      let ev = loop () in
      Mutex.unlock q.mutex;
      ev

    let send_rpc_completed q event =
      Mutex.lock q.mutex;
      let avail = available q in
      Queue.push q.rpc_completed event;
      if not avail then Condition.signal q.cond;
      Mutex.unlock q.mutex

    let send_jobserver_completed q event =
      Mutex.lock q.mutex;
      let avail = available q in
      Queue.push q.jobserver_completed event;
      if not avail then Condition.signal q.cond;
      Mutex.unlock q.mutex

    let send_files_changed q files =
      Mutex.lock q.mutex;
      let avail = available q in
      q.files_changed <- List.rev_append files q.files_changed;
      if not avail then Condition.signal q.cond;
      Mutex.unlock q.mutex

    let send_job_completed q job status =
      Mutex.lock q.mutex;
      let avail = available q in
      Queue.push q.jobs_completed (job, status);
      if not avail then Condition.signal q.cond;
      Mutex.unlock q.mutex

    let send_signal q signal =
      Mutex.lock q.mutex;
      let avail = available q in
      q.signals <- Signal.Set.add q.signals signal;
      if not avail then Condition.signal q.cond;
      Mutex.unlock q.mutex

    let send_sync_task q f =
      Mutex.lock q.mutex;
      let avail = available q in
      Queue.push q.sync_tasks f;
      if not avail then Condition.signal q.cond;
      Mutex.unlock q.mutex

    let pending_jobs q = q.pending_jobs

    let pending_rpc q = q.pending_rpc
  end
end

module File_watcher : sig
  type t

  (** Create a new file watcher. *)
  val create : Event.Queue.t -> t

  (** Pid of the external file watcher process *)
  val pid : t -> Pid.t
end = struct
  type t = Pid.t

  let pid t = t

  let command =
    lazy
      (let excludes =
         [ {|/_build|}
         ; {|/_opam|}
         ; {|/_esy|}
         ; {|/\..+|}
         ; {|~$|}
         ; {|/#[^#]*#$|}
         ; {|4913|} (* https://github.com/neovim/neovim/issues/3460 *)
         ]
       in
       let path = Path.to_string_maybe_quoted Path.root in
       match
         if Sys.linux then
           Bin.which ~path:(Env.path Env.initial) "inotifywait"
         else
           None
       with
       | Some inotifywait ->
         (* On Linux, use inotifywait. *)
         let excludes = String.concat ~sep:"|" excludes in
         ( inotifywait
         , [ "-r"
           ; path
           ; "--exclude"
           ; excludes
           ; "-e"
           ; "close_write"
           ; "-e"
           ; "delete"
           ; "--format"
           ; "%w%f"
           ; "-m"
           ; "-q"
           ] )
       | None -> (
         (* On all other platforms, try to use fswatch. fswatch's event
            filtering is not reliable (at least on Linux), so don't try to use
            it, instead act on all events. *)
         match Bin.which ~path:(Env.path Env.initial) "fswatch" with
         | Some fswatch ->
           let excludes =
             List.concat_map excludes ~f:(fun x -> [ "--exclude"; x ])
           in
           ( fswatch
           , [ "-r"
             ; path
             ; "--event"
             ; "Created"
             ; "--event"
             ; "Updated"
             ; "--event"
             ; "Removed"
             ]
             @ excludes )
         | None ->
           User_error.raise
             [ Pp.text
                 (if Sys.linux then
                   "Please install inotifywait to enable watch mode. If \
                    inotifywait is unavailable, fswatch may also be used but \
                    will result in a worse experience."
                 else
                   "Please install fswatch to enable watch mode.")
             ]))

  let buffering_time = 0.5 (* seconds *)

  let buffer_capacity = 65536

  type buffer =
    { data : Bytes.t
    ; mutable size : int
    }

  let read_lines buffer fd =
    let len =
      Unix.read fd buffer.data buffer.size (buffer_capacity - buffer.size)
    in
    buffer.size <- buffer.size + len;
    let lines = ref [] in
    let line_start = ref 0 in
    for i = 0 to buffer.size - 1 do
      let c = Bytes.get buffer.data i in
      if c = '\n' || c = '\r' then (
        (if !line_start < i then
          let line =
            Bytes.sub_string buffer.data ~pos:!line_start ~len:(i - !line_start)
          in
          lines := line :: !lines);
        line_start := i + 1
      )
    done;
    buffer.size <- buffer.size - !line_start;
    Bytes.blit ~src:buffer.data ~src_pos:!line_start ~dst:buffer.data ~dst_pos:0
      ~len:buffer.size;
    List.rev !lines

  let spawn_external_watcher () =
    let prog, args = Lazy.force command in
    let prog = Path.to_absolute_filename prog in
    let argv = prog :: args in
    let r, w = Unix.pipe () in
    let pid = Spawn.spawn () ~prog ~argv ~stdout:w in
    Unix.close w;
    (r, pid)

  let create q =
    let files_changed = ref [] in
    let event_mtx = Mutex.create () in
    let event_cv = Condition.create () in
    let worker_thread pipe =
      let buffer = { data = Bytes.create buffer_capacity; size = 0 } in
      while true do
        let lines = List.map (read_lines buffer pipe) ~f:Path.of_string in
        Mutex.lock event_mtx;
        files_changed := List.rev_append lines !files_changed;
        Condition.signal event_cv;
        Mutex.unlock event_mtx
      done
    in

    (* The buffer thread is used to avoid flooding the main thread with file
       changes events when a lot of file changes are reported at once. In
       particular, this avoids restarting the build over and over in a short
       period of time when many events are reported at once.

       It works as follow:

       - when the first event is received, send it to the main thread
       immediately so that we get a fast response time

       - after the first event is received, buffer subsequent events for
       [buffering_time] *)
    let rec buffer_thread () =
      Mutex.lock event_mtx;
      while List.is_empty !files_changed do
        Condition.wait event_cv event_mtx
      done;
      let files = !files_changed in
      files_changed := [];
      Mutex.unlock event_mtx;
      Event.Queue.send_files_changed q files;
      Thread.delay buffering_time;
      buffer_thread ()
    in
    let pipe, pid = spawn_external_watcher () in
    ignore (Thread.create worker_thread pipe : Thread.t);
    ignore (Thread.create buffer_thread () : Thread.t);
    pid
end

module Jobserver : sig
  type t

  val create : Event.Queue.t -> Env.t -> t option

  val with_job : t -> f:(unit -> 'a Fiber.t) -> 'a Fiber.t
end = struct
  type t =
    { jobserver : Jobserver.t
    ; read : Task_queue.t
    ; write : Task_queue.t
    }

  let create q env =
    let open Option.O in
    let scheduler =
      let create_thread_safe_ivar () =
        let ivar = Fiber.Ivar.create () in
        let fill v =
          Event.Queue.send_jobserver_completed q (Fiber.Fill (ivar, v))
        in
        Event.Queue.register_jobserver_started q;
        (ivar, fill)
      in
      { Task_queue.Scheduler.create_thread_safe_ivar
      ; spawn_thread = Thread.spawn
      }
    in
    let+ jobserver = Jobserver.of_env env in
    let read = Task_queue.create scheduler in
    let write = Task_queue.create scheduler in
    { jobserver; read; write }

  let with_job { read; write; jobserver } ~f =
    let open Fiber.O in
    let* job =
      Task_queue.task_exn read ~f:(fun () -> Jobserver.wait_next jobserver)
    in
    Fiber.finalize f ~finally:(fun () ->
        let+ res =
          Task_queue.task write ~f:(fun () -> Jobserver.Job.release job)
        in
        match res with
        | Ok ()
        | Error _ ->
          ())
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
    | Zombie of Unix.process_status

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

    val remove : t -> pid:Pid.t -> Unix.process_status -> unit

    val running_count : t -> int

    val iter : t -> f:(job -> unit) -> unit
  end = struct
    let add t job =
      match Table.find t.table job.pid with
      | None ->
        Table.set t.table job.pid (Running job);
        t.running_count <- t.running_count + 1;
        if t.running_count = 1 then Condition.signal t.something_is_running
      | Some (Zombie status) ->
        Table.remove t.table job.pid;
        Event.Queue.send_job_completed t.events job status
      | Some (Running _) -> assert false

    let remove t ~pid status =
      match Table.find t.table pid with
      | None -> Table.set t.table pid (Zombie status)
      | Some (Running job) ->
        t.running_count <- t.running_count - 1;
        Table.remove t.table pid;
        Event.Queue.send_job_completed t.events job status
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

  exception Finished of job * Unix.process_status

  let wait_nonblocking_win32 t =
    try
      Process_table.iter t ~f:(fun job ->
          let pid, status = Unix.waitpid [ WNOHANG ] (Pid.to_int job.pid) in
          if pid <> 0 then raise_notrace (Finished (job, status)));
      false
    with
    | Finished (job, status) ->
      (* We need to do the [Unix.waitpid] and remove the process while holding
         the lock, otherwise the pid might be reused in between. *)
      Process_table.remove t ~pid:job.pid status;
      true

  let wait_win32 t =
    while not (wait_nonblocking_win32 t) do
      Mutex.unlock t.mutex;
      Thread.delay 0.001;
      Mutex.lock t.mutex
    done

  let wait_unix t =
    Mutex.unlock t.mutex;
    let pid, status = Unix.wait () in
    Mutex.lock t.mutex;
    let pid = Pid.of_int pid in
    Process_table.remove t ~pid status

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

module Rpc0 = struct
  (* This module is called [Rpc0] to avoid conflict with the public [Rpc] module *)

  type cleanup =
    { symlink : Path.t
    ; socket : Path.t
    }

  type t =
    | Client
    | Server of
        { server : Csexp_rpc.Server.t
        ; handler : Dune_rpc_server.t
        ; where : Dune_rpc.Where.t
        ; cleanup : cleanup option
        }

  module Server = Dune_rpc_server.Make (Csexp_rpc.Session)

  let scheduler q =
    let create_thread_safe_ivar () =
      let ivar = Fiber.Ivar.create () in
      let fill v = Event.Queue.send_rpc_completed q (Fiber.Fill (ivar, v)) in
      Event.Queue.register_rpc_started q;
      (ivar, fill)
    in
    { Task_queue.Scheduler.create_thread_safe_ivar
    ; spawn_thread = Thread.spawn
    }

  let delete_cleanup = function
    | None -> ()
    | Some { socket; symlink } ->
      Path.unlink_no_err socket;
      Path.unlink_no_err symlink

  let where_to_socket = function
    | `Ip (addr, `Port port) -> Unix.ADDR_INET (addr, port)
    | `Unix p -> Unix.ADDR_UNIX (Path.to_string p)

  let of_config events rpc =
    Option.map rpc ~f:(function
      | Config.Rpc.Client -> Client
      | Config.Rpc.Server { handler; backlog } ->
        let where = Dune_rpc.Where.default () in
        let real_where, cleanup =
          match where with
          | `Ip _ -> (where_to_socket where, None)
          | `Unix symlink ->
            let socket =
              let dir =
                Path.of_string
                  (match Xdg.runtime_dir with
                  | Some p -> p
                  | None -> Filename.get_temp_dir_name ())
              in
              Temp.temp_path ~dir ~prefix:"dune" ~suffix:""
            in
            Unix.symlink (Path.to_string socket)
              (let from = Path.external_ (Path.External.cwd ()) in
               Path.mkdir_p (Path.parent_exn symlink);
               Path.reach_for_running ~from symlink);
            let cleanup = Some { socket; symlink } in
            at_exit (fun () -> delete_cleanup cleanup);
            (ADDR_UNIX (Path.to_string socket), cleanup)
        in
        let server =
          Csexp_rpc.Server.create real_where ~backlog (scheduler events)
        in
        Server { server; handler; where; cleanup })

  let with_rpc_serve t f =
    match t with
    | None
    | Some Client ->
      f
    | Some (Server t) ->
      fun () ->
        Fiber.fork_and_join_unit
          (fun () ->
            Fiber.finalize
              (fun () ->
                let open Fiber.O in
                let* sessions = Csexp_rpc.Server.serve t.server in
                Server.serve sessions t.handler)
              ~finally:(fun () ->
                delete_cleanup t.cleanup;
                Fiber.return ()))
          f
end

type status =
  (* Waiting for file changes to start a new a build *)
  | Waiting_for_file_changes of unit Fiber.Ivar.t
  (* Running a build *)
  | Building
  (* Cancellation requested. Build jobs are immediately rejected in this state *)
  | Restarting_build

module Handler = struct
  type t =
    { new_event : Config.t -> unit
    ; build_interrupted : Config.t -> unit
    }
end

type t =
  { original_cwd : string
  ; config : Config.t
  ; polling : bool
  ; rpc : Rpc0.t option
  ; mutable status : status
  ; handler : Handler.t
  ; job_throttle : Fiber.Throttle.t
  ; events : Event.Queue.t
  ; process_watcher : Process_watcher.t
  ; jobserver : Jobserver.t option Lazy.t
  }

let t_var : t Fiber.Var.t = Fiber.Var.create ()

let running_jobs_count () =
  let t = Fiber.Var.get_exn t_var in
  Event.Queue.pending_jobs t.events

let ignore_for_watch p =
  let t = Fiber.Var.get_exn t_var in
  Event.Queue.ignore_next_file_change_event t.events p

let with_chdir ~dir ~f =
  let t = Fiber.Var.get_exn t_var in
  Sys.chdir (Path.to_string dir);
  protect ~finally:(fun () -> Sys.chdir t.original_cwd) ~f

exception Cancel_build

let with_job_slot f =
  let t = Fiber.Var.get_exn t_var in
  let run =
    match Lazy.force t.jobserver with
    | None -> Fiber.Throttle.run t.job_throttle
    | Some jobserver -> Jobserver.with_job jobserver
  in
  run ~f:(fun () ->
      match t.status with
      | Restarting_build -> raise Cancel_build
      | Building -> f t.config
      | Waiting_for_file_changes _ ->
        (* At this stage, we're not running a build, so we shouldn't be running
           tasks here. *)
        assert false)

(* We use this version privately in this module whenever we can pass the
   scheduler explicitly *)
let wait_for_process t pid =
  let ivar = Fiber.Ivar.create () in
  Process_watcher.register_job t.process_watcher { pid; ivar };
  Fiber.Ivar.read ivar

let global = ref None

let wait_for_dune_cache () =
  let t = Option.value_exn !global in
  Event.Queue.flush_sync_tasks t.events

let got_signal signal =
  if !Log.verbose then
    Log.info [ Pp.textf "Got signal %s, exiting." (Signal.name signal) ]

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

let prepare (config : Config.t) ~polling ~(handler : Handler.t) =
  let events = Event.Queue.create () in
  (* The signal watcher must be initialized first so that signals are blocked in
     all threads. *)
  Signal_watcher.init events;
  let process_watcher = Process_watcher.init events in
  let cwd = Sys.getcwd () in
  let rpc = Rpc0.of_config events config.rpc in
  let jobserver = lazy (Jobserver.create events Env.initial) in
  let t =
    { original_cwd = cwd
    ; status = Building
    ; job_throttle = Fiber.Throttle.create config.concurrency
    ; polling
    ; process_watcher
    ; events
    ; config
    ; rpc
    ; handler
    ; jobserver
    }
  in
  global := Some t;
  t

module Run_once : sig
  type run_error =
    | Got_signal
    | Never
    | Exn of Exn_with_backtrace.t

  (** Run the build and clean up after it (kill any stray processes etc). *)
  val run_and_cleanup : t -> (unit -> 'a Fiber.t) -> ('a, run_error) Result.t
end = struct
  type run_error =
    | Got_signal
    | Never
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
      && Event.Queue.pending_rpc t.events = 0
    then
      raise (Abort Never)
    else (
      t.handler.new_event t.config;
      match Event.Queue.next t.events with
      | Job_completed (job, status) -> Fiber.Fill (job.ivar, status)
      | Files_changed changed_files -> (
        List.iter changed_files ~f:Fs_notify_memo.invalidate;
        match t.status with
        | Restarting_build ->
          (* We're already cancelling build, so file change events don't matter *)
          iter t
        | Building ->
          t.handler.build_interrupted t.config;
          t.status <- Restarting_build;
          Process_watcher.killall t.process_watcher Sys.sigkill;
          iter t
        | Waiting_for_file_changes ivar -> Fill (ivar, ()))
      | Jobserver fill
      | Rpc fill ->
        fill
      | Signal signal ->
        got_signal signal;
        raise (Abort Got_signal)
    )

  let run t f : _ result =
    let fiber =
      Fiber.Var.set t_var t (fun () -> Fiber.with_error_handler f ~on_error)
    in
    match Fiber.run fiber ~iter:(fun () -> iter t) with
    | res ->
      assert (Event.Queue.pending_jobs t.events = 0);
      assert (Event.Queue.pending_rpc t.events = 0);
      Ok res
    | exception Abort err -> Error err
    | exception exn -> Error (Exn (Exn_with_backtrace.capture exn))

  let run_and_cleanup t f =
    let res = run t f in
    Console.Status_line.set (Fun.const None);
    match kill_and_wait_for_all_processes t with
    | Got_signal -> Error Got_signal
    | Ok -> res
end

module Run = struct
  module Event = struct
    type build_result =
      | Success
      | Failure

    type go = Tick

    type poll =
      | Go of go
      | Source_files_changed
      | Build_interrupted
      | Build_finish of build_result

    let to_handler_poll ~on_event =
      { Handler.build_interrupted = (fun cfg -> on_event cfg Build_interrupted)
      ; new_event = (fun cfg -> on_event cfg (Go Tick))
      }

    let to_handler_go ~on_event : Handler.t =
      { Handler.build_interrupted = (fun _ -> assert false)
      ; new_event = (fun cfg -> on_event cfg Tick)
      }

    let go t run =
      let res =
        let f = Rpc0.with_rpc_serve t.rpc run in
        Run_once.run_and_cleanup t f
      in
      match res with
      | Error (Exn exn) -> Exn_with_backtrace.reraise exn
      | Ok res -> res
      | Error (Got_signal | Never) ->
        raise Dune_util.Report_error.Already_reported
  end

  let poll config ~on_event ~once ~finally =
    let handler = Event.to_handler_poll ~on_event in
    let t = prepare config ~polling:true ~handler in
    let watcher = File_watcher.create t.events in
    let rec loop () : unit Fiber.t =
      t.status <- Building;
      let open Fiber.O in
      let* res =
        let+ res =
          let on_error exn () =
            match t.status with
            | Building -> Report_error.report exn
            | Restarting_build -> ()
            | Waiting_for_file_changes _ ->
              (* We are inside a build, so we aren't waiting for a file change
                 event *)
              assert false
          in
          Fiber.fold_errors ~init:() ~on_error once
        in
        finally ();
        res
      in
      match t.status with
      | Waiting_for_file_changes _ ->
        (* We just finished a build, so there's no way this was set *)
        assert false
      | Restarting_build -> loop ()
      | Building -> (
        on_event t.config
          (Build_finish
             (match res with
             | Error _ -> Failure
             | Ok _ -> Success));
        let ivar = Fiber.Ivar.create () in
        t.status <- Waiting_for_file_changes ivar;
        let* () = Fiber.Ivar.read ivar in
        on_event t.config Source_files_changed;
        match res with
        | Error _
        | Ok `Continue ->
          loop ()
        | Ok `Stop -> Fiber.return ())
    in
    let run = Rpc0.with_rpc_serve t.rpc loop in
    let exn, bt =
      match Run_once.run_and_cleanup t run with
      | Ok () ->
        (* Polling mode is an infinite loop. We aren't going to terminate *)
        assert false
      | Error (Got_signal | Never) ->
        (Dune_util.Report_error.Already_reported, None)
      | Error (Exn exn_with_bt) -> (exn_with_bt.exn, Some exn_with_bt.backtrace)
    in
    ignore (wait_for_process t (File_watcher.pid watcher) : _ Fiber.t);
    ignore (kill_and_wait_for_all_processes t : saw_signal);
    match bt with
    | None -> Exn.raise exn
    | Some bt -> Exn.raise_with_backtrace exn bt

  let go config ~(on_event : Config.t -> Event.go -> unit) run =
    let handler = Event.to_handler_go ~on_event in
    let t = prepare config ~polling:false ~handler in
    Event.go t run
end

let send_sync_task d =
  let t = Option.value_exn !global in
  Event.Queue.send_sync_task t.events d

let wait_for_process pid =
  let t = Fiber.Var.get_exn t_var in
  wait_for_process t pid

module Rpc = struct
  let csexp_client p =
    let t = Fiber.Var.get_exn t_var in
    Csexp_rpc.Client.create (Rpc0.where_to_socket p) (Rpc0.scheduler t.events)

  let client p init ~on_notification ~f =
    let open Fiber.O in
    let c = csexp_client p in
    let* session = Csexp_rpc.Client.connect c in
    Drpc_client.connect_raw session init ~on_notification ~f

  let csexp_connect in_ out =
    let t = Option.value_exn !global in
    Csexp_rpc.Session.create in_ out (Rpc0.scheduler t.events)

  let add_to_env env =
    let t = Fiber.Var.get_exn t_var in
    match t.rpc with
    | None -> env
    | Some Client -> env
    | Some (Server server) -> Dune_rpc.Where.add_to_env server.where env

  let stop () =
    let t = Fiber.Var.get_exn t_var in
    match t.rpc with
    | None
    | Some Client ->
      Code_error.raise "rpc not running" []
    | Some (Server s) ->
      Csexp_rpc.Server.stop s.server;
      Fiber.return ()
end
