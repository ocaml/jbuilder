open Stdune
open Fiber.O

module Scheduler = struct
  type t =
    { create_thread_safe_ivar : 'a. unit -> 'a Fiber.Ivar.t * ('a -> unit)
    ; spawn_thread : (unit -> unit) -> unit
    }
end

type t =
  { worker : Worker.t
  ; scheduler : Scheduler.t
  }

let stop t = Worker.stop t.worker

let create (scheduler : Scheduler.t) =
  let worker = Worker.create ~spawn_thread:scheduler.spawn_thread in
  { worker; scheduler }

let task (t : t) ~f =
  let ivar, fill = t.scheduler.create_thread_safe_ivar () in
  let f () = fill (Result.try_with f) in
  match Worker.add_work t.worker ~f with
  | Ok () -> Fiber.Ivar.read ivar
  | Error `Stopped -> Code_error.raise "worker stopped" []

let task_exn t ~f =
  let+ res = task t ~f in
  match res with
  | Ok s -> s
  | Error e -> raise e
