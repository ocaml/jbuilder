open Stdune

module Scheduler : sig
  (** Hook into the fiber scheduler. *)
  type t =
    { create_thread_safe_ivar : 'a. unit -> 'a Fiber.Ivar.t * ('a -> unit)
          (** Create a thread safe Ivar. The returned function must be called
              from a separate thread in order to fill the ivar. *)
    ; spawn_thread : (unit -> unit) -> unit
          (** We spawn threads through this function in case the scheduler wants
              to block signals *)
    }
end

type t

val create : Scheduler.t -> t

val task : t -> f:(unit -> 'a) -> 'a Or_exn.t Fiber.t

val task_exn : t -> f:(unit -> 'a) -> 'a Fiber.t

val stop : t -> unit
