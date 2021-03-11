(** A system thread used to execute blocking functions asynchronously *)

type t

(** Spawn a new worker thread *)
val create : spawn_thread:((unit -> unit) -> unit) -> t

(** [add_work t ~f] add the work of running [f] to this worker *)
val add_work : t -> f:(unit -> unit) -> (unit, [ `Stopped ]) result

val stop : t -> unit
