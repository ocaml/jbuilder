open Stdune

module Job : sig
  type t

  val release : t -> unit
end

type t

val wait_next : t -> Job.t

val of_env : Env.t -> t option
