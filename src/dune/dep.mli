open Stdune

type t = private
  | Env of Env.Var.t
  | File of Path.t
  | Alias of Alias.t
  | File_selector of File_selector.t
  | Universe
  | Sandbox_config of Sandbox_config.t

val file : Path.t -> t

val env : Env.Var.t -> t

val universe : t

val file_selector : File_selector.t -> t

val alias : Alias.t -> t

val sandbox_config : Sandbox_config.t -> t

val compare : t -> t -> Ordering.t

type eval_pred = File_selector.t -> Path.Set.t Fiber.t

module Trace : sig
  type t
end

module Set : sig
  include Set.S with type elt = t

  val has_universe : t -> bool

  val sandbox_config : t -> Sandbox_config.t

  val of_files : Path.t list -> t

  val of_files_set : Path.Set.t -> t

  val paths : t -> eval_pred:eval_pred -> Path.Set.t Fiber.t

  val encode : t -> Dune_lang.t

  val trace :
       t
    -> sandbox_mode:Sandbox_mode.t
    -> env:Env.t
    -> eval_pred:eval_pred
    -> Trace.t Fiber.t

  val add_paths : t -> Path.Set.t -> t

  val parallel_iter : t -> f:(elt -> unit Fiber.t) -> unit Fiber.t

  val parallel_iter_files :
    t -> f:(Path.t -> unit Fiber.t) -> eval_pred:eval_pred -> unit Fiber.t

  val dirs : t -> Path.Set.t
end
