open! Import

(** A simple wrapper around [Deps.t], where some dependencies are recorded as
    [rule_deps] and other as [action_deps]. Action dependencies are dependencies
    the external commands are expected to access, and rule dependencies are
    dependencies needed in order to compute the action to execute as well as its
    dependencies. *)

type t =
  { rule_deps : Dep.Set.t
  ; action_deps : Dep.Set.t
  }

val to_dyn : t -> Dyn.t

(** No dependencies. *)
val empty : t

(** Union of dependencies. *)
val union : t -> t -> t

(** The paths to both rule and action dependencies. *)
val paths : t -> eval_pred:Dep.eval_pred -> Path.Set.t Fiber.t
