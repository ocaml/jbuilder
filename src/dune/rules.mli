(** A collection of rules across a known finite set of directories *)

open! Stdune

(** Represent a set of rules producing files in a given directory *)
module Dir_rules : sig
  type t

  val empty : t

  val union : t -> t -> t

  type alias_action =
    { stamp : Digest.t
    ; action : Action.t Build.With_targets.t
    ; locks : Path.t list
    ; context : Context.t
    ; env : Env.t option
    ; loc : Loc.t option
    }

  module Alias_spec : sig
    type t =
      { deps : Path.Set.t
      ; dyn_deps : Path.Set.t Build.t
      ; actions : alias_action Appendable_list.t
      }
  end

  (** A ready to process view of the rules of a directory *)
  type ready =
    { rules : Rule.t list
    ; aliases : Alias_spec.t Alias.Name.Map.t
    }

  val consume : t -> ready

  val is_subset : t -> of_:t -> bool

  val is_empty : t -> bool

  val to_dyn : t -> Dyn.t
end

(** A value of type [t] holds a set of rules for multiple directories *)
type t

val to_map : t -> Dir_rules.t Path.Build.Map.t

module Produce : sig
  (* CR-someday aalekseyev: the below comments are not quite right *)

  (** Add a rule to the system. This function must be called from the
      [gen_rules] callback. All the target of the rule must be in the same
      directory.

      Assuming that [gen_rules ~dir:a] calls [add_rule r] where [r.dir] is [b],
      one of the following assumption must hold:

      - [a] and [b] are the same - [gen_rules ~dir:b] calls [load_dir ~dir:a]

      The call to [load_dir ~dir:a] from [gen_rules ~dir:b] declares a directory
      dependency from [b] to [a]. There must be no cyclic directory
      dependencies. *)
  val rule : Rule.t -> unit

  module Alias : sig
    type t = Alias.t

    (** [add_deps store alias ?dyn_deps deps] arrange things so that all
        [dyn_deps] and [deps] are built as part of the build of alias [alias]. *)
    val add_deps : t -> ?dyn_deps:Path.Set.t Build.t -> Path.Set.t -> unit

    (** [add_action store alias ~stamp action] arrange things so that [action]
        is executed as part of the build of alias [alias]. [stamp] is any
        S-expression that is unique and persistent S-expression. *)
    val add_action :
         t
      -> context:Context.t
      -> env:Env.t option
      -> loc:Loc.t option
      -> ?locks:Path.t list
      -> stamp:_
      -> Action.t Build.With_targets.t
      -> unit
  end
end

val implicit_output : t Memo.Implicit_output.t

val empty : t

val union : t -> t -> t

val produce_dir : dir:Path.Build.t -> Dir_rules.t -> unit

val produce_dir' : dir:Path.t -> Dir_rules.t -> unit

val produce : t -> unit

val produce_opt : t option -> unit

val is_subset : t -> of_:t -> bool

val map_rules : t -> f:(Rule.t -> Rule.t) -> t

val collect_async : (unit -> 'a Fiber.t) -> ('a * t) Fiber.t

val collect_opt_async : (unit -> 'a Fiber.t) -> ('a * t option) Fiber.t

val collect : (unit -> 'a) -> 'a * t

val collect_unit : (unit -> unit) -> t

val collect_opt : (unit -> 'a) -> 'a * t option

(** returns [Dir_rules.empty] for non-build paths *)
val find : t -> Path.t -> Dir_rules.t
