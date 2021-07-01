open! Stdune
open! Import

type 'a t

module O : sig
  val ( >>> ) : unit t -> 'a t -> 'a t

  val ( >>= ) : 'a t -> ('a -> 'b t) -> 'b t

  val ( >>| ) : 'a t -> ('a -> 'b) -> 'b t

  val ( let* ) : 'a t -> ('a -> 'b t) -> 'b t

  val ( and* ) : 'a t -> 'b t -> ('a * 'b) t

  val ( let+ ) : 'a t -> ('a -> 'b) -> 'b t

  val ( and+ ) : 'a t -> 'b t -> ('a * 'b) t
end

val return : 'a -> 'a t

val bind : 'a t -> f:('a -> 'b t) -> 'b t

val map : 'a t -> f:('a -> 'b) -> 'b t

val map2 : 'a t -> 'b t -> f:('a -> 'b -> 'c) -> 'c t

val both : 'a t -> 'b t -> ('a * 'b) t

val ignore : 'a t -> unit t

val all : 'a t list -> 'a list t

val all_unit : unit t list -> unit t

module List : Monad_intf.List with type 'a t := 'a t

val push_stack_frame :
     human_readable_description:(unit -> User_message.Style.t Pp.t)
  -> (unit -> 'a t)
  -> 'a t

(** Delay a static computation until the description is evaluated *)
val delayed : (unit -> 'a) -> 'a t

(** Always fail when executed. We pass a function rather than an exception to
    get a proper backtrace *)
val fail : fail -> _ t

(** [memoize name t] is an action builder that behaves like [t] except that its
    result is computed only once. *)
val memoize : string -> 'a t -> 'a t

type ('input, 'output) memo

(** Same as [Memo.create] but for [Action_builder] *)
val create_memo :
     string
  -> input:(module Memo.Input with type t = 'i)
  -> ?cutoff:('o -> 'o -> bool)
  -> ?human_readable_description:('i -> User_message.Style.t Pp.t)
  -> ('i -> 'o t)
  -> ('i, 'o) memo

(** Same as [Memo.exec] but for [Action_builder]'s memos *)
val exec_memo : ('i, 'o) memo -> 'i -> 'o t

(** [goal t] ignores all facts that have been accumulated about the dependencies
    of [t]. For example, [goal (path p)] declares that a path [p] contributes to
    the "goal" of the resulting action builder, which means [p] must be built,
    but the contents of [p] is irrelevant. *)
val goal : 'a t -> 'a t

module Expander : String_with_vars.Expander with type 'a app := 'a t

(** If you're thinking of using [Process.run] here, check that: (i) you don't in
    fact need [Command.run], and that (ii) [Process.run] only reads the declared
    build rule dependencies. *)
val memo_build : 'a Memo.Build.t -> 'a t

(** Like [memo_build] but collapses the two levels of [t]. *)
val memo_build_join : 'a t Memo.Build.t -> 'a t

(** If you're thinking of using [Process.run] here, check that: (i) you don't in
    fact need [Command.run], and that (ii) [Process.run] only reads the declared
    build rule dependencies. *)
val dyn_memo_build : 'a Memo.Build.t t -> 'a t

(** {1 Execution} *)

(** Evaluation mode.

    In [Lazy] mode, dependencies are only collected. In [Eager] mode,
    dependencies are build as soon as they are recorded and their facts are
    returned.

    If you want to both evaluate an action builder and build the collected
    dependencies, using [Eager] mode will increase parallelism. If you only want
    to know the set of dependencies, using [Lazy] will avoid unnecessary work. *)
type 'a eval_mode =
  | Lazy : unit eval_mode
  | Eager : Dep.Fact.t eval_mode

(** Execute an action builder. *)
val run : 'a t -> 'b eval_mode -> ('a * 'b Dep.Map.t) Memo.Build.t

(** {1 Low-level} *)

type 'a thunk = { f : 'm. 'm eval_mode -> ('a * 'm Dep.Map.t) Memo.Build.t }
[@@unboxed]

val of_thunk : 'a thunk -> 'a t

module Deps_or_facts : sig
  val union : 'a eval_mode -> 'a Dep.Map.t -> 'a Dep.Map.t -> 'a Dep.Map.t

  val union_all : 'a eval_mode -> 'a Dep.Map.t list -> 'a Dep.Map.t
end
