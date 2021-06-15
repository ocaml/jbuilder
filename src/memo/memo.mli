open! Stdune

type 'a build

module type Build = sig
  include Monad

  module List : sig
    val map : 'a list -> f:('a -> 'b t) -> 'b list t

    val concat_map : 'a list -> f:('a -> 'b list t) -> 'b list t
  end

  val memo_build : 'a build -> 'a t
end

(* This should eventually be just [Module Build : Build] *)
module Build : sig
  (** The build monad *)

  include Build with type 'a t = 'a build

  (* CR-someday amokhov: Return the set of exceptions explicitly. *)
  val run : 'a t -> 'a Fiber.t

  (** Every error gets reported twice: once early, in non-deterministic order,
      by calling [handler_error], and once later, in deterministic order, by
      raising a fiber exception.

      [handle_error_no_raise] must not raise exceptions, otherwise internal memo
      invariants get messed up and you get confusing errors like "Attempted to
      create a cached value based on some stale inputs".

      Nested calls of [run_with_error_handler] are not allowed.

      If multiple calls to [run_with_error_handler] happen concurrently
      (possibly with different error handlers), then each handler will correctly
      receive all errors it would be expected to receive if run independently.
      However, each error will only be sent "early" to one of the handlers,
      while the other handler will get this error delayed. If this limitation
      becomes problematic, it may be possible to lift it by eagerly bubbling up
      each error through individual dependency edges instead of sending errors
      directly to the handler in scope. *)
  val run_with_error_handler :
       'a t
    -> handle_error_no_raise:(Exn_with_backtrace.t -> unit Fiber.t)
    -> 'a Fiber.t

  (** [of_reproducible_fiber fiber] injects a fiber into the build monad. The
      given fiber must be "reproducible", i.e. executing it multiple times
      should always yield the same result. It is up to the caller to ensure that
      this property holds. If it doesn't, use [of_non_reproducible_fiber]. *)
  val of_reproducible_fiber : 'a Fiber.t -> 'a t

  (** [of_non_reproducible_fiber fiber] injects a fiber into the build monad.
      The fiber is considered to be "non-reproducible", i.e. it may return
      different values each time it is executed (for example, the current time),
      and it will therefore be re-executed on every build run. *)
  val of_non_reproducible_fiber : 'a Fiber.t -> 'a t

  val return : 'a -> 'a t

  (** Combine results of two computations executed in sequence. *)
  val both : 'a t -> 'b t -> ('a * 'b) t

  (** Combine results of two computations executed in parallel. Note that this
      function combines both successes and errors: if one of the computations
      fails, we let the other one run to completion, to give it a chance to
      raise its errors too. Regardless of the outcome (success or failure), the
      result will collect dependencies from both of the computations. All other
      parallel execution combinators have the same error/dependencies semantics. *)
  val fork_and_join : (unit -> 'a t) -> (unit -> 'b t) -> ('a * 'b) t

  val fork_and_join_unit : (unit -> unit t) -> (unit -> 'a t) -> 'a t

  (** This uses a sequential implementation. We use the short name to conform
      with the [Applicative] interface. See [all_concurrently] for the version
      with concurrency. *)
  val all : 'a t list -> 'a list t

  val all_concurrently : 'a t list -> 'a list t

  val if_ : bool -> (unit -> unit t) -> unit t

  val sequential_map : 'a list -> f:('a -> 'b t) -> 'b list t

  val sequential_iter : 'a list -> f:('a -> unit t) -> unit t

  val parallel_map : 'a list -> f:('a -> 'b t) -> 'b list t

  val parallel_iter : 'a list -> f:('a -> unit t) -> unit t

  val parallel_iter_set :
       (module Set.S with type elt = 'a and type t = 's)
    -> 's
    -> f:('a -> unit t)
    -> unit t

  module Make_map_traversals (Map : Map.S) : sig
    val parallel_iter : 'a Map.t -> f:(Map.key -> 'a -> unit t) -> unit t

    val parallel_map : 'a Map.t -> f:(Map.key -> 'a -> 'b t) -> 'b Map.t t
  end
  [@@inline always]

  module Option : sig
    val iter : 'a option -> f:('a -> unit t) -> unit t

    val map : 'a option -> f:('a -> 'b t) -> 'b option t

    val bind : 'a option -> f:('a -> 'b option t) -> 'b option t
  end

  module Result : sig
    val iter : ('a, _) result -> f:('a -> unit t) -> unit t
  end
end

type ('input, 'output) t

(** A stack frame within a computation. *)
module Stack_frame : sig
  type ('input, 'output) memo = ('input, 'output) t

  type t

  val to_dyn : t -> Dyn.t

  val name : t -> string option

  val input : t -> Dyn.t

  (** Checks if the stack frame is a frame of the given memoized function and if
      so, returns [Some i] where [i] is the argument of the function. *)
  val as_instance_of : t -> of_:('input, _) memo -> 'input option

  val human_readable_description : t -> User_message.Style.t Pp.t option
end

(** Errors raised by user-supplied memoized functions that have been augmented
    with Memo call stack information. *)
module Error : sig
  type t

  exception E of t

  (** Get the underlying exception.*)
  val get : t -> exn

  (** Return the stack leading to the node which raised the exception.*)
  val stack : t -> Stack_frame.t list
end

module Cycle_error : sig
  type t

  exception E of t

  (** Get the list of stack frames in this cycle. *)
  val get : t -> Stack_frame.t list
end

(* CR-someday amokhov: The current implementation memoizes all errors that are
   not wrapped into [Non_reproducible], which may be inconvenient in some cases,
   e.g. if a build action fails due to a spurious error, such as running out of
   memory. Right now, the only way to force such actions to be rebuilt is to
   restart Dune, which clears all memoized errors. In future, we would like to
   provide a way to rerun all failed actions without restarting the build, e.g.
   via a Dune RPC call. *)

(** Mark an exception as non-reproducible to indicate that it shouldn't be
    cached. *)
exception Non_reproducible of exn

(** [Invalidation] describes a set of nodes to be invalidated between
    memoization runs. These sets can be combined into larger sets to then be
    passed to [reset]. *)
module Invalidation : sig
  type ('input, 'output) memo = ('input, 'output) t

  type t

  val empty : t

  val combine : t -> t -> t

  val is_empty : t -> bool

  (** Clear all memoization tables. We use it if the incremental mode is not
      enabled. *)
  val clear_caches : t

  (** Invalidate all computations stored in a given [memo] table. *)
  val invalidate_cache : _ memo -> t

  val to_dyn : t -> Dyn.t
end

module Volatile : sig
  type 'a t

  val create : sample:(unit -> 'a Fiber.t) -> equal:('a -> 'a -> bool) -> 'a t

  val sample : 'a t -> 'a Build.t

  val check : _ t -> Invalidation.t Fiber.t
end

(** Notify the memoization system that the build system has restarted. This
    removes the values specified by [Invalidation.t] from the memoization cache,
    and advances the current run. *)
val reset : Invalidation.t -> unit

(** Returns [true] if the user enabled the incremental mode via the environment
    variable [DUNE_WATCHING_MODE_INCREMENTAL], and we should therefore assume
    that the build system tracks all relevant side effects in the [Build] monad. *)
val incremental_mode_enabled : bool ref

module type Input = sig
  type t

  include Table.Key with type t := t
end

module Store : sig
  module type Input = sig
    type t

    val to_dyn : t -> Dyn.t
  end

  module type S = sig
    type key

    type 'a t

    val create : unit -> _ t

    val clear : _ t -> unit

    val set : 'a t -> key -> 'a -> unit

    val find : 'a t -> key -> 'a option

    val iter : 'a t -> f:('a -> unit) -> unit
  end
end

(** [create name ~input ?cutoff f] creates a memoized version of the function
    [f : 'i -> 'o Build.t]. The result of [f] for a given input is cached, so
    that the second time [exec t x] is called, the previous result is re-used if
    possible.

    [exec t x] tracks what calls to other memoized functions [f x] performs.
    When the result of any of such dependent calls changes, [exec t x] will
    automatically recompute [f x].

    If the caller provides the [cutoff] equality check, we will use it to check
    if the function's output is the same as cached in the previous computation.
    If it's the same, we will be able to skip recomputing the functions that
    depend on it. Note: by default Dune wipes all memoization caches on every
    run, so this early cutoff optimisation is not effective. To override default
    behaviour, run Dune with the flag [DUNE_WATCHING_MODE_INCREMENTAL=true].

    If [human_readable_description] is passed, it will be used when displaying
    the memo stack to the user.

    Running the computation may raise [Memo.Cycle_error.E] if a dependency cycle
    is detected. *)
val create :
     string
  -> input:(module Input with type t = 'i)
  -> ?cutoff:('o -> 'o -> bool)
  -> ?human_readable_description:('i -> User_message.Style.t Pp.t)
  -> ('i -> 'o Build.t)
  -> ('i, 'o) t

(** Like [create] but accepts a custom [store] for memoization. This is useful
    when there is a custom data structure indexed by keys of type ['i] that is
    more efficient than the one that Memo uses by default (a plain hash table). *)
val create_with_store :
     string
  -> store:(module Store.S with type key = 'i)
  -> input:(module Store.Input with type t = 'i)
  -> ?cutoff:('o -> 'o -> bool)
  -> ?human_readable_description:('i -> User_message.Style.t Pp.t)
  -> ('i -> 'o Build.t)
  -> ('i, 'o) t

(** Execute a memoized function. *)
val exec : ('i, 'o) t -> 'i -> 'o Build.t

(** Print the memoized call stack during execution. This is useful for debugging
    purposes. *)
val dump_stack : unit -> unit Fiber.t

val pp_stack : unit -> _ Pp.t Fiber.t

(** Get the memoized call stack during the execution of a memoized function. *)
val get_call_stack : unit -> Stack_frame.t list Build.t

(** Insert a stack frame to make call stacks more precise when showing them to
    the user. *)
val push_stack_frame :
     human_readable_description:(unit -> User_message.Style.t Pp.t)
  -> (unit -> 'a Build.t)
  -> 'a Build.t

module Run : sig
  (** A single build run. *)
  type t

  module For_tests : sig
    val compare : t -> t -> Ordering.t

    val current : unit -> t
  end
end

(** Introduces a dependency on the current build run. *)
val current_run : unit -> Run.t Build.t

module Lazy : sig
  type 'a t

  val of_val : 'a -> 'a t

  val create :
       ?cutoff:('a -> 'a -> bool)
    -> ?name:string
    -> ?human_readable_description:(unit -> User_message.Style.t Pp.t)
    -> (unit -> 'a Build.t)
    -> 'a t

  val force : 'a t -> 'a Build.t

  val map : 'a t -> f:('a -> 'b) -> 'b t
end

val lazy_ :
     ?cutoff:('a -> 'a -> bool)
  -> ?name:string
  -> ?human_readable_description:(unit -> User_message.Style.t Pp.t)
  -> (unit -> 'a Build.t)
  -> 'a Lazy.t

module Implicit_output : sig
  type 'o t

  (** [produce] and [produce_opt] are used by effectful functions to produce
      output. *)
  val produce : 'o t -> 'o -> unit Build.t

  val produce_opt : 'o t -> 'o option -> unit Build.t

  (** [collect] and [forbid] take a potentially effectful function (one which
      may produce some implicit output) and turn it into a pure one (with
      explicit output if any). *)
  val collect : 'o t -> (unit -> 'a Build.t) -> ('a * 'o option) Build.t

  val forbid : (unit -> 'a Build.t) -> 'a Build.t

  module type Implicit_output = sig
    type t

    val name : string

    val union : t -> t -> t
  end

  (** Register a new type of implicit output. *)
  val add : (module Implicit_output with type t = 'o) -> 'o t
end

module With_implicit_output : sig
  type ('i, 'o) t

  val create :
       string
    -> input:(module Input with type t = 'i)
    -> implicit_output:'io Implicit_output.t
    -> ('i -> 'o Build.t)
    -> ('i, 'o) t

  val exec : ('i, 'o) t -> 'i -> 'o Build.t
end

module Cell : sig
  type ('i, 'o) t

  val input : ('i, _) t -> 'i

  val read : (_, 'o) t -> 'o Build.t

  (** Mark this cell as invalid, forcing recomputation of this value. The
      consumers may be recomputed or not, depending on early cutoff. *)
  val invalidate : _ t -> Invalidation.t
end

(** Create a "memoization cell" that focuses on a single input/output pair of a
    memoized function. *)
val cell : ('i, 'o) t -> 'i -> ('i, 'o) Cell.t

val lazy_cell :
     ?cutoff:('a -> 'a -> bool)
  -> ?name:string
  -> ?human_readable_description:(unit -> User_message.Style.t Pp.t)
  -> (unit -> 'a Build.t)
  -> (unit, 'a) Cell.t

(** Memoization of polymorphic functions ['a input -> 'a output Build.t]. The
    provided [id] function must be injective, i.e. there must be a one-to-one
    correspondence between [input]s and their [id]s. *)
module Poly (Function : sig
  type 'a input

  type 'a output

  val name : string

  val eval : 'a input -> 'a output Build.t

  val to_dyn : _ input -> Dyn.t

  val id : 'a input -> 'a Type_eq.Id.t
end) : sig
  val eval : 'a Function.input -> 'a Function.output Build.t
end

(** Diagnostics features that affect performance but are useful for debugging. *)
module Debug : sig
  (** If [true], Memo will record the location of [Lazy.t] values. *)
  val track_locations_of_lazy_values : bool ref

  (** If [true], Memo will perform additional checks of internal invariants. *)
  val check_invariants : bool ref
end

(** Various performance counters. Reset to zero at the start of every run. *)
module Perf_counters : sig
  (** This function must be called to enable performance counters. *)
  val enable : unit -> unit

  (** Number of nodes visited in the current run. *)
  val nodes_in_current_run : unit -> int

  (** Number of dependency edges of the nodes visited in the current run. *)
  val edges_in_current_run : unit -> int

  (** Number of nodes that were (re)computed in the current run. This number
      cannot not exceed [nodes_in_current_run]. *)
  val nodes_computed_in_current_run : unit -> int

  (** Number of edges that were traversed in the current run. Some edges may be
      traversed twice, so this number can exceed [edges_in_current_run]. *)
  val edges_traversed_in_current_run : unit -> int

  (** A concise summary of performance counters. *)
  val report_for_current_run : unit -> string

  (** Raise if any internal invariants are violated. *)
  val assert_invariants : unit -> unit

  (** Reset the counters to zero. You typically don't need to call this function
      directly (as counters are reset on every run) but it's useful for tests. *)
  val reset : unit -> unit
end

module Expert : sig
  (** Like [cell] but returns [Nothing] if the given memoized function has never
      been evaluated on the specified input. We use [previously_evaluated_cell]
      to skip unnecessary rebuilds when receiving file system events for files
      that we don't care about.

      Note that this function is monotonic: its result can change from [Nothing]
      to [Some cell] as new cells get evaluated. However, calling [reset] clears
      all memoization tables, and therefore resets [previously_evaluated_cell]
      to [Nothing] as well. *)
  val previously_evaluated_cell : ('i, 'o) t -> 'i -> ('i, 'o) Cell.t option
end

module For_tests : sig
  (** After executing a memoized function with a given name and input, it is
      possible to query which dependencies that function used during execution
      by calling [get_deps] with the name and input used during execution.

      Returns [None] if the dependencies were not computed yet. *)
  val get_deps : ('i, _) t -> 'i -> (string option * Dyn.t) list option

  (** Forget all memoized values, forcing them to be recomputed on the next
      build run. *)
  val clear_memoization_caches : unit -> unit
end

val yield_if_there_are_pending_events : (unit -> unit Fiber.t) ref
