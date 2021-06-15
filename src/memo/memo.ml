open! Stdune
open Fiber.O

module Debug = struct
  let track_locations_of_lazy_values = ref false

  let check_invariants = ref false
end

module Counters = struct
  let enabled = ref false

  let nodes_considered = ref 0

  let edges_considered = ref 0

  let nodes_computed = ref 0

  let edges_traversed = ref 0

  let reset () =
    nodes_considered := 0;
    edges_considered := 0;
    nodes_computed := 0;
    edges_traversed := 0

  let record_newly_considered_node ~edges ~computed =
    incr nodes_considered;
    edges_considered := !edges_considered + edges;
    if computed then incr nodes_computed

  let record_new_edge_traversals ~count =
    edges_traversed := !edges_traversed + count
end

type 'a build = 'a Fiber.t

module type Build = sig
  include Monad

  module List : sig
    val map : 'a list -> f:('a -> 'b t) -> 'b list t

    val concat_map : 'a list -> f:('a -> 'b list t) -> 'b list t
  end

  val memo_build : 'a build -> 'a t
end

module Build0 = struct
  include Fiber

  let if_ x y =
    match x with
    | true -> y ()
    | false -> return ()

  let of_reproducible_fiber = Fun.id

  module Option = struct
    let iter option ~f =
      match option with
      | None -> return ()
      | Some a -> f a

    let map option ~f =
      match option with
      | None -> return None
      | Some a -> f a >>| Option.some

    let bind option ~f =
      match option with
      | None -> return None
      | Some a -> f a
  end

  module Result = struct
    let iter result ~f =
      match result with
      | Error _ -> return ()
      | Ok a -> f a
  end

  module List = struct
    let map = parallel_map

    let concat_map l ~f = map l ~f >>| List.concat
  end

  let memo_build = Fun.id
end

module Allow_cutoff = struct
  type 'o t =
    | No
    | Yes of ('o -> 'o -> bool)
end

module type Input = sig
  type t

  include Table.Key with type t := t
end

module Spec = struct
  type ('i, 'o) t =
    { name : string option
    ; (* If the field [witness] precedes any of the functional values ([input]
         and [f]), then polymorphic comparison actually works for [Spec.t]s. *)
      witness : 'i Type_eq.Id.t
    ; input : (module Store_intf.Input with type t = 'i)
    ; allow_cutoff : 'o Allow_cutoff.t
    ; f : 'i -> 'o Fiber.t
    ; human_readable_description : ('i -> User_message.Style.t Pp.t) option
    }

  let create ~name ~input ~human_readable_description ~cutoff f =
    let name =
      match name with
      | None when !Debug.track_locations_of_lazy_values ->
        Option.map
          (Caller_id.get ~skip:[ __FILE__ ])
          ~f:(fun loc ->
            sprintf "lazy value created at %s" (Loc.to_file_colon_line loc))
      | _ -> name
    in
    let allow_cutoff =
      match cutoff with
      | None -> Allow_cutoff.No
      | Some equal -> Yes equal
    in
    { name
    ; input
    ; allow_cutoff
    ; witness = Type_eq.Id.create ()
    ; f
    ; human_readable_description
    }
end

module Id = Id.Make ()

(* We can get rid of this once we use the memoization system more pervasively
   and all the dependencies are properly specified *)
module Caches = struct
  let cleaners = ref []

  let register ~clear = cleaners := clear :: !cleaners

  let clear () = List.iter !cleaners ~f:(fun f -> f ())
end

module Dep_node_without_state = struct
  type ('i, 'o) t =
    { id : Id.t
          (* If [id] is placed first in this data structure, then polymorphic
             comparison for dep nodes works fine regardless of the other fields.
             At the moment polymorphic comparison is used for [Exn_set], but we
             hope to change that. *)
    ; spec : ('i, 'o) Spec.t
    ; input : 'i
    }

  type packed = T : (_, _) t -> packed [@@unboxed]
end

let ser_input (type i) (node : (i, _) Dep_node_without_state.t) =
  let (module Input : Store_intf.Input with type t = i) = node.spec.input in
  Input.to_dyn node.input

module Stack_frame_without_state = struct
  open Dep_node_without_state

  type t = Dep_node_without_state.packed

  let name (T t) = t.spec.name

  let input (T t) = ser_input t

  let to_dyn t =
    Dyn.Tuple
      [ String
          (match name t with
          | Some name -> name
          | None -> "<unnamed>")
      ; input t
      ]

  let id (T a) = a.id

  let equal (T a) (T b) = Id.equal a.id b.id
end

module Cycle_error = struct
  type t = Stack_frame_without_state.t list

  exception E of t

  let get t = t

  let to_dyn = Dyn.Encoder.list Stack_frame_without_state.to_dyn
end

module Error = struct
  type t =
    { exn : exn
    ; rev_stack : Stack_frame_without_state.t list
    }

  exception E of t

  let rotate_cycle ~is_desired_head cycle =
    match
      List.split_while cycle ~f:(fun elem -> not (is_desired_head elem))
    with
    | _, [] -> None
    | prefix, suffix -> Some (suffix @ prefix)

  let shorten_stack_leading_to_cycle ~rev_stack cycle =
    let ids_in_cycle =
      List.map cycle ~f:Stack_frame_without_state.id |> Id.Set.of_list
    in
    match
      List.split_while
        ~f:(fun frame ->
          not (Id.Set.mem ids_in_cycle (Stack_frame_without_state.id frame)))
        rev_stack
    with
    | rev_stack, [] -> (rev_stack, cycle)
    | rev_stack, node_in_cycle :: _ ->
      let cycle =
        rotate_cycle
          ~is_desired_head:(Stack_frame_without_state.equal node_in_cycle)
          (List.rev cycle)
        |> Option.value_exn |> List.rev
      in
      (rev_stack, cycle)

  let get_exn_and_stack t =
    match t.exn with
    | Cycle_error.E cycle ->
      let rev_stack, cycle =
        shorten_stack_leading_to_cycle ~rev_stack:t.rev_stack cycle
      in
      (Cycle_error.E cycle, List.rev rev_stack)
    | exn -> (exn, List.rev t.rev_stack)

  let get t = fst (get_exn_and_stack t)

  let stack t = snd (get_exn_and_stack t)

  let extend_stack exn ~stack_frame =
    E
      (match exn with
      | E t -> { t with rev_stack = stack_frame :: t.rev_stack }
      | _ -> { exn; rev_stack = [ stack_frame ] })

  let to_dyn t =
    let open Dyn.Encoder in
    record
      [ ("exn", Exn.to_dyn t.exn)
      ; ("stack", Dyn.Encoder.list Stack_frame_without_state.to_dyn (stack t))
      ]
end

(* The user can wrap exceptions into the [Non_reproducible] constructor to tell
   Memo that they shouldn't be cached. We will catch them, unwrap, and re-raise
   without the wrapper. *)
exception Non_reproducible of exn

let () =
  Printexc.register_printer (fun exn ->
      let dyn =
        let open Dyn.Encoder in
        match exn with
        | Error.E err -> Some (constr "Memo.Error.E" [ Error.to_dyn err ])
        | Cycle_error.E frames ->
          Some (constr "Cycle_error.E" [ Cycle_error.to_dyn frames ])
        | Non_reproducible exn ->
          Some (constr "Memo.Non_reproducible" [ Exn.to_dyn exn ])
        | _ -> None
      in
      Option.map dyn ~f:Dyn.to_string)

module Exn_comparable = Comparable.Make (struct
  type t = Exn_with_backtrace.t

  let unwrap = function
    | Error.E { exn; _ } -> exn
    | exn -> exn

  let compare { Exn_with_backtrace.exn; backtrace = _ } (t : t) =
    Poly.compare (unwrap exn) (unwrap t.exn)

  let to_dyn = Exn_with_backtrace.to_dyn
end)

module Exn_set = Exn_comparable.Set

module Collect_errors_monoid = struct
  module T = struct
    type t =
      { exns : Exn_set.t
      ; reproducible : bool
      }

    let empty = { exns = Exn_set.empty; reproducible = true }

    let combine { exns = exns1; reproducible = reproducible1 }
        { exns = exns2; reproducible = reproducible2 } =
      { exns = Exn_set.union exns1 exns2
      ; reproducible = reproducible1 && reproducible2
      }
  end

  include T
  include Monoid.Make (T)
end

(* A value calculated during a "sample attempt". A sample attempt can fail for
   two reasons:

   - [Error]: the user-supplied function that was called to compute the value
   raised one or more exceptions recorded in the [Collect_errors_monoid.t].

   - [Cancelled]: the attempt was cancelled due to a dependency cycle.

   Note that we plan to make [Cancelled] more general and store the reason for
   cancellation: a dependency cycle or a request to cancel the current run. *)
module Value = struct
  type 'a t =
    | Ok of 'a
    | Error of Collect_errors_monoid.t
    | Cancelled of { dependency_cycle : Cycle_error.t }

  let get_exn t ~stack_frame =
    match t with
    | Ok a -> Fiber.return a
    | Error { exns; _ } ->
      Fiber.reraise_all
        (Exn_set.to_list_map exns ~f:(fun exn ->
             { exn with exn = Error.extend_stack exn.exn ~stack_frame }))
    | Cancelled { dependency_cycle } -> raise (Cycle_error.E dependency_cycle)
end

module Dag : Dag.S with type value := Dep_node_without_state.packed =
Dag.Make (struct
  type t = Dep_node_without_state.packed
end)

(* Dependencies of a value accumulated so far. All of them are added to the DAG
   of sample attempts to detect cycles.

   We start accumulating dependencies during the [restore_from_cache] step. Some
   of them may turn out to be "phantom", i.e. we used to depend on them but do
   not depend on them anymore. This happens when the [restore_from_cache] step
   fails and we fall back to recomputing the value, which can lead to a new set
   of dependencies.

   Once we move to the [compute] step, we start accumulating actual dependencies
   of the value in [compute_deps]. The list of dependencies is stored in the
   reversed order for efficient updates.

   After the [compute] step has finished, [compute_deps] are stored in the
   resulting [Cached_value.t]. *)
module Deps_so_far = struct
  type 'node deps =
    | Compute_not_started
    | Compute_started of { deps_reversed : 'node list }

  (* Some of the [added_to_dag] nodes also need to be added to [compute_deps]. *)
  type status = { added_to_compute_deps : bool } [@@unboxed]

  type 'node t =
    { added_to_dag : status Id.Table.t
    ; mutable compute_deps : 'node deps
    }

  let create () =
    { added_to_dag = Id.Table.create 4; compute_deps = Compute_not_started }

  let start_compute t = t.compute_deps <- Compute_started { deps_reversed = [] }

  (* Add a new dependency [node] to [added_to_dag] and also to [compute_deps] if
     [Compute_started] and the dependency hasn't been added before. *)
  let add_dep t node_id node =
    match t.compute_deps with
    | Compute_not_started ->
      Id.Table.set t.added_to_dag node_id { added_to_compute_deps = false }
    | Compute_started { deps_reversed } -> (
      match Id.Table.find t.added_to_dag node_id with
      | Some { added_to_compute_deps = true } -> ()
      | None
      | Some { added_to_compute_deps = false } ->
        t.compute_deps <-
          Compute_started { deps_reversed = node :: deps_reversed };
        Id.Table.set t.added_to_dag node_id { added_to_compute_deps = true })

  let get_compute_deps_rev t =
    match t.compute_deps with
    | Compute_not_started ->
      Code_error.raise
        "get_compute_deps_rev called in the Compute_not_started state" []
    | Compute_started { deps_reversed } -> deps_reversed
end

module Cache_lookup = struct
  (* Looking up a value cached in a previous run can fail in three possible
     ways:

     - [Not_found]: either the value has never been computed before, or the last
     computation attempt failed.

     - [Out_of_date]: we found a value computed in a previous run but it is out
     of date because one of its dependencies changed; we return the old value so
     that it can be compared with a new one to support the early cutoff.

     - [Cancelled _]: the cache lookup attempt has been cancelled because of a
     dependency cycle. This outcome indicates that a dependency cycle has been
     introduced in the current run. If a cycle existed in a previous run, the
     outcome would have been [Not_found] instead. *)
  module Failure = struct
    type 'a t =
      | Not_found
      | Out_of_date of 'a
      | Cancelled of { dependency_cycle : Cycle_error.t }
  end

  module Result = struct
    type 'a t =
      | Ok of 'a
      | Failure of 'a Failure.t
  end
end

(* A fiber that can be shared but is still computed at most once. An equivalent
   of ['a Lazy.t] but for asynchronous computations. *)
module Once = struct
  type 'a state =
    | Not_forced of { must_not_raise : unit -> 'a Fiber.t }
    | Forced of 'a Fiber.Ivar.t

  type 'a t = { mutable state : 'a state }

  (* If a given thunk does in fact raise an exception, forcing it will propagate
     the exception to the first caller, and leave all subsequent callers stuck,
     forever waiting for the unfilled [Ivar.t]. *)
  let create ~must_not_raise = { state = Not_forced { must_not_raise } }

  (* Note that side effects of the shared fiber will happen only when the fiber
     returned by [force t] is executed. *)
  let force t =
    Fiber.of_thunk (fun () ->
        match t.state with
        | Forced ivar ->
          let+ res = Fiber.Ivar.read ivar in
          res
        | Not_forced { must_not_raise } ->
          let ivar = Fiber.Ivar.create () in
          t.state <- Forced ivar;
          let* result = must_not_raise () in
          let+ () = Fiber.Ivar.fill ivar result in
          result)

  (* Like a monadic bind but [f_must_not_raise] returns a [Fiber.t], not a [t]. *)
  let and_then t ~f_must_not_raise =
    create ~must_not_raise:(fun () -> Fiber.bind (force t) ~f:f_must_not_raise)
end

(* An attempt to sample the current value of a node. It's an "attempt" because
   it can fail due to a dependency cycle.

   A sample attempt begins its life in the [Running] state. Multiple readers can
   concurrently subscribe to the (possibly future) [result] of the attempt using
   the [restore] and [compute] functions. If the attempt succeeds, it goes to
   the [Finished] state.

   To detect dependency cycles, we maintain a DAG of [Running] sample attempts.
   [Finished] attempts do not need to be in the DAG but currently they remain
   there until the end of the current run. When the run completes, the DAG is
   garbage collected because we no longer hold any references to its nodes. *)
module Sample_attempt = struct
  module Result = struct
    type 'a t =
      { restore_from_cache : 'a Cache_lookup.Result.t Once.t
      ; compute : 'a Once.t
      }
  end

  type 'a t =
    | Finished of 'a
    | Running of
        { dag_node : Dag.node
        ; result : 'a Result.t
        }

  let restore = function
    | Finished cached_value ->
      Fiber.return (Cache_lookup.Result.Ok cached_value)
    | Running { result; _ } -> Once.force result.restore_from_cache

  let compute = function
    | Finished cached_value -> Fiber.return cached_value
    | Running { result; _ } -> Once.force result.compute
end

(* Checking dependencies of a node can lead to one of these outcomes:

   - [Unchanged]: all the dependencies of the current node are up to date and we
   can therefore skip recomputing the node and can reuse the value computed in
   the previous run.

   - [Changed]: one of the dependencies has changed since the previous run and
   the current node should therefore be recomputed.

   - [Cancelled _]: one of the dependencies leads to a dependency cycle. In this
   case, there is no point in recomputing the current node: it's impossible to
   bring its dependencies up to date! *)
module Changed_or_not = struct
  type t =
    | Unchanged
    | Changed
    | Cancelled of { dependency_cycle : Cycle_error.t }
end

module M = struct
  (* A [value] along with some additional information that allows us to check
     whether the it is up to date or needs to be recomputed. *)
  module rec Cached_value : sig
    type 'o t =
      { value : 'o Value.t
      ; (* We store [last_changed_at] and [last_validated_at] for early cutoff.
           See Section 5.2.2 of "Build Systems a la Carte: Theory and Practice"
           for more details (https://doi.org/10.1017/S0956796820000088).

           - [last_changed_at] is the run when the value changed last time.

           - [last_validated_at] is the run when the value was last confirmed as
           up to date. Invariant: [last_changed_at <= last_validated_at].

           Consider a dependency [dep] of a node [caller].

           If [dep.last_changed_at > caller.last_validated_at], then the [dep]'s
           value has changed since it had been previously used by the [caller]
           and therefore the [caller] needs to be recomputed. *)
        last_changed_at : Run.t
      ; mutable last_validated_at : Run.t
      ; (* The list of dependencies [deps], as captured at [last_validated_at].
           Note that the list of dependencies can change over the lifetime of
           [Cached_value]: this happens if the value gets re-computed but is
           declared unchanged by the cutoff check.

           Note that [deps] should be listed in the order in which they were
           depended on to avoid recomputations of the dependencies that are no
           longer relevant (see an example below). Asynchronous functions induce
           a partial (rather than a total) order on dependencies, and so [deps]
           should be a linearisation of this partial order. It is also worth
           noting that the problem only occurs with dynamic dependencies,
           because static dependencies can never become irrelevant.

           As an example, consider the function [let f x = let y = g x in h y].
           The correct order of dependencies of [f 0] is [g 0] and then [h y1],
           where [y1] is the result of computing [g 0] in the first build run.
           Now consider the situation where (i) [h y1] is incorrectly listed
           first in [deps], and (ii) both [g] and [h] have changed in the second
           build run (e.g. because they read modified files). To determine that
           [f] needs to be recomputed, we start by recomputing [h y1], which is
           likely to be a waste because now we are really interested in [h y2],
           where [y2] is the result of computing [g 0] in the second run. Had we
           listed [g 0] first, we would recompute it and the work wouldn't be
           wasted since [f 0] does depend on it.

           Another important reason to list [deps] according to a linearisation
           of the dependency order is to eliminate spurious dependency cycles. *)
        mutable deps : Deps.t
      }
  end =
    Cached_value

  and Deps : sig
    type t

    val create : deps_rev:Dep_node.packed list -> t

    val empty : t

    val length : t -> int

    val to_list : t -> Dep_node.packed list

    val changed_or_not :
         t
      -> f:(Dep_node.packed -> Changed_or_not.t Fiber.t)
      -> Changed_or_not.t Fiber.t
  end = struct
    (* The array is stored reversed to avoid reversing the list in [create]. We
       need to be careful about traversing the array in the right order in the
       functions [to_list] and [changed_or_not]. *)
    type t = Dep_node.packed array

    let create ~deps_rev = Array.of_list deps_rev

    let empty = Array.init 0 ~f:(fun _ -> assert false)

    let length = Array.length

    let to_list = Array.fold_left ~init:[] ~f:(fun acc x -> x :: acc)

    let changed_or_not t ~f =
      let rec go index =
        if index < 0 then
          Fiber.return Changed_or_not.Unchanged
        else
          f t.(index) >>= function
          | Changed_or_not.Unchanged -> go (index - 1)
          | (Changed | Cancelled _) as res -> Fiber.return res
      in
      go (Array.length t - 1)
  end

  and Running_state : sig
    type t =
      { deps_so_far : Dep_node.packed Deps_so_far.t
      ; dag_node : Dag.node
      }
  end =
    Running_state

  (* Why do we store a [run] in the [Considering] state?

     It is possible for a computation to remain in the [Considering] state after
     the current run is complete. This happens if the [restore_from_cache] step
     fails, and the parent node recreates all dependencies from scratch in the
     subsequent [compute] step. We call computations that become stuck in the
     [Considering] state "stale computations".

     To distinguish between "current" and "stale" computations, we store the
     [run] in which the computation had started. In this way, before subscribing
     to a [sample_attempt_result], we can check if it corresponds to the current
     run, and if not, restart the sample attempt from scratch. This is what the
     function [currently_considering] does.

     Once all stale computations have been restarted, we should hold no more
     references to the corresponding [sample_attempt_result]s, allowing them to
     be garbage collected. Note: some stale computations may never be restarted,
     e.g. if they end up getting forever stuck behind an inactive conditional. *)
  and State : sig
    type 'o t =
      (* [Considering] marks computations currently being considered, i.e. whose
         result we currently attempt to restore from the cache or recompute. *)
      | Not_considering
      | Considering of
          { run : Run.t
          ; running : Running_state.t
          ; sample_attempt_result : 'o Cached_value.t Sample_attempt.Result.t
          }
  end =
    State

  and Dep_node : sig
    type ('i, 'o) t =
      { without_state : ('i, 'o) Dep_node_without_state.t
      ; mutable state : 'o State.t
      ; mutable last_cached_value : 'o Cached_value.t option
      ; (* This field caches the value of [without_state.spec.allow_cutoff] to
           avoid jumping through two more pointers in a tight loop. *)
        has_cutoff : bool
      }

    type packed = T : (_, _) t -> packed [@@unboxed]
  end =
    Dep_node
end

module State = M.State
module Running_state = M.Running_state
module Dep_node = M.Dep_node
module Deps = M.Deps

module Stack_frame_with_state = struct
  type ('i, 'o) unpacked =
    { without_state : ('i, 'o) Dep_node_without_state.t
    ; running_state : Running_state.t
    }

  type t = T : ('i, 'o) unpacked -> t

  let to_dyn (T t) = Stack_frame_without_state.to_dyn (T t.without_state)
end

module To_open = struct
  module Stack_frame = Stack_frame_with_state
end

open To_open

let global_dep_dag = Dag.create ()

module Call_stack = struct
  type t = Stack_frame_with_state.t list

  (* The variable holding the call stack for the current context. *)
  let call_stack_var : t Fiber.Var.t = Fiber.Var.create ()

  let get_call_stack () =
    Fiber.Var.get call_stack_var >>| Option.value ~default:[]

  let get_call_stack_without_state () =
    get_call_stack ()
    >>| List.map ~f:(fun (Stack_frame_with_state.T t) ->
            Dep_node_without_state.T t.without_state)

  let get_call_stack_tip () = get_call_stack () >>| List.hd_opt

  let push_frame (frame : Stack_frame_with_state.t) f =
    let* stack = get_call_stack () in
    let stack = frame :: stack in
    Fiber.Var.set call_stack_var stack (fun () -> Implicit_output.forbid f)
end

module Error_handler : sig
  val is_set : bool Fiber.t

  val report_error : Exn_with_backtrace.t -> unit Fiber.t

  val with_error_handler :
    (Exn_with_backtrace.t -> unit Fiber.t) -> (unit -> 'a Fiber.t) -> 'a Fiber.t
end = struct
  type t = Exn_with_backtrace.t -> unit Fiber.t

  let var : t Fiber.Var.t = Fiber.Var.create ()

  let is_set = Fiber.map (Fiber.Var.get var) ~f:Option.is_some

  let get_exn = Fiber.Var.get_exn var

  let report_error error =
    let open Fiber.O in
    let* handler = get_exn in
    let* stack = Call_stack.get_call_stack_without_state () in
    let error =
      Exn_with_backtrace.map error ~f:(fun exn ->
          List.fold_left stack ~init:exn ~f:(fun exn stack_frame ->
              Error.extend_stack exn ~stack_frame))
    in
    Fiber.map
      (Fiber.collect_errors (fun () -> handler error))
      ~f:(function
        | Ok () -> ()
        | Error e ->
          (* Unfortunately, by re-raising an exception here we're violating some
             Memo invariants and causing more confusing exceptions, but
             hopefully this code_error will be a hint. *)
          Code_error.raise "Memo error handler raised an exception"
            [ ("exns", Dyn.Encoder.list Exn_with_backtrace.to_dyn e) ])

  let deduplicate_errors f =
    let reported = ref Exn_set.empty in
    fun exn ->
      if Exn_set.mem !reported exn then
        Fiber.return ()
      else (
        reported := Exn_set.add !reported exn;
        f exn
      )

  let with_error_handler t f =
    Fiber.of_thunk (fun () ->
        (* [with_error_handler] runs once for every incremental run, so calling
           [deduplicate_errors] afresh here makes sure that we re-report all
           errors*)
        let t = deduplicate_errors t in
        Fiber.bind (Fiber.Var.get var) ~f:(function
          | None -> Fiber.Var.set var t f
          | Some _handler ->
            Code_error.raise
              "Memo.run_with_error_handler: an error handler is already \
               installed"
              []))
end

let pp_stack () =
  let open Pp.O in
  let+ stack = Call_stack.get_call_stack () in
  Pp.vbox
    (Pp.box (Pp.text "Memoized function stack:")
    ++ Pp.cut
    ++ Pp.chain stack ~f:(fun frame -> Dyn.pp (Stack_frame.to_dyn frame)))

let dump_stack () =
  let+ pp = pp_stack () in
  Console.print [ pp ]

let get_cached_value_in_current_cycle (dep_node : _ Dep_node.t) =
  match dep_node.last_cached_value with
  | None -> None
  | Some cv ->
    if Run.is_current cv.last_validated_at then
      Some cv
    else
      None

module Cached_value = struct
  include M.Cached_value

  let capture_deps ~deps_rev =
    if !Debug.check_invariants then
      List.iter deps_rev ~f:(function Dep_node.T dep_node ->
          (match get_cached_value_in_current_cycle dep_node with
          | None ->
            let reason =
              match dep_node.last_cached_value with
              | None -> "(no value)"
              | Some _ -> "(old run)"
            in
            Code_error.raise
              ("Attempted to create a cached value based on some stale inputs "
             ^ reason)
              []
          | Some _up_to_date_cached_value -> ()));
    Deps.create ~deps_rev

  let create x ~deps_rev =
    { value = x
    ; last_changed_at = Run.current ()
    ; last_validated_at = Run.current ()
    ; deps = capture_deps ~deps_rev
    }

  (* Dependencies of cancelled computations are not accurate, so we store the
     empty list of [deps] in this case. In future, it would be better to
     refactor the code to avoid storing the list altogether in this case. *)
  let create_cancelled ~dependency_cycle =
    { value = Cancelled { dependency_cycle }
    ; last_changed_at = Run.current ()
    ; last_validated_at = Run.current ()
    ; deps = Deps.empty
    }

  let confirm_old_value t ~deps_rev =
    t.last_validated_at <- Run.current ();
    t.deps <- capture_deps ~deps_rev;
    t

  let value_changed (node : _ Dep_node.t) prev_value cur_value =
    match ((prev_value : _ Value.t), (cur_value : _ Value.t)) with
    | (Cancelled _ | Error { reproducible = false; _ }), _
    | _, (Cancelled _ | Error { reproducible = false; _ })
    | Error _, Ok _
    | Ok _, Error _ ->
      true
    | Ok prev_value, Ok cur_value -> (
      match node.without_state.spec.allow_cutoff with
      | Yes equal -> not (equal prev_value cur_value)
      | No -> true)
    | ( Error { exns = prev_exns; reproducible = true }
      , Error { exns = cur_exns; reproducible = true } ) ->
      not (Exn_set.equal prev_exns cur_exns)
end

(* Add a dependency on the [dep_node] from the caller, if there is one. Returns
   an [Error] if the new dependency would introduce a dependency cycle. *)
let add_dep_from_caller (type i o) (dep_node : (i, o) Dep_node.t)
    (sample_attempt : _ Sample_attempt.t) =
  let+ caller = Call_stack.get_call_stack_tip () in
  (* Not counting the above computation of the [caller] towards cycle detection,
     to avoid inserting an extra Fiber map or bind. *)
  match caller with
  | None -> Ok ()
  | Some (Stack_frame_with_state.T caller) -> (
    let deps_so_far_of_caller = caller.running_state.deps_so_far in
    match
      Id.Table.mem deps_so_far_of_caller.added_to_dag dep_node.without_state.id
    with
    | true ->
      Deps_so_far.add_dep deps_so_far_of_caller dep_node.without_state.id
        (Dep_node.T dep_node);
      Ok ()
    | false -> (
      let cycle_error =
        match sample_attempt with
        | Finished _ -> None
        | Running { dag_node; _ } -> (
          match
            Dag.add_assuming_missing global_dep_dag
              caller.running_state.dag_node dag_node
          with
          | () -> None
          | exception Dag.Cycle cycle ->
            Some (List.map cycle ~f:(fun dag_node -> dag_node.Dag.data)))
      in
      match cycle_error with
      | None ->
        Deps_so_far.add_dep deps_so_far_of_caller dep_node.without_state.id
          (Dep_node.T dep_node);
        Ok ()
      | Some cycle -> Error cycle))

type ('input, 'output) t =
  { spec : ('input, 'output) Spec.t
  ; cache : ('input, ('input, 'output) Dep_node.t) Store.t
  }

module Stack_frame = struct
  type ('input, 'output) memo = ('input, 'output) t

  include Stack_frame_without_state

  let as_instance_of (type i) (Dep_node_without_state.T t)
      ~of_:(memo : (i, _) memo) : i option =
    match Type_eq.Id.same memo.spec.witness t.spec.witness with
    | Some Type_eq.T -> Some t.input
    | None -> None

  let human_readable_description (Dep_node_without_state.T t) =
    Option.map t.spec.human_readable_description ~f:(fun f -> f t.input)
end

(* There are two approaches to invalidating memoization nodes. Currently, when a
   node is invalidated by calling [invalidate_dep_node], only the node itself is
   marked as "changed" (by setting [node.last_cached_value] to [None]). Then,
   the whole graph is marked as "possibly changed" by calling [Run.restart ()],
   which in O(1) time makes all [last_validated_at : Run.t] values out of date.
   In the subsequent computation phase, the whole graph is traversed from top to
   bottom to discover "actual changes" and recompute all the nodes affected by
   these changes. One disadvantage of this approach is that the whole graph
   needs to be traversed even if only a small part of it depends on the set of
   invalidated nodes.

   An alternative approach is as follows. Whenever the [invalidate_dep_node]
   function is called, we recursively mark all of its reverse dependencies as
   "possibly changed". Then, in the computation phase, we only need to traverse
   the marked part of graph (instead of the whole graph as we do currently). One
   disadvantage of this approach is that every node needs to store a list of its
   reverse dependencies, which introduces cyclic memory references and
   complicates garbage collection.

   Is it worth switching from the current approach to the alternative? It's best
   to answer this question by benchmarking. This is not urgent but is worth
   documenting in the code. *)
let invalidate_dep_node (node : _ Dep_node.t) = node.last_cached_value <- None

let invalidate_store = Store.iter ~f:invalidate_dep_node

let create_with_cache (type i o) name ~cache ~input ~cutoff
    ~human_readable_description (f : i -> o Fiber.t) =
  let spec =
    Spec.create ~name:(Some name) ~input ~cutoff ~human_readable_description f
  in
  Caches.register ~clear:(fun () ->
      Store.clear cache;
      invalidate_store cache);
  { cache; spec }

let create_with_store (type i) name
    ~store:(module S : Store_intf.S with type key = i) ~input ?cutoff
    ?human_readable_description f =
  let cache = Store.make (module S) in
  create_with_cache name ~cache ~input ~cutoff ~human_readable_description f

let create (type i) name ~input:(module Input : Input with type t = i) ?cutoff
    ?human_readable_description f =
  (* This mutable table is safe: the implementation tracks all dependencies. *)
  let cache = Store.of_table (Table.create (module Input) 2) in
  let input = (module Input : Store_intf.Input with type t = i) in
  create_with_cache name ~cache ~input ~cutoff ~human_readable_description f

let make_dep_node ~spec ~input : _ Dep_node.t =
  let dep_node_without_state : _ Dep_node_without_state.t =
    { id = Id.gen (); input; spec }
  in
  { without_state = dep_node_without_state
  ; last_cached_value = None
  ; state = Not_considering
  ; has_cutoff =
      (match spec.allow_cutoff with
      | Yes _equal -> true
      | No -> false)
  }

let dep_node (t : (_, _) t) input =
  match Store.find t.cache input with
  | Some dep_node -> dep_node
  | None ->
    let dep_node = make_dep_node ~spec:t.spec ~input in
    Store.set t.cache input dep_node;
    dep_node

let report_and_collect_errors f =
  Fiber.map_reduce_errors
    (module Collect_errors_monoid)
    ~on_error:(fun exn ->
      let exn, reproducible =
        match exn with
        | { Exn_with_backtrace.exn = Non_reproducible exn; backtrace } ->
          ({ Exn_with_backtrace.exn; backtrace }, false)
        | exn -> (exn, true)
      in
      let+ () = Error_handler.report_error exn in
      ({ exns = Exn_set.singleton exn; reproducible } : Collect_errors_monoid.t))
    f

let compute_dep_node (node : (_, 'a) Dep_node.t) : 'a Value.t Fiber.t =
  let+ res =
    report_and_collect_errors (fun () ->
        node.without_state.spec.f node.without_state.input)
  in
  match res with
  | Ok res -> Value.Ok res
  | Error errors -> Error errors

let yield_if_there_are_pending_events = ref Fiber.return

module Exec : sig
  (* [exec_dep_node] is a variant of [consider_and_compute] but with a simpler
     type, convenient for external usage. *)
  val exec_dep_node : ('i, 'o) Dep_node.t -> 'o Fiber.t
end = struct
  let currently_considering (v : _ State.t) : _ State.t =
    match v with
    | Not_considering -> Not_considering
    | Considering { run; _ } as running ->
      if Run.is_current run then
        running
      else
        Not_considering

  let rec restore_from_cache :
            'o.    'o Cached_value.t option
            -> 'o Cached_value.t Cache_lookup.Result.t Fiber.t =
   fun last_cached_value ->
    match last_cached_value with
    | None -> Fiber.return (Cache_lookup.Result.Failure Not_found)
    | Some cached_value -> (
      match cached_value.value with
      | Cancelled _dependency_cycle ->
        (* Dependencies of cancelled computations are not accurate, so we can't
           use [deps_changed] in this case. *)
        Fiber.return (Cache_lookup.Result.Failure Not_found)
      | Error { reproducible = false; _ } ->
        (* We do not cache non-reproducible errors. *)
        Fiber.return (Cache_lookup.Result.Failure Not_found)
      | Ok _
      | Error { reproducible = true; _ } -> (
        (* We cache reproducible errors just like normal values. We assume that
           all [Memo] computations are deterministic, which means if we rerun a
           computation that previously raised a set of errors on the same inputs
           then we expect to get the same set of errors back and might as well
           skip the unnecessary work. The downside is that if a computation is
           non-deterministic, there is no way to force rerunning it, apart from
           changing some of its dependencies. *)
        let+ deps_changed =
          (* Make sure [f] gets inlined to avoid unnecessary closure allocations
             and improve stack traces in profiling. *)
          Deps.changed_or_not cached_value.deps
            ~f:(fun [@inline] (Dep_node.T dep) ->
              if !Counters.enabled then
                Counters.record_new_edge_traversals ~count:1;
              match dep.has_cutoff with
              | false -> (
                (* If [dep] has no cutoff, it is sufficient to check whether it
                   is up to date. If not, we must recompute [last_cached_value]. *)
                consider_and_restore_from_cache dep
                >>| function
                | Ok cached_value_of_dep -> (
                  (* The [Changed] branch will be taken if [cached_value]'s node
                     was skipped in the previous run (it was unreachable), while
                     [dep] wasn't skipped and [cached_value_of_dep] changed. *)
                  match
                    Run.compare cached_value_of_dep.last_changed_at
                      cached_value.last_validated_at
                  with
                  | Gt -> Changed_or_not.Changed
                  | Eq
                  | Lt ->
                    Unchanged)
                | Failure (Cancelled { dependency_cycle }) ->
                  Cancelled { dependency_cycle }
                | Failure (Not_found | Out_of_date _) -> Changed)
              | true -> (
                (* If [dep] has a cutoff predicate, it is not sufficient to
                   check whether it is up to date: even if it isn't, after we
                   recompute it, the resulting value may remain unchanged,
                   allowing us to skip recomputing the [last_cached_value]. *)
                consider_and_compute dep
                >>= function
                | Ok cached_value_of_dep -> (
                  let+ cached_value_of_dep = cached_value_of_dep in
                  (* Note: [cached_value_of_dep.value] will be [Cancelled _] if
                     [dep] itself doesn't introduce a dependency cycle but one
                     of its transitive dependencies does. In this case, the
                     value will be new, so we will take the [Changed] branch. *)
                  match
                    Run.compare cached_value_of_dep.last_changed_at
                      cached_value.last_validated_at
                  with
                  | Gt -> Changed_or_not.Changed
                  | Eq
                  | Lt ->
                    Unchanged)
                | Error dependency_cycle ->
                  Fiber.return (Changed_or_not.Cancelled { dependency_cycle })))
        in
        match deps_changed with
        | Unchanged ->
          cached_value.last_validated_at <- Run.current ();
          Cache_lookup.Result.Ok cached_value
        | Changed -> Failure (Out_of_date cached_value)
        | Cancelled { dependency_cycle } ->
          Failure (Cancelled { dependency_cycle })))

  and compute :
        'i 'o.    ('i, 'o) Dep_node.t
        -> 'o Cached_value.t Cache_lookup.Failure.t
        -> Dep_node.packed Deps_so_far.t -> 'o Cached_value.t Fiber.t =
   fun dep_node cache_lookup_failure deps_so_far ->
    Deps_so_far.start_compute deps_so_far;
    let compute_value_and_deps_rev () =
      let* () = !yield_if_there_are_pending_events () in
      let+ value = compute_dep_node dep_node in
      let deps_rev = Deps_so_far.get_compute_deps_rev deps_so_far in
      if !Counters.enabled then
        Counters.record_new_edge_traversals ~count:(List.length deps_rev);
      (value, deps_rev)
    in
    match cache_lookup_failure with
    | Cancelled { dependency_cycle } ->
      Fiber.return (Cached_value.create_cancelled ~dependency_cycle)
    | Not_found ->
      let+ value, deps_rev = compute_value_and_deps_rev () in
      Cached_value.create value ~deps_rev
    | Out_of_date (old_cv : _ Cached_value.t) -> (
      let+ value, deps_rev = compute_value_and_deps_rev () in
      match Cached_value.value_changed dep_node old_cv.value value with
      | true -> Cached_value.create value ~deps_rev
      | false -> Cached_value.confirm_old_value ~deps_rev old_cv)

  and newly_considering :
        'i 'o. ('i, 'o) Dep_node.t -> 'o Cached_value.t Sample_attempt.t =
   fun dep_node ->
    let dag_node : Dag.node =
      { info = Dag.create_node_info global_dep_dag
      ; data = Dep_node_without_state.T dep_node.without_state
      }
    in
    let running_state : Running_state.t =
      { dag_node; deps_so_far = Deps_so_far.create () }
    in
    let frame : Stack_frame_with_state.t =
      T { without_state = dep_node.without_state; running_state }
    in
    let stop_considering ~(cached_value : _ Cached_value.t) ~computed =
      dep_node.state <- Not_considering;
      if !Counters.enabled then
        Counters.record_newly_considered_node
          ~edges:(Deps.length cached_value.deps)
          ~computed
    in
    let restore_from_cache =
      Once.create ~must_not_raise:(fun () ->
          Call_stack.push_frame frame (fun () ->
              let+ restore_result =
                restore_from_cache dep_node.last_cached_value
              in
              (match restore_result with
              | Ok cached_value ->
                stop_considering ~cached_value ~computed:false
              | Failure _ -> ());
              restore_result))
    in
    let compute =
      Once.and_then restore_from_cache ~f_must_not_raise:(function
        | Ok cached_value -> Fiber.return cached_value
        | Failure cache_lookup_failure ->
          Call_stack.push_frame frame (fun () ->
              dep_node.last_cached_value <- None;
              let+ cached_value =
                compute dep_node cache_lookup_failure running_state.deps_so_far
              in
              dep_node.last_cached_value <- Some cached_value;
              stop_considering ~cached_value ~computed:true;
              cached_value))
    in
    let result : _ Sample_attempt.Result.t = { restore_from_cache; compute } in
    dep_node.state <-
      Considering
        { run = Run.current ()
        ; running = running_state
        ; sample_attempt_result = result
        };
    Sample_attempt.Running { dag_node; result }

  and start_considering :
        'i 'o. ('i, 'o) Dep_node.t -> 'o Cached_value.t Sample_attempt.t =
   fun dep_node ->
    match get_cached_value_in_current_cycle dep_node with
    | Some cv -> Finished cv
    | None -> (
      match currently_considering dep_node.state with
      | Not_considering -> newly_considering dep_node
      | Considering
          { running = { dag_node; deps_so_far = _ }
          ; sample_attempt_result = result
          ; _
          } ->
        Running { dag_node; result })

  and consider :
        'i 'o.    ('i, 'o) Dep_node.t
        -> ('o Cached_value.t Sample_attempt.t, Cycle_error.t) result Fiber.t =
   fun dep_node ->
    let sample_attempt = start_considering dep_node in
    add_dep_from_caller dep_node sample_attempt
    >>| Result.map ~f:(fun () -> sample_attempt)

  and consider_and_compute :
        'i 'o.    ('i, 'o) Dep_node.t
        -> ('o Cached_value.t Fiber.t, Cycle_error.t) result Fiber.t =
   fun dep_node -> consider dep_node >>| Result.map ~f:Sample_attempt.compute

  and consider_and_restore_from_cache :
        'i 'o.    ('i, 'o) Dep_node.t
        -> 'o Cached_value.t Cache_lookup.Result.t Fiber.t =
   fun dep_node ->
    consider dep_node >>= function
    | Ok sample_attempt -> Sample_attempt.restore sample_attempt
    | Error dependency_cycle ->
      Fiber.return
        (Cache_lookup.Result.Failure (Cancelled { dependency_cycle }))

  let exec_dep_node (dep_node : _ Dep_node.t) =
    Fiber.of_thunk (fun () ->
        let stack_frame = Dep_node_without_state.T dep_node.without_state in
        consider_and_compute dep_node >>= function
        | Ok res ->
          let* res = res in
          Value.get_exn res.value ~stack_frame
        | Error cycle_error -> raise (Cycle_error.E cycle_error))
end

let exec (type i o) (t : (i, o) t) i = Exec.exec_dep_node (dep_node t i)

let get_call_stack = Call_stack.get_call_stack_without_state

module Invalidation = struct
  type ('i, 'o) memo = ('i, 'o) t

  module Leaf = struct
    type t =
      | Invalidate_node : _ Dep_node.t -> t
      | Clear_cache : ('input, ('input, 'output) Dep_node.t) Store.t -> t
      | Clear_caches

    let to_dyn (t : t) =
      match t with
      | Invalidate_node node ->
        Stack_frame_without_state.to_dyn (T node.without_state)
      | Clear_cache _ -> Dyn.Variant ("Clear_cache", [ Dyn.Opaque ])
      | Clear_caches -> Dyn.Variant ("Clear_caches", [])
  end

  (* Represented as a tree mainly to get a tail-recursive execution. *)
  type t =
    | Empty
    | Leaf of Leaf.t
    | Combine of t * t

  let empty : t = Empty

  let combine a b =
    match (a, b) with
    | Empty, x
    | x, Empty ->
      x
    | x, y -> Combine (x, y)

  let execute_leaf = function
    | Leaf.Invalidate_node node -> invalidate_dep_node node
    | Clear_cache store -> invalidate_store store
    | Clear_caches -> Caches.clear ()

  let rec execute x xs =
    match x with
    | Empty -> execute_list xs
    | Leaf f ->
      execute_leaf f;
      execute_list xs
    | Combine (x, y) -> execute x (y :: xs)

  and execute_list = function
    | [] -> ()
    | x :: xs -> execute x xs

  let rec to_list_x_xs x xs acc =
    match x with
    | Empty -> to_list_xs xs acc
    | Leaf f -> to_list_xs xs (f :: acc)
    | Combine (x, y) -> to_list_x_xs x (y :: xs) acc

  and to_list_xs xs acc =
    match xs with
    | [] -> acc
    | x :: xs -> to_list_x_xs x xs acc

  let to_list t = to_list_x_xs t [] []

  let to_dyn t = Dyn.List (List.map (to_list t) ~f:Leaf.to_dyn)

  let execute x = execute x []

  let is_empty = function
    | Empty -> true
    | _ -> false

  let clear_caches = Leaf Clear_caches

  let invalidate_cache { cache; _ } = Leaf (Clear_cache cache)

  let invalidate_node (node : _ Dep_node.t) = Leaf (Invalidate_node node)
end

module Current_run = struct
  let f () = Run.current () |> Build0.return

  let memo = create "current-run" ~input:(module Unit) f

  let exec () = exec memo ()

  let invalidate () = Invalidation.invalidate_node (dep_node memo ())
end

let current_run () = Current_run.exec ()

module Build = struct
  include Build0

  let of_non_reproducible_fiber fiber =
    let* (_ : Run.t) = current_run () in
    fiber

  let is_top_level =
    let+ is_set = Error_handler.is_set in
    not is_set

  let run_with_error_handler t ~handle_error_no_raise =
    Error_handler.with_error_handler handle_error_no_raise (fun () ->
        let* res = report_and_collect_errors (fun () -> t) in
        match res with
        | Ok ok -> Fiber.return ok
        | Error ({ exns; reproducible = _ } : Collect_errors_monoid.t) ->
          Fiber.reraise_all (Exn_set.to_list exns))

  let run t =
    let* is_top_level = is_top_level in
    (* CR-someday aalekseyev: I think this automagical detection of toplevel
       calls is weird. My hunch is that having separate functions for toplevel
       and non-toplevel [run] would be better. *)
    match is_top_level with
    | true ->
      run_with_error_handler t ~handle_error_no_raise:(fun _exn ->
          Fiber.return ())
    | false -> t
end

module With_implicit_output = struct
  type ('i, 'o) t = 'i -> 'o Fiber.t

  let create name ~input ~implicit_output impl =
    let memo =
      create name ~input (fun i ->
          Implicit_output.collect implicit_output (fun () -> impl i))
    in
    fun input ->
      let* res, output = exec memo input in
      let+ () = Implicit_output.produce_opt implicit_output output in
      res

  let exec t = t
end

module Cell = struct
  type ('i, 'o) t = ('i, 'o) Dep_node.t

  let input (t : (_, _) t) = t.without_state.input

  let read = Exec.exec_dep_node

  let invalidate = Invalidation.invalidate_node
end

let cell = dep_node

module Expert = struct
  let previously_evaluated_cell t input = Store.find t.cache input
end

module Implicit_output = Implicit_output

let lazy_cell ?cutoff ?name ?human_readable_description f =
  let spec =
    Spec.create ~name ~input:(module Unit) ~cutoff ~human_readable_description f
  in
  make_dep_node ~spec ~input:()

let lazy_ ?cutoff ?name ?human_readable_description f =
  let cell = lazy_cell ?cutoff ?name ?human_readable_description f in
  fun () -> Cell.read cell

let push_stack_frame ~human_readable_description f =
  Cell.read (lazy_cell ~human_readable_description f)

module Lazy = struct
  type 'a t = unit -> 'a Fiber.t

  let of_val a () = Fiber.return a

  let create = lazy_

  let force f = f ()

  let map t ~f = create (fun () -> Fiber.map ~f (t ()))
end

module Volatile = struct
  type 'a t = (unit, 'a) Dep_node.t

  (* CR amokhov: Create a table instead of using lazy cells so that all volatile
     cells can be cleared in one go if needed. *)
  let create ~sample ~equal = lazy_cell ~cutoff:equal sample

  let sample = Cell.read

  let check (t : _ Dep_node.t) =
    match t.last_cached_value with
    | None -> Fiber.return (Invalidation.invalidate_node t)
    | Some cached_value -> (
      let+ current = compute_dep_node t in
      match Cached_value.value_changed t cached_value.value current with
      | true -> Invalidation.invalidate_node t
      | false -> Invalidation.empty)
end

module Poly (Function : sig
  type 'a input

  type 'a output

  val name : string

  val id : 'a input -> 'a Type_eq.Id.t

  val to_dyn : _ input -> Dyn.t

  val eval : 'a input -> 'a output Fiber.t
end) =
struct
  open Function

  module Key = struct
    type t = T : _ input -> t

    let to_dyn (T t) = to_dyn t

    let hash (T t) = Type_eq.Id.hash (id t)

    let equal (T x) (T y) = Type_eq.Id.equal (id x) (id y)
  end

  module Value = struct
    type t = T : ('a Type_eq.Id.t * 'a output) -> t

    let get (type a) ~(input_with_matching_id : a input) value : a output =
      match value with
      | T (id_v, res) -> (
        match Type_eq.Id.same id_v (id input_with_matching_id) with
        | None ->
          Code_error.raise
            "Type_eq.Id.t mismatch in Memo.Poly: the likely reason is that the \
             provided Function.id returns different ids for the same input."
            [ ("Function.name", Dyn.String name) ]
        | Some Type_eq.T -> res)
  end

  let memo =
    create name
      ~input:(module Key)
      (function
        | Key.T input -> eval input >>| fun v -> Value.T (id input, v))

  let eval x = exec memo (Key.T x) >>| Value.get ~input_with_matching_id:x
end

let incremental_mode_enabled =
  ref
    (match Sys.getenv_opt "DUNE_WATCHING_MODE_INCREMENTAL" with
    | Some "true" -> true
    | Some "false"
    | None ->
      false
    | Some _ ->
      User_error.raise
        [ Pp.text "Invalid value of DUNE_WATCHING_MODE_INCREMENTAL" ])

let reset invalidation =
  Invalidation.execute
    (Invalidation.combine invalidation (Current_run.invalidate ()));
  Run.restart ();
  Counters.reset ()

module Perf_counters = struct
  let enable () = Counters.enabled := true

  let nodes_in_current_run () = !Counters.nodes_considered

  let edges_in_current_run () = !Counters.edges_considered

  let nodes_computed_in_current_run () = !Counters.nodes_computed

  let edges_traversed_in_current_run () = !Counters.edges_traversed

  let report_for_current_run () =
    sprintf "%d/%d computed/total nodes, %d/%d traversed/total edges"
      (nodes_computed_in_current_run ())
      (nodes_in_current_run ())
      (edges_traversed_in_current_run ())
      (edges_in_current_run ())

  let assert_invariants () =
    assert (nodes_computed_in_current_run () <= nodes_in_current_run ());
    assert (edges_in_current_run () <= edges_traversed_in_current_run ());
    assert (edges_traversed_in_current_run () <= 2 * edges_in_current_run ())

  let reset () = Counters.reset ()
end

module For_tests = struct
  let get_deps (type i o) (t : (i, o) t) inp =
    match Store.find t.cache inp with
    | None -> None
    | Some dep_node -> (
      match get_cached_value_in_current_cycle dep_node with
      | None -> None
      | Some cv ->
        Some
          (Deps.to_list cv.deps
          |> List.map ~f:(fun (Dep_node.T dep) ->
                 (dep.without_state.spec.name, ser_input dep.without_state))))

  let clear_memoization_caches () = Caches.clear ()
end

module Store = Store_intf

module Run = struct
  type t = Run.t

  module For_tests = struct
    let compare = Run.compare

    let current = Run.current
  end
end
