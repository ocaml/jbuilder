open! Stdune

type 'a t = ('a -> unit) -> unit

(* This module tries to enforce the following invariants:

   - the execution context passed to a continuation is the same as the current
   one

   - the execution of a fiber always ends with [deref]

   - when an exception is raised by the user code, the exception must be
   forwarded to the execution context that was active at the time the exception
   was raised

   - when an exception is raised by the user code, then we assume that the
   current fiber didn't reach the [deref] point. As a result we have to call
   [deref] at this point on the current execution context

   Remarks:

   - most of the code assumes that errors will be caught by the caller, so when
   we do a context switch, we simply change the current execution context and
   chain to the continuation without catching errors. The current [try..with]
   will catch any raised error and forward to the current execution context. The
   only place we add a [try..with] is at the toplevel or when forking. *)

let of_thunk f k = f () k

module Execution_context : sig
  module K : sig
    (* Represent a suspended fiber *)
    type 'a t

    (* Create a continuation that captures the current execution context *)
    val create : ('a -> unit) -> 'a t

    (* Restart a suspended fiber. [run] doesn't preserve the current execution
       context and should always be called last. *)
    val run : 'a t -> 'a -> unit
  end

  (* Execute the current continuation, making sure to forward errors to the
     current execution context. This function doesn't preserve the current
     execution context. It should be used to execute the current continuation
     before calling [K.run] *)
  val safe_run_k : ('a -> unit) -> 'a -> unit

  (* Execute a function returning a fiber, passing any raised exception to the
     current execution context. This function preserve the current execution
     context. It should be called when creating forks.*)
  val apply : ('a -> 'b t) -> 'a -> 'b t

  (* Add [n] references to the current execution context *)
  val add_refs : int -> unit

  (* Decrese the reference count of the current execution context *)
  val deref : unit -> unit

  (* [wait_errors f] executes [f ()] inside a new execution contexts. Returns a
     fiber that terminates when all the fiber in the sub-context have
     terminated. *)
  val wait_errors : (unit -> 'a t) -> ('a, unit) result t

  (* Set the current error handler. [on_error] is called in the current
     execution context. *)
  val set_error_handler :
    on_error:(Exn_with_backtrace.t -> unit) -> ('a -> 'b t) -> 'a -> 'b t

  val vars : unit -> Univ_map.t

  val set_vars : Univ_map.t -> ('a -> 'b t) -> 'a -> 'b t

  val set_vars_sync : Univ_map.t -> ('a -> 'b) -> 'a -> 'b

  (* Execute a callback with a fresh execution context. For the toplevel
     [Fiber.run] function. *)
  val new_run : (unit -> 'a) -> 'a

  val reraise_all : Exn_with_backtrace.t list -> unit
end = struct
  type t =
    { on_error : Exn_with_backtrace.t k option
          (* This handler must never raise *)
    ; vars : Univ_map.t
    ; on_release : on_release
    }

  and 'a on_release_exec =
    { k : ('a, unit) result k
    ; mutable result : ('a, unit) result
    ; mutable ref_count : int
    }

  and on_release =
    | Do_nothing : on_release
    | Exec : _ on_release_exec -> on_release

  and 'a k =
    { run : 'a -> unit
    ; ctx : t
    }

  let current =
    ref { on_error = None; vars = Univ_map.empty; on_release = Do_nothing }

  let add_refs n =
    let t = !current in
    match t.on_release with
    | Do_nothing -> ()
    | Exec r -> r.ref_count <- r.ref_count + n

  let rec deref t =
    match t.on_release with
    | Do_nothing -> ()
    | Exec r ->
      let n = r.ref_count - 1 in
      assert (n >= 0);
      r.ref_count <- n;
      if n = 0 then (
        current := r.k.ctx;
        (* We need to call [safe_run_k] as we might be the in handler of the
           [try...with] block inside [apply] and so we are no more in a
           [try...with] blocks *)
        safe_run_k r.k.run r.result
      )

  and safe_run_k : type a. (a -> unit) -> a -> unit =
   fun k x ->
    try k x with
    | exn -> forward_error exn

  and forward_exn_with_bt t exn =
    match t.on_error with
    | None -> Exn_with_backtrace.reraise exn
    | Some { ctx; run } -> (
      current := ctx;
      try run exn with
      | exn ->
        let exn = Exn_with_backtrace.capture exn in
        forward_exn_with_bt ctx exn)

  and forward_error exn =
    let exn = Exn_with_backtrace.capture exn in
    let t = !current in
    forward_exn_with_bt t exn;
    deref t

  let deref () = deref !current

  let wait_errors f k =
    let t = !current in
    let on_release =
      { k = { ctx = t; run = k }; ref_count = 1; result = Error () }
    in
    let child = { t with on_release = Exec on_release } in
    current := child;
    f () (fun x ->
        on_release.result <- Ok x;
        deref ())

  let set_error_handler ~on_error f x k =
    let t = !current in
    let on_error = Some { run = on_error; ctx = t } in
    current := { t with on_error };
    f x (fun x ->
        current := t;
        k x)

  let vars () = !current.vars

  let set_vars vars f x k =
    let t = !current in
    current := { t with vars };
    f x (fun x ->
        current := t;
        k x)

  let set_vars_sync (type b) vars f x : b =
    let t = !current in
    current := { t with vars };
    Exn.protect ~finally:(fun () -> current := t) ~f:(fun () -> f x)

  module K = struct
    type 'a t = 'a k

    let create run = { run; ctx = !current }

    let run { run; ctx } x =
      current := ctx;
      safe_run_k run x
  end

  let apply f x k =
    let backup = !current in
    (try f x k with
    | exn -> forward_error exn);
    current := backup

  let reraise_all exns =
    let backup = !current in
    List.iter exns ~f:(forward_exn_with_bt backup);
    current := backup;
    deref ()

  let new_run f =
    let backup = !current in
    Exn.protect
      ~finally:(fun () -> current := backup)
      ~f:(fun () ->
        current :=
          { on_error = None; vars = Univ_map.empty; on_release = Do_nothing };
        f ())
end

module EC = Execution_context
module K = EC.K

let return x k = k x

let never _ = ()

module O = struct
  let ( >>> ) a b k = a (fun () -> b k)

  let ( >>= ) t f k = t (fun x -> f x k)

  let ( >>| ) t f k = t (fun x -> k (f x))

  let ( let+ ) = ( >>| )

  let ( let* ) = ( >>= )
end

open O

let map t ~f = t >>| f

let bind t ~f = t >>= f

let both a b =
  let* x = a in
  let* y = b in
  return (x, y)

let sequential_map l ~f =
  let rec loop l acc =
    match l with
    | [] -> return (List.rev acc)
    | x :: l ->
      let* x = f x in
      loop l (x :: acc)
  in
  loop l []

let sequential_iter l ~f =
  let rec loop l =
    match l with
    | [] -> return ()
    | x :: l ->
      let* () = f x in
      loop l
  in
  loop l

type ('a, 'b) fork_and_join_state =
  | Nothing_yet
  | Got_a of 'a
  | Got_b of 'b

let fork_and_join fa fb k =
  let state = ref Nothing_yet in
  EC.add_refs 1;
  EC.apply fa () (fun a ->
      match !state with
      | Nothing_yet ->
        state := Got_a a;
        EC.deref ()
      | Got_a _ -> assert false
      | Got_b b -> k (a, b));
  fb () (fun b ->
      match !state with
      | Nothing_yet ->
        state := Got_b b;
        EC.deref ()
      | Got_a a -> k (a, b)
      | Got_b _ -> assert false)

let fork_and_join_unit fa fb k =
  let state = ref Nothing_yet in
  EC.add_refs 1;
  EC.apply fa () (fun () ->
      match !state with
      | Nothing_yet ->
        state := Got_a ();
        EC.deref ()
      | Got_a _ -> assert false
      | Got_b b -> k b);
  fb () (fun b ->
      match !state with
      | Nothing_yet ->
        state := Got_b b;
        EC.deref ()
      | Got_a () -> k b
      | Got_b _ -> assert false)

module Sequence = struct
  type 'a fiber = 'a t

  type 'a t = 'a node fiber

  and 'a node =
    | Nil
    | Cons of 'a * 'a t

  let rec sequential_iter t ~f =
    t >>= function
    | Nil -> return ()
    | Cons (x, t) ->
      let* () = f x in
      sequential_iter t ~f

  let parallel_iter t ~f k =
    let n = ref 1 in
    let k () =
      decr n;
      if !n = 0 then
        k ()
      else
        EC.deref ()
    in
    let rec loop t =
      t (function
        | Nil -> k ()
        | Cons (x, t) ->
          EC.add_refs 1;
          incr n;
          EC.apply f x k;
          loop t)
    in
    loop t
end

let list_of_option_array =
  let rec loop arr i acc =
    if i = 0 then
      acc
    else
      let i = i - 1 in
      match arr.(i) with
      | None -> assert false
      | Some x -> loop arr i (x :: acc)
  in
  fun a -> loop a (Array.length a) []

let parallel_map l ~f k =
  match l with
  | [] -> k []
  | [ x ] -> f x (fun x -> k [ x ])
  | _ ->
    let n = List.length l in
    EC.add_refs (n - 1);
    let left_over = ref n in
    let results = Array.make n None in
    List.iteri l ~f:(fun i x ->
        EC.apply f x (fun y ->
            results.(i) <- Some y;
            decr left_over;
            if !left_over = 0 then
              k (list_of_option_array results)
            else
              EC.deref ()))

let[@inline always] parallel_iter_generic ~n ~iter ~f k =
  EC.add_refs (n - 1);
  let left_over = ref n in
  let k () =
    decr left_over;
    if !left_over = 0 then
      k ()
    else
      EC.deref ()
  in
  iter ~f:(fun x -> EC.apply f x k)

let parallel_iter l ~f k =
  match l with
  | [] -> k ()
  | [ x ] -> f x k
  | _ -> parallel_iter_generic ~n:(List.length l) ~iter:(List.iter l) ~f k

let parallel_iter_set (type a s)
    (module S : Set.S with type elt = a and type t = s) t ~(f : a -> unit t) k =
  let len = S.cardinal t in
  match len with
  | 0 -> k ()
  | 1 -> f (Option.value_exn (S.min_elt t)) k
  | n -> parallel_iter_generic ~n ~iter:(S.iter t) ~f k

module Var = struct
  include Univ_map.Key

  let get var = Univ_map.find (EC.vars ()) var

  let get_exn var = Univ_map.find_exn (EC.vars ()) var

  let set_sync var x f = EC.set_vars_sync (Univ_map.set (EC.vars ()) var x) f ()

  let set var x f k = EC.set_vars (Univ_map.set (EC.vars ()) var x) f () k

  let unset_sync var f =
    EC.set_vars_sync (Univ_map.remove (EC.vars ()) var) f ()

  let unset var f k = EC.set_vars (Univ_map.remove (EC.vars ()) var) f () k

  let create () = create ~name:"var" (fun _ -> Dyn.Encoder.string "var")
end

let with_error_handler f ~on_error k = EC.set_error_handler ~on_error f () k

let wait_errors f k = EC.wait_errors f k

let fold_errors f ~init ~on_error =
  let acc = ref init in
  let on_error exn = acc := on_error exn !acc in
  wait_errors (fun () -> with_error_handler ~on_error f) >>| function
  | Ok _ as ok -> ok
  | Error () -> Error !acc

let collect_errors f =
  let+ res = fold_errors f ~init:[] ~on_error:(fun e l -> e :: l) in
  match res with
  | Ok x -> Ok x
  | Error l -> Error (List.rev l)

let reraise_all = function
  | [] -> never
  | [ exn ] -> Exn_with_backtrace.reraise exn
  | exns ->
    EC.reraise_all exns;
    never

let finalize f ~finally =
  let* res1 = collect_errors f in
  let* res2 = collect_errors finally in
  let res =
    match (res1, res2) with
    | Ok x, Ok () -> Ok x
    | Error l, Ok _
    | Ok _, Error l ->
      Error l
    | Error l1, Error l2 -> Error (l1 @ l2)
  in
  match res with
  | Ok x -> return x
  | Error l -> reraise_all l

module Ivar = struct
  type 'a state =
    | Full of 'a
    | Empty of 'a K.t Queue.t

  type 'a t = { mutable state : 'a state }

  let create () = { state = Empty (Queue.create ()) }

  let fill t x k =
    match t.state with
    | Full _ -> failwith "Fiber.Ivar.fill"
    | Empty q ->
      t.state <- Full x;
      EC.safe_run_k k ();
      Queue.iter q ~f:(fun k -> K.run k x)

  let read t k =
    match t.state with
    | Full x -> k x
    | Empty q -> Queue.push q (K.create k)

  let peek t k =
    k
      (match t.state with
      | Full x -> Some x
      | Empty _ -> None)
end

module Mvar = struct
  type 'a t =
    { writers : ('a * unit K.t) Queue.t
    ; readers : 'a K.t Queue.t
    ; mutable value : 'a option
    }

  (* Invariant enforced on mvars. We don't actually call this function, but we
     keep it here for documentation and to help understand the implementation: *)
  let _invariant t =
    match t.value with
    | None -> Queue.is_empty t.writers
    | Some _ -> Queue.is_empty t.readers

  let create () =
    { value = None; writers = Queue.create (); readers = Queue.create () }

  let create_full x =
    { value = Some x; writers = Queue.create (); readers = Queue.create () }

  let read t k =
    match t.value with
    | None -> Queue.push t.readers (K.create k)
    | Some v -> (
      match Queue.pop t.writers with
      | None ->
        t.value <- None;
        k v
      | Some (v', w) ->
        t.value <- Some v';
        EC.safe_run_k k v;
        K.run w ())

  let write t x k =
    match t.value with
    | Some _ -> Queue.push t.writers (x, K.create k)
    | None -> (
      match Queue.pop t.readers with
      | None ->
        t.value <- Some x;
        k ()
      | Some r ->
        EC.safe_run_k k ();
        K.run r x)
end

module Mutex = struct
  type t =
    { mutable locked : bool
    ; mutable waiters : unit K.t Queue.t
    }

  let lock t k =
    if t.locked then
      Queue.push t.waiters (K.create k)
    else (
      t.locked <- true;
      k ()
    )

  let unlock t k =
    assert t.locked;
    match Queue.pop t.waiters with
    | None ->
      t.locked <- false;
      k ()
    | Some next ->
      EC.safe_run_k k ();
      K.run next ()

  let with_lock t f =
    let* () = lock t in
    finalize f ~finally:(fun () -> unlock t)

  let create () = { locked = false; waiters = Queue.create () }
end

module Throttle = struct
  type t =
    { mutable size : int
    ; mutable running : int
    ; waiting : unit Ivar.t Queue.t
    }

  let create size = { size; running = 0; waiting = Queue.create () }

  let size t = t.size

  let running t = t.running

  let rec restart t =
    if t.running >= t.size then
      return ()
    else
      match Queue.pop t.waiting with
      | None -> return ()
      | Some ivar ->
        t.running <- t.running + 1;
        let* () = Ivar.fill ivar () in
        restart t

  let resize t n =
    t.size <- n;
    restart t

  let run t ~f =
    finalize
      ~finally:(fun () ->
        t.running <- t.running - 1;
        restart t)
      (fun () ->
        if t.running < t.size then (
          t.running <- t.running + 1;
          f ()
        ) else
          let waiting = Ivar.create () in
          Queue.push t.waiting waiting;
          let* () = Ivar.read waiting in
          f ())
end

type fill = Fill : 'a Ivar.t * 'a -> fill

let run t ~iter =
  EC.new_run (fun () ->
      let result = ref None in
      EC.apply (fun () -> t) () (fun x -> result := Some x);
      let rec loop () =
        match !result with
        | Some res -> res
        | None ->
          let (Fill (ivar, v)) = iter () in
          Ivar.fill ivar v ignore;
          loop ()
      in
      loop ())
