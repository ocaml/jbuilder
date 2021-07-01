open! Stdune
open Import

type 'a eval_mode =
  | Lazy : unit eval_mode
  | Eager : Dep.Fact.t eval_mode

type 'a thunk = { f : 'm. 'm eval_mode -> ('a * 'm Dep.Map.t) Memo.Build.t }
[@@unboxed]

module Deps_or_facts = struct
  let union : type a. a eval_mode -> a Dep.Map.t -> a Dep.Map.t -> a Dep.Map.t =
   fun mode a b ->
    match mode with
    | Lazy -> Dep.Set.union a b
    | Eager -> Dep.Facts.union a b

  let union_all mode l = List.fold_left l ~init:Dep.Map.empty ~f:(union mode)
end

module T = struct
  open Memo.Build.O

  module M = struct
    type 'a t = 'a thunk

    let return x = { f = (fun _mode -> Memo.Build.return (x, Dep.Map.empty)) }

    let map t ~f =
      { f =
          (fun mode ->
            let+ x, deps = t.f mode in
            (f x, deps))
      }

    let bind t ~f =
      { f =
          (fun mode ->
            let* x, deps1 = t.f mode in
            let+ y, deps2 = (f x).f mode in
            (y, Deps_or_facts.union mode deps1 deps2))
      }
  end

  include M

  let both x y =
    { f =
        (fun mode ->
          let+ x, deps1 = x.f mode
          and+ y, deps2 = y.f mode in
          ((x, y), Deps_or_facts.union mode deps1 deps2))
    }

  let all xs =
    { f =
        (fun mode ->
          let+ res = Memo.Build.parallel_map xs ~f:(fun x -> x.f mode) in
          let res, facts = List.split res in
          (res, Deps_or_facts.union_all mode facts))
    }

  let memo_build m =
    { f =
        (fun _mode ->
          let+ x = m in
          (x, Dep.Map.empty))
    }

  module O = struct
    let ( >>> ) a b =
      { f =
          (fun mode ->
            let+ ((), deps_a), (b, deps_b) =
              Memo.Build.fork_and_join (fun () -> a.f mode) (fun () -> b.f mode)
            in
            (b, Deps_or_facts.union mode deps_a deps_b))
      }

    let ( >>= ) t f = bind t ~f

    let ( >>| ) t f = map t ~f

    let ( and+ ) = both

    let ( and* ) = both

    let ( let+ ) t f = map t ~f

    let ( let* ) t f = bind t ~f
  end

  module List = struct
    include Monad.List (struct
      include M
      include Monad.Make (M)
    end)

    let map l ~f =
      { f =
          (fun mode ->
            let+ res = Memo.Build.parallel_map l ~f:(fun x -> (f x).f mode) in
            let res, deps = List.split res in
            (res, Deps_or_facts.union_all mode deps))
      }

    let concat_map l ~f =
      { f =
          (fun mode ->
            let+ res = Memo.Build.parallel_map l ~f:(fun x -> (f x).f mode) in
            let res, deps = List.split res in
            (List.concat res, Deps_or_facts.union_all mode deps))
      }
  end
end

module Expander = String_with_vars.Make_expander (T)
include T
open O

open struct
  module List = Stdune.List
end

let of_thunk t = t

let run t mode = t.f mode

let force_lazy_or_eager :
    type a b.
       a eval_mode
    -> (b * Dep.Set.t) Memo.Lazy.t Lazy.t
    -> (b * Dep.Facts.t) Memo.Lazy.t Lazy.t
    -> (b * a Dep.Map.t) Memo.Build.t =
 fun mode lazy_ eager ->
  match mode with
  | Lazy -> Memo.Lazy.force (Lazy.force lazy_)
  | Eager -> Memo.Lazy.force (Lazy.force eager)

let memoize name t =
  let lazy_ = lazy (Memo.lazy_ ~name (fun () -> t.f Lazy))
  and eager = lazy (Memo.lazy_ ~name (fun () -> t.f Eager)) in
  { f = (fun mode -> force_lazy_or_eager mode lazy_ eager) }

let ignore x = map x ~f:ignore

let map2 x y ~f =
  let+ x = x
  and+ y = y in
  f x y

let push_stack_frame ~human_readable_description f =
  { f =
      (fun mode ->
        Memo.push_stack_frame ~human_readable_description (fun () ->
            (f ()).f mode))
  }

let delayed f =
  let+ () = return () in
  f ()

let all_unit (xs : unit t list) =
  { f =
      (fun mode ->
        let open Memo.Build.O in
        let+ res = Memo.Build.parallel_map xs ~f:(fun x -> x.f mode) in
        let deps = List.map res ~f:snd in
        ((), Deps_or_facts.union_all mode deps))
  }

let fail x =
  let+ () = return () in
  x.fail ()

type ('input, 'output) memo =
  { lazy_ : ('input, 'output * Dep.Set.t) Memo.t Lazy.t
  ; eager : ('input, 'output * Dep.Facts.t) Memo.t Lazy.t
  }

let create_memo name ~input ?cutoff ?human_readable_description f =
  let lazy_ =
    lazy
      (let cutoff =
         Option.map cutoff ~f:(fun f (a, deps1) (b, deps2) ->
             f a b && Dep.Set.equal deps1 deps2)
       in
       let name = name ^ "(lazy)" in
       Memo.create name ~input ?cutoff ?human_readable_description (fun x ->
           (f x).f Lazy))
  and eager =
    lazy
      (let cutoff =
         Option.map cutoff ~f:(fun f (a, facts1) (b, facts2) ->
             f a b && Dep.Map.equal facts1 facts2 ~equal:Dep.Fact.equal)
       in
       Memo.create name ~input ?cutoff ?human_readable_description (fun x ->
           (f x).f Eager))
  in
  { lazy_; eager }

let exec_memo :
    type i o m.
    (i, o) memo -> i -> m eval_mode -> (o * m Dep.Map.t) Memo.Build.t =
 fun memo i mode ->
  match mode with
  | Lazy -> Memo.exec (Lazy.force memo.lazy_) i
  | Eager -> Memo.exec (Lazy.force memo.eager) i

let exec_memo m i = { f = (fun mode -> exec_memo m i mode) }

let goal t =
  { f =
      (fun mode ->
        let open Memo.Build.O in
        let+ a, (_irrelevant_for_goals : _ Dep.Map.t) = t.f mode in
        (a, Dep.Map.empty))
  }

let memo_build_join f =
  { f =
      (fun mode ->
        let open Memo.Build.O in
        let* t = f in
        t.f mode)
  }

let dyn_memo_build f = f >>= memo_build
