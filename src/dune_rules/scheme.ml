open! Dune_engine
open! Stdune

module T = struct
  type 'rules t =
    | Empty
    | Union of 'rules t * 'rules t
    | Approximation of Path.Build.w Dir_set.t * 'rules t
    | Finite of 'rules Path.Build.Map.t
    | Thunk of (unit -> 'rules t)
end

include T

module Evaluated : sig
  type 'rules t

  val union : union_rules:('a -> 'a -> 'a) -> 'a t -> 'a t -> 'a t

  val empty : 'a t

  val restrict : Path.Build.w Dir_set.t -> 'a t Memo.Lazy.t -> 'a t

  val finite : union_rules:('a -> 'a -> 'a) -> 'a Path.Build.Map.t -> 'a t

  val get_rules : 'a t -> dir:Path.Build.t -> 'a option * String.Set.t
end = struct
  type 'rules t =
    { by_child : 'rules t Memo.Lazy.t String.Map.t
    ; rules_here : 'rules option Memo.Lazy.t
    }

  let empty =
    { by_child = String.Map.empty; rules_here = Memo.Lazy.of_val None }

  let descend t dir =
    match String.Map.find t.by_child dir with
    | None -> empty
    | Some res -> Memo.Lazy.force res

  let union_option ~f a b =
    match (a, b) with
    | None, x
    | x, None ->
      x
    | Some x, Some y -> Some (f x y)

  let rec union ~union_rules x y =
    { by_child =
        String.Map.union x.by_child y.by_child ~f:(fun _key data1 data2 ->
            Some
              (Memo.Lazy.map2 data1 data2 ~f:(fun x y -> union ~union_rules x y)))
    ; rules_here =
        Memo.Lazy.map2 x.rules_here y.rules_here
          ~f:(union_option ~f:union_rules)
    }

  let rec restrict (dirs : Path.Local.w Dir_set.t) t : _ t =
    { rules_here =
        (if Dir_set.here dirs then
          Memo.Lazy.bind t ~f:(fun t -> t.rules_here)
        else
          Memo.Lazy.of_val None)
    ; by_child =
        (match Dir_set.default dirs with
        | true ->
          (* This is forcing the lazy potentially too early if the directory the
             user is interested in is not actually in the set. We're not fully
             committed to supporting this case though, anyway. *)
          String.Map.mapi (Memo.Lazy.force t).by_child ~f:(fun dir v ->
              Memo.lazy_ (fun () -> restrict (Dir_set.descend dirs dir) v))
        | false ->
          String.Map.mapi (Dir_set.exceptions dirs) ~f:(fun dir v ->
              Memo.lazy_ (fun () ->
                  restrict v
                    (Memo.lazy_ (fun () -> descend (Memo.Lazy.force t) dir)))))
    }

  let restrict dirs t = restrict (Dir_set.forget_root dirs) t

  let singleton path rules =
    let rec go = function
      | [] ->
        { by_child = String.Map.empty
        ; rules_here = Memo.Lazy.of_val (Some rules)
        }
      | x :: xs ->
        { by_child = String.Map.singleton x (Memo.Lazy.of_val (go xs))
        ; rules_here = Memo.Lazy.of_val None
        }
    in
    go (Path.Build.explode path)

  let finite ~union_rules m =
    Path.Build.Map.foldi m ~init:empty ~f:(fun path rules acc ->
        union ~union_rules (singleton path rules) acc)

  let get_rules t ~dir =
    let dir = Path.Build.explode dir in
    let t = List.fold_left dir ~init:t ~f:descend in
    ( Memo.Lazy.force t.rules_here
    , String.Set.of_list (String.Map.keys t.by_child) )
end

let evaluate ~union_rules =
  let rec loop ~env = function
    | Empty -> Evaluated.empty
    | Union (x, y) -> Evaluated.union ~union_rules (loop ~env x) (loop ~env y)
    | Approximation (paths, rules) ->
      if
        (not (Dir_set.is_subset paths ~of_:env))
        && not (Dir_set.is_subset (Dir_set.negate paths) ~of_:env)
      then
        Code_error.raise
          "inner [Approximate] specifies a set such that neither it, nor its \
           negation, are a subset of directories specified by the outer \
           [Approximate]."
          [ ("inner", Dir_set.to_dyn paths); ("outer", Dir_set.to_dyn env) ]
      else
        let paths = Dir_set.inter paths env in
        Evaluated.restrict paths (Memo.lazy_ (fun () -> loop ~env:paths rules))
    | Finite rules ->
      let violations =
        List.filter (Path.Build.Map.keys rules) ~f:(fun p ->
            not (Dir_set.mem env p))
      in
      (match violations with
      | [] -> ()
      | _ :: _ ->
        Code_error.raise
          "Scheme attempted to generate rules in a directory it promised not \
           to touch"
          [ ("directories", (Dyn.Encoder.list Path.Build.to_dyn) violations) ]);
      Evaluated.finite ~union_rules rules
    | Thunk f -> loop ~env (f ())
  in
  fun t -> loop ~env:Dir_set.universal t

let all l = List.fold_left ~init:Empty ~f:(fun x y -> Union (x, y)) l

let evaluate t ~union = evaluate ~union_rules:union t
