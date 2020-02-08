open! Stdune

module Id = Id.Make ()

module Dir_rules = struct
  type alias_action =
    { stamp : Digest.t
    ; action : Action.t Build.With_targets.t
    ; locks : Path.t list
    ; context : Context.t
    ; env : Env.t option
    ; loc : Loc.t option
    }

  module Alias_spec = struct
    type t =
      { deps : Path.Set.t
      ; dyn_deps : Path.Set.t Build.t
      ; actions : alias_action Appendable_list.t
      }

    let empty =
      { deps = Path.Set.empty
      ; dyn_deps = Build.return Path.Set.empty
      ; actions = Appendable_list.empty
      }

    let union x y =
      { deps = Path.Set.union x.deps y.deps
      ; dyn_deps = Build.map2 x.dyn_deps y.dyn_deps ~f:Path.Set.union
      ; actions = Appendable_list.( @ ) x.actions y.actions
      }
  end

  type alias =
    { name : Alias.Name.t
    ; spec : Alias_spec.t
    }

  type data =
    | Rule of Rule.t
    | Alias of alias

  type t = data Id.Map.t

  let data_to_dyn = function
    | Rule rule ->
      Dyn.Variant
        ( "Rule"
        , [ Record [ ("targets", Path.Build.Set.to_dyn rule.action.targets) ] ]
        )
    | Alias alias ->
      Dyn.Variant
        ("Alias", [ Record [ ("name", Alias.Name.to_dyn alias.name) ] ])

  let to_dyn t = Dyn.Encoder.(list data_to_dyn) (Id.Map.values t)

  type ready =
    { rules : Rule.t list
    ; aliases : Alias_spec.t Alias.Name.Map.t
    }

  let consume t =
    let data = List.map ~f:snd (Id.Map.to_list t) in
    let rules =
      List.filter_map data ~f:(function
        | Rule rule -> Some rule
        | Alias _ -> None)
    in
    let aliases =
      Alias.Name.Map.of_list_multi
        (List.filter_map data ~f:(function
          | Rule _ -> None
          | Alias { name; spec } -> Some (name, spec)))
      |> Alias.Name.Map.map ~f:(fun specs ->
             List.fold_left specs ~init:Alias_spec.empty ~f:Alias_spec.union)
    in
    { rules; aliases }

  let empty = Id.Map.empty

  let union_map a b ~f = Id.Map.union a b ~f:(fun _key a b -> Some (f a b))

  let union =
    union_map ~f:(fun a b ->
        assert (a == b);
        a)

  let singleton (data : data) =
    let id = Id.gen () in
    Id.Map.singleton id data

  let is_subset t ~of_ = Id.Map.is_subset t ~of_ ~f:(fun _ ~of_:_ -> true)

  let is_empty = Id.Map.is_empty

  module Nonempty : sig
    type maybe_empty = t

    type t = private maybe_empty

    val create : maybe_empty -> t option

    val union : t -> t -> t

    val singleton : data -> t
  end = struct
    type maybe_empty = t

    type nonrec t = t

    let create t =
      if is_empty t then
        None
      else
        Some t

    let union = union

    let singleton = singleton
  end
end

module T = struct
  type t = Dir_rules.Nonempty.t Path.Build.Map.t

  let empty = Path.Build.Map.empty

  let union_map a b ~f =
    Path.Build.Map.union a b ~f:(fun _key a b -> Some (f a b))

  let union = union_map ~f:Dir_rules.Nonempty.union

  let name = "Rules"
end

include T

let singleton_rule (rule : Rule.t) =
  let dir = rule.dir in
  Path.Build.Map.singleton dir (Dir_rules.Nonempty.singleton (Rule rule))

let implicit_output = Memo.Implicit_output.add (module T)

let produce = Memo.Implicit_output.produce implicit_output

let produce_opt = Memo.Implicit_output.produce_opt implicit_output

module Produce = struct
  let rule rule = produce (singleton_rule rule)

  module Alias = struct
    type t = Alias.t

    let alias t spec =
      produce
        (let dir = Alias.dir t in
         let name = Alias.name t in
         Path.Build.Map.singleton dir
           (Dir_rules.Nonempty.singleton (Alias { name; spec })))

    let add_deps t ?(dyn_deps = Build.return Path.Set.empty) deps =
      alias t { deps; dyn_deps; actions = Appendable_list.empty }

    let add_action t ~context ~env ~loc ?(locks = []) ~stamp action =
      alias t
        { deps = Path.Set.empty
        ; dyn_deps = Build.return Path.Set.empty
        ; actions =
            Appendable_list.singleton
              ( { stamp = Digest.generic stamp
                ; action
                ; locks
                ; context
                ; loc
                ; env
                }
                : Dir_rules.alias_action )
        }
  end
end

let produce_dir ~dir rules =
  match Dir_rules.Nonempty.create rules with
  | None -> ()
  | Some rules -> produce (Path.Build.Map.singleton dir rules)

let produce_dir' ~dir rules =
  let dir = Path.as_in_build_dir_exn dir in
  produce_dir ~dir rules

let collect_opt_async f = Memo.Implicit_output.collect_async implicit_output f

let collect_async f =
  let open Fiber.O in
  let+ result, out = collect_opt_async f in
  (result, Option.value out ~default:T.empty)

let collect_opt f = Memo.Implicit_output.collect_sync implicit_output f

let collect f =
  let result, out = collect_opt f in
  (result, Option.value out ~default:T.empty)

let collect_unit f =
  let (), rules = collect f in
  rules

let to_map x = (x : t :> Dir_rules.t Path.Build.Map.t)

let map t ~f =
  Path.Build.Map.map t ~f:(fun m ->
      Id.Map.to_list (m : Dir_rules.Nonempty.t :> Dir_rules.t)
      |> List.map ~f:(fun (id, data) ->
             match f data with
             | `No_change -> (id, data)
             | `Changed data -> (Id.gen (), data))
      |> Id.Map.of_list_exn |> Dir_rules.Nonempty.create |> Option.value_exn)

let is_subset t ~of_ =
  Path.Build.Map.is_subset (to_map t) ~of_:(to_map of_) ~f:Dir_rules.is_subset

let map_rules t ~f =
  map t ~f:(function
    | (Alias _ : Dir_rules.data) -> `No_change
    | Rule r -> `Changed (Rule (f r) : Dir_rules.data))

let find t p =
  match Path.as_in_build_dir p with
  | None -> Dir_rules.empty
  | Some p -> (
    match Path.Build.Map.find t p with
    | Some dir_rules -> (dir_rules : Dir_rules.Nonempty.t :> Dir_rules.t)
    | None -> Dir_rules.empty )
