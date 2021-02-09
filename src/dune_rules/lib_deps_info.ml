open! Stdune
open Dune_engine

module Kind = struct
  type t =
    | Optional
    | Required

  let merge a b =
    match (a, b) with
    | Optional, Optional -> Optional
    | _ -> Required

  let of_optional b =
    if b then
      Optional
    else
      Required

  let to_dyn t =
    Dyn.String
      (match t with
      | Optional -> "optional"
      | Required -> "required")
end

type t = Kind.t Lib_name.Map.t

let merge a b =
  Lib_name.Map.merge a b ~f:(fun _ a b ->
      match (a, b) with
      | None, None -> None
      | x, None
      | None, x ->
        x
      | Some a, Some b -> Some (Kind.merge a b))

let to_dyn = Lib_name.Map.to_dyn Kind.to_dyn

type Action_builder.label += Label of t

let lib_deps t : t =
  Action_builder.fold_labeled t ~init:Lib_name.Map.empty ~f:(fun r acc ->
      match r with
      | Label u -> merge u acc
      | _ -> acc)
