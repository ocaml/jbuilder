open! Stdune
open Import
module Action_builder = Action_builder0

module Info = struct
  type t =
    | From_dune_file of Loc.t
    | Internal
    | Source_file_copy of Path.Source.t

  let of_loc_opt = function
    | None -> Internal
    | Some loc -> From_dune_file loc

  let to_dyn : t -> Dyn.t = function
    | From_dune_file loc -> Dyn.Variant ("From_dune_file", [ Loc.to_dyn loc ])
    | Internal -> Dyn.Variant ("Internal", [])
    | Source_file_copy p ->
      Dyn.Variant ("Source_file_copy", [ Path.Source.to_dyn p ])
end

module Promote = struct
  module Lifetime = struct
    type t =
      | Unlimited
      | Until_clean
  end

  module Into = struct
    type t =
      { loc : Loc.t
      ; dir : string
      }
  end

  type t =
    { lifetime : Lifetime.t
    ; into : Into.t option
    ; only : Predicate_lang.Glob.t option
    }
end

module Mode = struct
  type t =
    | Standard
    | Fallback
    | Promote of Promote.t
    | Ignore_source_files
end

type t =
  { context : Build_context.t option
  ; targets : Path.Build.Set.t
  ; action : Action.Full.t Action_builder.t
  ; mode : Mode.t
  ; info : Info.t
  ; loc : Loc.t
  ; dir : Path.Build.t
  }

let loc t = t.loc

let to_dyn t : Dyn.t = Record [ ("info", Info.to_dyn t.info) ]

let head_target t = Path.Build.Set.min_elt t.targets |> Option.value_exn

let add_sandbox_config :
    type a.
    a Action_builder.eval_mode -> Sandbox_config.t -> a Dep.Map.t -> a Dep.Map.t
    =
 fun mode sandbox map ->
  let dep = Dep.sandbox_config sandbox in
  match mode with
  | Lazy -> Dep.Set.add map dep
  | Eager -> Dep.Map.set map dep Dep.Fact.nothing

let make ?(sandbox = Sandbox_config.default) ?(mode = Mode.Standard) ~context
    ?(info = Info.Internal) ~targets action =
  let open Memo.Build.O in
  let action =
    Action_builder.memoize "Rule.make"
      (Action_builder.of_thunk
         { f =
             (fun mode ->
               let+ action, deps = Action_builder.run action mode in
               let deps = add_sandbox_config mode sandbox deps in
               (action, deps))
         })
  in
  let dir =
    match Path.Build.Set.choose targets with
    | None -> (
      match info with
      | From_dune_file loc ->
        User_error.raise ~loc [ Pp.text "Rule has no targets specified" ]
      | _ -> Code_error.raise "Build_interpret.Rule.make: no targets" [])
    | Some x ->
      let dir = Path.Build.parent_exn x in
      (if
       Path.Build.Set.exists targets ~f:(fun path ->
           Path.Build.( <> ) (Path.Build.parent_exn path) dir)
      then
        match info with
        | Internal
        | Source_file_copy _ ->
          Code_error.raise "rule has targets in different directories"
            [ ("targets", Path.Build.Set.to_dyn targets) ]
        | From_dune_file loc ->
          User_error.raise ~loc
            [ Pp.text "Rule has targets in different directories.\nTargets:"
            ; Pp.enumerate (Path.Build.Set.to_list targets) ~f:(fun p ->
                  Pp.verbatim (Path.to_string_maybe_quoted (Path.build p)))
            ]);
      dir
  in
  let loc =
    match info with
    | From_dune_file loc -> loc
    | Internal ->
      Loc.in_file
        (Path.drop_optional_build_context
           (Path.build (Path.Build.relative dir "_unknown_")))
    | Source_file_copy p -> Loc.in_file (Path.source p)
  in
  { targets; context; action; mode; info; loc; dir }

let set_action t action =
  let action = Action_builder.memoize "Rule.set_action" action in
  { t with action }

let find_source_dir rule =
  let _, src_dir = Path.Build.extract_build_context_dir_exn rule.dir in
  Source_tree.nearest_dir src_dir

module Anonymous_action = struct
  type t =
    { context : Build_context.t option
    ; action : Action.Full.t
    ; loc : Loc.t option
    ; dir : Path.Build.t
    ; alias : Alias.Name.t option
    }
end
