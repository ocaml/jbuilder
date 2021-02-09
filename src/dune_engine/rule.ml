open! Stdune
open Import

module Info = struct
  type t =
    | From_dune_file of Loc.t
    | Internal
    | Source_file_copy

  let of_loc_opt = function
    | None -> Internal
    | Some loc -> From_dune_file loc
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

module Id = Id.Make ()

module T = struct
  type t =
    { id : Id.t
    ; context : Build_context.t option
    ; env : Env.t option
    ; action : Action.t Action_builder.With_targets.t
    ; mode : Mode.t
    ; locks : Path.t list
    ; info : Info.t
    ; dir : Path.Build.t
    }

  let compare a b = Id.compare a.id b.id

  let equal a b = Id.equal a.id b.id

  let hash t = Id.hash t.id

  let loc t =
    match (t.info : Info.t) with
    | From_dune_file loc -> loc
    | Internal
    | Source_file_copy ->
      let dir = Path.drop_optional_build_context_src_exn (Path.build t.dir) in
      let file =
        match
          Option.bind (File_tree.find_dir dir) ~f:File_tree.Dir.dune_file
        with
        | Some file -> File_tree.Dune_file.path file
        | None -> Path.Source.relative dir "_unknown_"
      in
      Loc.in_file (Path.source file)

  let to_dyn t : Dyn.t =
    Record [ ("id", Id.to_dyn t.id); ("loc", Loc.to_dyn_hum (loc t)) ]
end

include T
module O = Comparable.Make (T)
module Set = O.Set

let make ?(sandbox = Sandbox_config.default) ?(mode = Mode.Standard) ~context
    ~env ?(locks = []) ?(info = Info.Internal) action =
  let open Action_builder.With_targets.O in
  let action =
    Action_builder.With_targets.memoize "Rule.make"
      (Action_builder.with_no_targets
         (Action_builder.dep (Dep.sandbox_config sandbox))
      >>> action)
  in
  let targets = action.targets in
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
        | Source_file_copy ->
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
  { id = Id.gen (); context; env; action; mode; locks; info; dir }

let with_prefix t ~build =
  { t with
    action =
      (let open Action_builder.With_targets.O in
      Action_builder.With_targets.memoize "Rule.with_prefix"
        (Action_builder.with_no_targets build >>> t.action))
  }

let effective_env t =
  match (t.env, t.context) with
  | None, None -> Env.initial
  | Some e, _ -> e
  | None, Some c -> c.env

let find_source_dir rule =
  let _, src_dir = Path.Build.extract_build_context_dir_exn rule.dir in
  let res = File_tree.nearest_dir src_dir in
  res
