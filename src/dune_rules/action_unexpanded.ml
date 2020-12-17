open! Dune_engine
open! Stdune
open Import
module Unresolved = Action.Unresolved
module Mapper = Action_mapper.Make (Action_dune_lang) (Action_dune_lang)

let check_mkdir loc path =
  if not (Path.is_managed path) then
    User_error.raise ~loc
      [ Pp.text
          "(mkdir ...) is not supported for paths outside of the workspace:"
      ; Pp.seq (Pp.verbatim "  ")
          (Dune_lang.pp
             (List
                [ Dune_lang.unsafe_atom_of_string "mkdir"; Dpath.encode path ]))
      ]

module Expand (S : sig
  (** Shared code between the expansion passes.

      Creates common expansion for strings, paths, targets, and programs *)
  type 'a input

  type 'a output

  val expand :
       expander:Expander.t
    -> mode:'a String_with_vars.Mode.t
    -> l:('b -> 'c) (** Continuation for when expansion isn't necessary*)
    -> r:(loc:Loc.t option -> 'a -> 'c) (** Expansion continuation *)
    -> 'b input
    -> 'c output
end) : sig
  open S

  type ('i, 'o) expand = expander:Expander.t -> 'i input -> 'o output

  val string : (String.t, String.t) expand

  val strings : (String.t, String.t list) expand

  val path : (Path.t, Path.t) expand

  val target : (Path.Build.t, Path.Build.t) expand

  val prog_and_args :
    (Unresolved.Program.t, Unresolved.Program.t * String.t list) expand

  val cat_strings : ('a, String.t) expand
end = struct
  open S

  type ('i, 'o) expand = expander:Expander.t -> 'i input -> 'o output

  let ignore_loc k ~loc:_ = k

  let expand ~expander ~mode ~l ~r =
    let dir = Path.build (Expander.dir expander) in
    expand ~expander ~mode ~l ~r:(fun ~loc s -> r ~loc s ~dir)

  let string = expand ~mode:Single ~l:Fun.id ~r:(ignore_loc Value.to_string)

  let strings =
    expand ~mode:Many ~l:List.singleton ~r:(ignore_loc Value.L.to_strings)

  let path ~expander e =
    expand ~expander ~mode:Single ~l:Fun.id
      ~r:(fun ~loc v ~dir -> Value.to_path ?error_loc:loc v ~dir)
      e

  let as_in_build_dir ~loc p =
    match Path.as_in_build_dir p with
    | Some p -> p
    | None ->
      User_error.raise ?loc
        [ Pp.textf
            "target %s is outside the build directory. This is not allowed."
            (Path.to_string_maybe_quoted p)
        ]

  let target ~expander e =
    expand e ~expander ~mode:Single ~l:Fun.id ~r:(fun ~loc v ~dir ->
        Value.to_path ?error_loc:loc v ~dir |> as_in_build_dir ~loc)

  let prog_and_args_of_values ~loc p ~dir =
    match p with
    | [] -> (Unresolved.Program.Search (loc, ""), [])
    | Value.Dir p :: _ ->
      User_error.raise ?loc
        [ Pp.textf "%s is a directory and cannot be used as an executable"
            (Path.to_string_maybe_quoted p)
        ]
    | Value.Path p :: xs -> (This p, Value.L.to_strings ~dir xs)
    | String s :: xs ->
      (Unresolved.Program.of_string ~loc ~dir s, Value.L.to_strings ~dir xs)

  let prog_and_args =
    expand ~mode:Many ~l:(fun x -> (x, [])) ~r:prog_and_args_of_values

  let cat_strings ~expander e =
    expand ~expander ~mode:Many
      ~l:(fun _ -> (* This code is never executed *) assert false)
      ~r:(ignore_loc Value.L.concat)
      e
end

module Partial = struct
  module Program = Unresolved.Program

  module type Past =
    Action_intf.Ast
      with type program = Program.t String_with_vars.Partial.t
      with type path = Path.t String_with_vars.Partial.t
      with type target = Path.Build.t String_with_vars.Partial.t
      with type string = String.t String_with_vars.Partial.t

  module rec Past : Past = Past

  include Past

  module E = Expand (struct
    type 'a input = 'a String_with_vars.Partial.t

    type 'a output = 'a

    let expand ~expander ~mode ~l ~r =
      String_with_vars.Partial.elim ~exp:l ~unexp:(fun sw ->
          let x = Expander.expand expander ~template:sw ~mode in
          let loc = Some (String_with_vars.loc sw) in
          r ~loc x)
  end)

  let rec expand t ~expander : Unresolved.t =
    let expand_run prog args =
      let args = List.concat_map args ~f:(E.strings ~expander) in
      let prog, more_args = E.prog_and_args ~expander prog in
      let prog =
        match prog with
        | Search _ -> prog
        | This path -> This (Expander.map_exe expander path)
      in
      (prog, more_args @ args)
    in
    match t with
    | Run (prog, args) ->
      let prog, args = expand_run prog args in
      Run (prog, args)
    | With_accepted_exit_codes (pred, t) ->
      With_accepted_exit_codes (pred, expand ~expander t)
    | Dynamic_run (prog, args) ->
      let prog, args = expand_run prog args in
      Dynamic_run (prog, args)
    | Chdir (fn, t) ->
      let fn = E.path ~expander fn in
      let expander =
        (* TODO this conversion doesn't look safe. It's possible to chdir
           outside the build dir *)
        Expander.set_dir expander ~dir:(Path.as_in_build_dir_exn fn)
      in
      Chdir (fn, expand t ~expander)
    | Setenv (var, value, t) ->
      let var = E.string ~expander var in
      let value = E.string ~expander value in
      let expander = Expander.set_env expander ~var ~value in
      Setenv (var, value, expand t ~expander)
    | Redirect_out (outputs, fn, t) ->
      Redirect_out (outputs, E.target ~expander fn, expand t ~expander)
    | Redirect_in (inputs, fn, t) ->
      Redirect_in (inputs, E.path ~expander fn, expand t ~expander)
    | Ignore (outputs, t) -> Ignore (outputs, expand t ~expander)
    | Progn l -> Progn (List.map l ~f:(expand ~expander))
    | Echo xs -> Echo (List.concat_map xs ~f:(E.strings ~expander))
    | Cat x -> Cat (E.path ~expander x)
    | Copy (x, y) -> Copy (E.path ~expander x, E.target ~expander y)
    | Symlink (x, y) -> Symlink (E.path ~expander x, E.target ~expander y)
    | Copy_and_add_line_directive (x, y) ->
      Copy_and_add_line_directive (E.path ~expander x, E.target ~expander y)
    | System x -> System (E.string ~expander x)
    | Bash x -> Bash (E.string ~expander x)
    | Write_file (x, y, z) ->
      Write_file (E.target ~expander x, E.string ~expander y, z)
    | Rename (x, y) -> Rename (E.target ~expander x, E.target ~expander y)
    | Remove_tree x -> Remove_tree (E.target ~expander x)
    | Mkdir x -> (
      match x with
      | Expanded path -> Mkdir path
      | Unexpanded tmpl ->
        let path = E.path ~expander x in
        check_mkdir (String_with_vars.loc tmpl) path;
        Mkdir path )
    | Digest_files x -> Digest_files (List.map x ~f:(E.path ~expander))
    | Diff { optional; file1; file2; mode } ->
      Diff
        { optional
        ; file1 = E.path ~expander file1
        ; file2 = E.target ~expander file2
        ; mode
        }
    | Merge_files_into (sources, extras, target) ->
      Merge_files_into
        ( List.map ~f:(E.path ~expander) sources
        , List.map ~f:(E.string ~expander) extras
        , E.target ~expander target )
    | No_infer t -> No_infer (expand t ~expander)
    | Pipe (outputs, l) -> Pipe (outputs, List.map l ~f:(expand ~expander))
    | Format_dune_file (ver, src, dst) ->
      Format_dune_file (ver, E.path ~expander src, E.target ~expander dst)
    | Cram script -> Cram (E.path ~expander script)
end

module E = Expand (struct
  type 'a input = String_with_vars.t

  type 'a output = 'a String_with_vars.Partial.t

  let expand ~expander ~mode ~l:_ ~r sw : _ output =
    let dir = Path.build (Expander.dir expander) in
    let f = Expander.expand_var_exn expander in
    String_with_vars.partial_expand ~mode ~dir ~f sw
    |> String_with_vars.Partial.map ~f:(fun x ->
           let loc = Some (String_with_vars.loc sw) in
           r ~loc x)
end)

let rec partial_expand t ~expander : Partial.t =
  let partial_expand_exe prog args =
    let args =
      List.concat_map args ~f:(fun arg ->
          match E.strings ~expander arg with
          | Expanded args -> List.map args ~f:String_with_vars.Partial.expanded
          | Unexpanded _ as x -> [ x ])
    in
    match E.prog_and_args ~expander prog with
    | Expanded (prog, more_args) ->
      let more_args = List.map more_args ~f:String_with_vars.Partial.expanded in
      let prog =
        match prog with
        | Search _ -> prog
        | This path -> This (Expander.map_exe expander path)
      in
      (String_with_vars.Partial.Expanded prog, more_args @ args)
    | Unexpanded _ as prog -> (prog, args)
  in
  match (t : Action_dune_lang.t) with
  | Run (prog, args) ->
    let prog, args = partial_expand_exe prog args in
    Run (prog, args)
  | With_accepted_exit_codes (pred, t) ->
    With_accepted_exit_codes (pred, partial_expand t ~expander)
  | Dynamic_run (prog, args) ->
    let prog, args = partial_expand_exe prog args in
    Dynamic_run (prog, args)
  | Chdir (fn, t) -> (
    let res = E.path ~expander fn in
    match res with
    | Expanded dir ->
      let expander =
        (* TODO this conversion doesn't look safe. It's possible to chdir
           outside the build dir *)
        Expander.set_dir expander ~dir:(Path.as_in_build_dir_exn dir)
      in
      Chdir (res, partial_expand t ~expander)
    | Unexpanded fn ->
      let loc = String_with_vars.loc fn in
      User_error.raise ~loc
        [ Pp.text "This directory cannot be evaluated statically."
        ; Pp.text "This is not allowed by dune"
        ] )
  | Setenv (var, value, t) ->
    let var =
      match E.string ~expander var with
      | Expanded l -> l
      | Unexpanded sw ->
        User_error.raise ~loc:(String_with_vars.loc sw)
          [ Pp.text "environment variable names must be static" ]
    in
    let value = E.string ~expander value in
    let expander =
      match value with
      | Expanded value -> Expander.set_env expander ~var ~value
      | Unexpanded _ -> Expander.hide_env expander ~var
    in
    Setenv (Expanded var, value, partial_expand t ~expander)
  | Redirect_out (outputs, fn, t) ->
    Redirect_out (outputs, E.target ~expander fn, partial_expand t ~expander)
  | Redirect_in (inputs, fn, t) ->
    Redirect_in (inputs, E.path ~expander fn, partial_expand t ~expander)
  | Ignore (outputs, t) -> Ignore (outputs, partial_expand t ~expander)
  | Progn l -> Progn (List.map l ~f:(partial_expand ~expander))
  | Echo xs -> Echo (List.map xs ~f:(E.cat_strings ~expander))
  | Cat x -> Cat (E.path ~expander x)
  | Copy (x, y) -> Copy (E.path ~expander x, E.target ~expander y)
  | Symlink (x, y) -> Symlink (E.path ~expander x, E.target ~expander y)
  | Copy_and_add_line_directive (x, y) ->
    Copy_and_add_line_directive (E.path ~expander x, E.target ~expander y)
  | System x -> System (E.string ~expander x)
  | Bash x -> Bash (E.string ~expander x)
  | Write_file (x, y, z) ->
    Write_file (E.target ~expander x, E.string ~expander y, z)
  | Rename (x, y) -> Rename (E.target ~expander x, E.target ~expander y)
  | Remove_tree x -> Remove_tree (E.target ~expander x)
  | Mkdir x ->
    let res = E.path ~expander x in
    ( match res with
    | Expanded path -> check_mkdir (String_with_vars.loc x) path
    | Unexpanded _ -> () );
    Mkdir res
  | Digest_files x -> Digest_files (List.map x ~f:(E.path ~expander))
  | Diff { optional; file1; file2; mode } ->
    Diff
      { optional
      ; file1 = E.path ~expander file1
      ; file2 = E.target ~expander file2
      ; mode
      }
  | Merge_files_into (sources, extras, target) ->
    Merge_files_into
      ( List.map sources ~f:(E.path ~expander)
      , List.map extras ~f:(E.string ~expander)
      , E.target ~expander target )
  | No_infer t -> No_infer (partial_expand t ~expander)
  | Pipe (outputs, l) -> Pipe (outputs, List.map l ~f:(partial_expand ~expander))
  | Format_dune_file (ver, src, dst) ->
    Format_dune_file (ver, E.path ~expander src, E.target ~expander dst)
  | Cram script -> Cram (E.path ~expander script)

module Infer : sig
  module Outcome : sig
    type t =
      { deps : Path.Set.t
      ; deps_if_exist : Path.Set.t
      ; targets : Path.Build.Set.t
      }
  end

  val unexpanded_targets : Action_dune_lang.t -> String_with_vars.t list

  val infer : Action.t -> Outcome.t

  val partial : Targets.Or_forbidden.t -> Partial.t -> Outcome.t
end = struct
  module Outcome = struct
    type t =
      { deps : Path.Set.t
      ; deps_if_exist : Path.Set.t
      ; targets : Path.Build.Set.t
      }
  end

  open Outcome

  module type Sets = sig
    module Targets : sig
      type t

      val empty : t
    end

    module Deps : sig
      type t

      val empty : t

      val diff : t -> Targets.t -> t
    end
  end

  module type Outcome = sig
    type path_set

    type target_set

    type t =
      { deps : path_set
      ; deps_if_exist : path_set
      ; targets : target_set
      }
  end

  module type Primitives = sig
    type path

    type target

    type program

    type outcome

    val ( +@+ ) : outcome -> target -> outcome

    val ( +< ) : outcome -> path -> outcome

    (** Add a dependency *)
    val ( +<+ ) : outcome -> target -> outcome

    val ( +<! ) : outcome -> program -> outcome

    (** Remove a target *)
    val ( +<- ) : outcome -> target -> outcome

    (** Add an optional (if exists) dependency *)
    val ( +<? ) : outcome -> path -> outcome
  end

  module Make
      (Ast : Action_intf.Ast)
      (Sets : Sets)
      (Out : Outcome
               with type path_set := Sets.Deps.t
                and type target_set := Sets.Targets.t)
      (Prim : Primitives
                with type path := Ast.path
                with type target := Ast.target
                with type program := Ast.program
                with type outcome := Out.t) =
  struct
    open Ast
    open Out
    open Prim

    let rec infer acc t =
      match t with
      | Run (prog, _) -> acc +<! prog
      | With_accepted_exit_codes (_, t) -> infer acc t
      | Dynamic_run (prog, _) -> acc +<! prog
      | Redirect_out (_, fn, t) -> infer (acc +@+ fn) t
      | Redirect_in (_, fn, t) -> infer (acc +< fn) t
      | Cat fn -> acc +< fn
      | Write_file (fn, _, _) -> acc +@+ fn
      | Rename (src, dst) -> acc +<+ src +@+ dst
      | Copy (src, dst)
      | Copy_and_add_line_directive (src, dst)
      | Symlink (src, dst) ->
        acc +< src +@+ dst
      | Chdir (_, t)
      | Setenv (_, _, t)
      | Ignore (_, t) ->
        infer acc t
      | Progn l
      | Pipe (_, l) ->
        List.fold_left l ~init:acc ~f:infer
      | Digest_files l -> List.fold_left l ~init:acc ~f:( +< )
      | Cram script -> acc +< script
      | Diff { optional; file1; file2; mode = _ } ->
        if optional then
          acc +<? file1 +<- file2
        else
          acc +<? file1 +<+ file2
      | Merge_files_into (sources, _extras, target) ->
        List.fold_left sources ~init:acc ~f:( +< ) +@+ target
      | Echo _
      | System _
      | Bash _
      | Remove_tree _
      | Mkdir _
      | No_infer _ ->
        acc
      | Format_dune_file (_, src, dst) -> acc +< src +@+ dst

    let infer t =
      let { deps; deps_if_exist; targets } =
        infer
          { deps = Sets.Deps.empty
          ; deps_if_exist = Sets.Deps.empty
          ; targets = Sets.Targets.empty
          }
          t
      in

      (* A file can be inferred as both a dependency and a target, for instance:

         {[ (progn (copy a b) (copy b c)) ]} *)
      { deps = Sets.Deps.diff deps targets
      ; deps_if_exist = Sets.Deps.diff deps_if_exist targets
      ; targets
      }
  end
  [@@inline always]

  module Sets = struct
    module Targets = Path.Build.Set

    module Deps = struct
      include Path.Set

      let diff deps targets =
        Path.Build.Set.fold targets ~init:deps ~f:(fun target acc ->
            Path.Set.remove acc (Path.build target))
    end
  end

  include Make (Action.Ast) (Sets) (Outcome)
            (struct
              let ( +@+ ) acc fn =
                { acc with targets = Path.Build.Set.add acc.targets fn }

              let ( +< ) acc fn = { acc with deps = Path.Set.add acc.deps fn }

              let ( +<+ ) acc fn =
                { acc with deps = Path.Set.add acc.deps (Path.build fn) }

              let ( +<? ) acc fn =
                { acc with deps_if_exist = Path.Set.add acc.deps_if_exist fn }

              let ( +<- ) acc fn =
                { acc with targets = Path.Build.Set.remove acc.targets fn }

              let ( +<! ) acc prog =
                match prog with
                | Ok p -> acc +< p
                | Error _ -> acc
            end)

  module Partial_with_all_targets =
    Make (Partial.Past) (Sets) (Outcome)
      (struct
        let ( +@+ ) acc (fn : _ String_with_vars.Partial.t) =
          match fn with
          | Expanded fn ->
            { acc with targets = Path.Build.Set.add acc.targets fn }
          | Unexpanded sw ->
            User_error.raise ~loc:(String_with_vars.loc sw)
              [ Pp.text "Cannot determine this target statically." ]

        let ( +< ) acc (fn : _ String_with_vars.Partial.t) =
          match fn with
          | Expanded fn -> { acc with deps = Path.Set.add acc.deps fn }
          | Unexpanded _ -> acc

        let ( +<+ ) acc (fn : _ String_with_vars.Partial.t) =
          match fn with
          | Expanded fn ->
            { acc with deps = Path.Set.add acc.deps (Path.build fn) }
          | Unexpanded _ -> acc

        let ( +<? ) acc (fn : _ String_with_vars.Partial.t) =
          match fn with
          | Expanded fn ->
            { acc with deps_if_exist = Path.Set.add acc.deps_if_exist fn }
          | Unexpanded _ -> acc

        let ( +<- ) acc (fn : _ String_with_vars.Partial.t) =
          match fn with
          | Expanded fn ->
            { acc with targets = Path.Build.Set.remove acc.targets fn }
          | Unexpanded _ -> acc

        let ( +<! ) acc fn =
          match (fn : Partial.program) with
          | Expanded (This fn) -> { acc with deps = Path.Set.add acc.deps fn }
          | Expanded (Search _)
          | Unexpanded _ ->
            acc
      end)

  module Partial =
    Make (Partial.Past) (Sets) (Outcome)
      (struct
        let ( +@+ ) acc (fn : _ String_with_vars.Partial.t) =
          match fn with
          | Expanded fn ->
            { acc with targets = Path.Build.Set.add acc.targets fn }
          | Unexpanded _ -> acc

        let ( +< ) acc (fn : _ String_with_vars.Partial.t) =
          match fn with
          | Expanded fn -> { acc with deps = Path.Set.add acc.deps fn }
          | Unexpanded _ -> acc

        let ( +<+ ) acc (fn : _ String_with_vars.Partial.t) =
          match fn with
          | Expanded fn ->
            { acc with deps = Path.Set.add acc.deps (Path.build fn) }
          | Unexpanded _ -> acc

        let ( +<? ) acc (fn : _ String_with_vars.Partial.t) =
          match fn with
          | Expanded fn ->
            { acc with deps_if_exist = Path.Set.add acc.deps_if_exist fn }
          | Unexpanded _ -> acc

        let ( +<- ) acc (fn : _ String_with_vars.Partial.t) =
          match fn with
          | Expanded fn ->
            { acc with targets = Path.Build.Set.remove acc.targets fn }
          | Unexpanded _ -> acc

        let ( +<! ) acc fn =
          match (fn : Partial.program) with
          | Expanded (This fn) -> { acc with deps = Path.Set.add acc.deps fn }
          | Expanded (Search _)
          | Unexpanded _ ->
            acc
      end)

  let partial (targets : Targets.Or_forbidden.t) t =
    match targets with
    | Targets Infer -> Partial_with_all_targets.infer t
    | Forbidden _ ->
      (* Q: why don't we make sure that no targets were inferred?

         A: Target detection is not robust and sufffers from false
         positive/negatives. *)
      { (Partial.infer t) with targets = Path.Build.Set.empty }
    | Targets (Static { targets = written_by_user; multiplicity = _ }) ->
      let outcome = Partial.infer t in
      { outcome with
        targets =
          Path.Build.Set.union outcome.targets
            (Path.Build.Set.of_list written_by_user)
      }

  module S_unexp = struct
    module Targets = struct
      type t = String_with_vars.t list

      let empty = []
    end

    module Deps = struct
      include Targets

      let diff a _ = a
    end
  end

  module Outcome_unexp = struct
    type t =
      { deps : S_unexp.Deps.t
      ; deps_if_exist : S_unexp.Deps.t
      ; targets : S_unexp.Targets.t
      }
  end

  module Unexp =
    Make (Action_dune_lang) (S_unexp) (Outcome_unexp)
      (struct
        open Outcome_unexp

        let ( +@+ ) acc fn =
          if String_with_vars.is_var fn ~name:"null" then
            acc
          else
            { acc with targets = fn :: acc.targets }

        let ( +< ) acc _ = acc

        let ( +<+ ) acc _ = acc

        let ( +<? ) acc _ = acc

        let ( +<! ) = ( +< )

        let ( +<- ) acc fn =
          if String_with_vars.is_var fn ~name:"null" then
            acc
          else
            { acc with targets = List.filter acc.targets ~f:(fun t -> t <> fn) }
      end)

  let unexpanded_targets t = (Unexp.infer t).targets
end

let add_deps_if_exist deps_if_exist =
  let open Build.O in
  (let+ l =
     Path.Set.to_list deps_if_exist
     |> List.map
          ~f:
            Build.(
              fun f ->
                if_file_exists f ~then_:(return (Some f)) ~else_:(return None))
     |> Build.all
   in
   List.filter_opt l)
  |> Build.dyn_paths_unit

let expand t ~loc ~dep_kind ~targets_dir ~targets:targets_written_by_user
    ~expander deps_written_by_user =
  let open Build.O in
  ( match (targets_written_by_user : Targets.Or_forbidden.t) with
  | Targets _ -> ()
  | Forbidden context -> (
    match Infer.unexpanded_targets t with
    | [] -> ()
    | x :: _ ->
      let loc = String_with_vars.loc x in
      User_error.raise ~loc
        [ Pp.textf "%s must not have targets." (String.capitalize context) ] )
  );
  let partially_expanded, fully_expanded =
    Expander.expand_action expander ~dep_kind ~deps_written_by_user
      ~targets_written_by_user
      ~partial:(fun expander -> partial_expand t ~expander)
      ~final:(fun expander t -> Partial.expand t ~expander)
  in
  let { Infer.Outcome.deps; deps_if_exist; targets } =
    Infer.partial targets_written_by_user partially_expanded
  in
  Path.Build.Set.iter targets ~f:(fun target ->
      if Path.Build.( <> ) (Path.Build.parent_exn target) targets_dir then
        User_error.raise ~loc
          [ Pp.text
              "This action has targets in a different directory than the \
               current one, this is not allowed by dune at the moment:"
          ; Pp.enumerate (Path.Build.Set.to_list targets) ~f:(fun target ->
                Pp.text (Dpath.describe_path (Path.build target)))
          ]);
  let targets = Path.Build.Set.to_list targets in
  Build.path_set deps
  >>> add_deps_if_exist deps_if_exist
  >>> Build.dyn_path_set
        (let+ (action, deps), deps_if_exist_which_exist =
           Build.filter_existing_files
             (let+ action =
                let+ unresolved = fully_expanded in
                let artifacts = Expander.artifacts expander in
                Action.Unresolved.resolve unresolved ~f:(fun loc prog ->
                    Artifacts.Bin.binary artifacts ~loc prog
                    |> Action.Prog.ok_exn)
              in

              (* Targets cannot be introduced at this stage because they must
                 all be expanded after partial expansion.

                 This is because we don't allow things like: [(with-stdout-to
                 %{read:foo} ..)] *)
              let { Infer.Outcome.deps
                  ; Infer.Outcome.deps_if_exist
                  ; targets = _
                  } =
                Infer.infer action
              in
              let dir = Path.build (Expander.dir expander) in
              ((Action.Chdir (dir, action), deps), deps_if_exist))
         in
         (action, Path.Set.union deps deps_if_exist_which_exist))
  |> Build.with_targets ~targets

(* We re-export [Action_dune_lang] in the end to avoid polluting the inferred
   types in this module with all the various t's *)
include Action_dune_lang
