open! Stdune

type program = String_with_vars.t

type string = String_with_vars.t

type path = String_with_vars.t

type target = String_with_vars.t

module String_with_vars = struct
  include String_with_vars

  let is_dev_null t = String_with_vars.is_pform t (Var Dev_null)
end

module type Uast =
  Action_intf.Ast
    with type program = String_with_vars.t
    with type path = String_with_vars.t
    with type target = String_with_vars.t
    with type string = String_with_vars.t

module rec Uast : Uast = Uast

include
  Action_ast.Make (String_with_vars) (String_with_vars) (String_with_vars)
    (String_with_vars)
    (Uast)
module Mapper = Action_mapper.Make (Uast) (Uast)

(* In [Action_exec] we rely on one-to-one mapping between the cwd-relative paths
   seen by the action and [Path.t] seen by dune.

   Having more than one dynamic_run with different cwds could break that. Also,
   we didn't really want to think about how multiple dynamic actions would
   interact (do we want dependencies requested by one to be visible to the
   other?).

   Moreover, we also check that 'dynamic-run' is not used within
   'with-exit-codes', since the meaning of this interaction is not clear. *)
let ensure_at_most_one_dynamic_run ~loc action =
  let rec loop : t -> bool = function
    | Dynamic_run _ -> true
    | Chdir (_, t)
    | Setenv (_, _, t)
    | Redirect_out (_, _, t)
    | Redirect_in (_, _, t)
    | Ignore (_, t)
    | With_accepted_exit_codes (_, t)
    | No_infer t ->
      loop t
    | Run _
    | Echo _
    | Cat _
    | Copy _
    | Symlink _
    | Copy_and_add_line_directive _
    | System _
    | Bash _
    | Write_file _
    | Rename _
    | Remove_tree _
    | Mkdir _
    | Digest_files _
    | Diff _
    | Merge_files_into _
    | Cram _
    | Format_dune_file _ ->
      false
    | Pipe (_, ts)
    | Progn ts ->
      List.fold_left ts ~init:false ~f:(fun acc t ->
          let have_dyn = loop t in
          if acc && have_dyn then
            User_error.raise ~loc
              [ Pp.text
                  "Multiple 'dynamic-run' commands within single action are \
                   not supported."
              ]
          else
            acc || have_dyn)
  in
  ignore (loop action)

let validate ~loc t = ensure_at_most_one_dynamic_run ~loc t

let remove_locs =
  let dir = String_with_vars.make_text Loc.none "" in
  let f_program ~dir:_ = String_with_vars.remove_locs in
  let f_path ~dir:_ = String_with_vars.remove_locs in
  let f_target ~dir:_ = String_with_vars.remove_locs in
  let f_string ~dir:_ = String_with_vars.remove_locs in
  Mapper.map ~dir ~f_program ~f_path ~f_target ~f_string

let compare_no_locs t1 t2 = Poly.compare (remove_locs t1) (remove_locs t2)

open Dune_lang.Decoder

let decode =
  (let+ loc, action = located decode in
   validate ~loc action;
   action)
  <|> let+ loc = loc in
      User_error.raise ~loc
        [ Pp.textf
            "if you meant for this to be executed with bash, write (bash \
             \"...\") instead"
        ]

let to_dyn a = Dune_lang.to_dyn (encode a)
