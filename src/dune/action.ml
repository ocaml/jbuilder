open! Stdune
open Import
module Outputs = Action_ast.Outputs
module Inputs = Action_ast.Inputs

module Prog = struct
  module Not_found = struct
    type t =
      { context : Context_name.t
      ; program : string
      ; hint : string option
      ; loc : Loc.t option
      }

    let create ?hint ~context ~program ~loc () = { hint; context; program; loc }

    let raise { context; program; hint; loc } =
      let hint =
        match program with
        | "refmt" ->
          Some (Option.value ~default:"try: opam install reason" hint)
        | _ -> hint
      in
      Utils.program_not_found ?hint ~loc ~context program
  end

  type t = (Path.t, Not_found.t) result

  let decode : t Dune_lang.Decoder.t =
    Dune_lang.Decoder.map Dpath.decode ~f:Result.ok

  let encode = function
    | Ok s -> Dpath.encode s
    | Error (e : Not_found.t) -> Dune_lang.Encoder.string e.program
end

module type Ast =
  Action_intf.Ast
    with type program = Prog.t
    with type path = Path.t
    with type target = Path.Build.t
    with type string = String.t

module rec Ast : Ast = Ast

module String_with_sexp = struct
  type t = string

  let decode = Dune_lang.Decoder.string

  let encode = Dune_lang.Encoder.string

  let is_dev_null s = Path.equal (Path.of_string s) Config.dev_null
end

include Action_ast.Make (Prog) (Dpath) (Dpath.Build) (String_with_sexp) (Ast)

type program = Prog.t

type path = Path.t

type target = Path.Build.t

type string = String.t

module For_shell = struct
  module type Ast =
    Action_intf.Ast
      with type program = string
      with type path = string
      with type target = string
      with type string = string

  module rec Ast : Ast = Ast

  include Action_ast.Make (String_with_sexp) (String_with_sexp)
            (String_with_sexp)
            (String_with_sexp)
            (Ast)
end

module Relativise = Action_mapper.Make (Ast) (For_shell.Ast)

let for_shell t =
  let rec loop t ~dir ~f_program ~f_string ~f_path ~f_target =
    match t with
    | Symlink (src, dst) ->
      let src =
        match Path.Build.parent dst with
        | None -> Path.to_string src
        | Some from -> Path.reach ~from:(Path.build from) src
      in
      let dst = Path.reach ~from:dir (Path.build dst) in
      For_shell.Symlink (src, dst)
    | t ->
      Relativise.map_one_step loop t ~dir ~f_program ~f_string ~f_path ~f_target
  in
  loop t ~dir:Path.root
    ~f_string:(fun ~dir:_ x -> x)
    ~f_path:(fun ~dir x -> Path.reach x ~from:dir)
    ~f_target:(fun ~dir x -> Path.reach (Path.build x) ~from:dir)
    ~f_program:(fun ~dir x ->
      match x with
      | Ok p -> Path.reach p ~from:dir
      | Error e -> e.program)

module Unresolved = struct
  module Program = struct
    type t =
      | This of Path.t
      | Search of Loc.t option * string

    let of_string ~dir ~loc s =
      if String.contains s '/' then
        This (Path.relative dir s)
      else
        Search (loc, s)
  end

  module type Uast =
    Action_intf.Ast
      with type program = Program.t
      with type path = Path.t
      with type target = Path.Build.t
      with type string = String.t

  module rec Uast : Uast = Uast

  include Uast
  include Action_mapper.Make (Uast) (Ast)

  let resolve t ~f =
    map t ~dir:Path.root
      ~f_path:(fun ~dir:_ x -> x)
      ~f_target:(fun ~dir:_ x -> x)
      ~f_string:(fun ~dir:_ x -> x)
      ~f_program:(fun ~dir:_ -> function
        | This p -> Ok p
        | Search (loc, s) -> Ok (f loc s))
end

let fold_one_step t ~init:acc ~f =
  match t with
  | Chdir (_, t)
  | Setenv (_, _, t)
  | Redirect_out (_, _, t)
  | Redirect_in (_, _, t)
  | Ignore (_, t)
  | With_accepted_exit_codes (_, t) ->
    f acc t
  | Progn l -> List.fold_left l ~init:acc ~f
  | Run _
  | Dynamic_run _
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
  | Merge_files_into _ ->
    acc

include Action_mapper.Make (Ast) (Ast)

let chdirs =
  let rec loop acc t =
    let acc =
      match t with
      | Chdir (dir, _) -> Path.Set.add acc dir
      | _ -> acc
    in
    fold_one_step t ~init:acc ~f:loop
  in
  fun t -> loop Path.Set.empty t

let empty = Progn []

let rec is_dynamic = function
  | Dynamic_run _ -> true
  | Chdir (_, t)
  | Setenv (_, _, t)
  | Redirect_out (_, _, t)
  | Redirect_in (_, _, t)
  | Ignore (_, t)
  | With_accepted_exit_codes (_, t) ->
    is_dynamic t
  | Progn l -> List.exists l ~f:is_dynamic
  | Run _
  | System _
  | Bash _
  | Echo _
  | Cat _
  | Copy _
  | Symlink _
  | Copy_and_add_line_directive _
  | Write_file _
  | Rename _
  | Remove_tree _
  | Diff _
  | Mkdir _
  | Digest_files _
  | Merge_files_into _ ->
    false

let prepare_managed_paths ~link ~sandboxed deps ~eval_pred =
  let open Fiber.O in
  let+ steps =
    let+ deps = Dep.Set.paths deps ~eval_pred in
    Path.Set.fold deps ~init:[] ~f:(fun path acc ->
        match Path.as_in_build_dir path with
        | None ->
          (* This can actually raise if we try to sandbox the "copy from source
             dir" rules. There is no reason to do that though. *)
          if Path.is_in_source_tree path then
            Code_error.raise
              "Action depends on source tree. All actions should depend on the \
               copies in build directory instead"
              [ ("path", Path.to_dyn path) ];
          acc
        | Some p -> link path (sandboxed p) :: acc)
  in
  Progn steps

let link_function ~(mode : Sandbox_mode.some) : path -> target -> t =
  match mode with
  | Symlink ->
    if Sys.win32 then
      Code_error.raise
        "Don't have symlinks on win32, but [Symlink] sandboxing mode was \
         selected. To use emulation via copy, the [Copy] sandboxing mode \
         should be selected."
        []
    else
      fun a b ->
    Symlink (a, b)
  | Copy -> fun a b -> Copy (a, b)

let maybe_sandbox_path f p =
  match Path.as_in_build_dir p with
  | None -> p
  | Some p -> Path.build (f p)

let sandbox t ~sandboxed ~mode ~deps ~eval_pred : t Fiber.t =
  let link = link_function ~mode in
  let open Fiber.O in
  let+ managed_paths = prepare_managed_paths ~sandboxed ~link deps ~eval_pred in
  Progn
    [ managed_paths
    ; map t ~dir:Path.root
        ~f_string:(fun ~dir:_ x -> x)
        ~f_path:(fun ~dir:_ p -> maybe_sandbox_path sandboxed p)
        ~f_target:(fun ~dir:_ -> sandboxed)
        ~f_program:(fun ~dir:_ -> Result.map ~f:(maybe_sandbox_path sandboxed))
    ]

type is_useful =
  | Clearly_not
  | Maybe

let is_useful_to memoize =
  let rec loop t =
    match t with
    | Chdir (_, t) -> loop t
    | Setenv (_, _, t) -> loop t
    | Redirect_out (_, _, t) -> memoize || loop t
    | Redirect_in (_, _, t) -> loop t
    | Ignore (_, t)
    | With_accepted_exit_codes (_, t) ->
      loop t
    | Progn l -> List.exists l ~f:loop
    | Echo _ -> false
    | Cat _ -> memoize
    | Copy _ -> memoize
    | Symlink _ -> false
    | Copy_and_add_line_directive _ -> memoize
    | Write_file _ -> memoize
    | Rename _ -> memoize
    | Remove_tree _ -> false
    | Diff _ -> memoize
    | Mkdir _ -> false
    | Digest_files _ -> memoize
    | Merge_files_into _ -> memoize
    | Run _ -> true
    | Dynamic_run _ -> true
    | System _ -> true
    | Bash _ -> true
  in
  fun t ->
    match loop t with
    | true -> Maybe
    | false -> Clearly_not

let is_useful_to_sandbox = is_useful_to false

let is_useful_to_memoize = is_useful_to true
