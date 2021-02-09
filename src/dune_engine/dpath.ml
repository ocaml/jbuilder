open Stdune

let install_dir_basename = "install"

let alias_dir_basename = ".aliases"

let install_dir = Path.Build.(relative root) install_dir_basename

let alias_dir = Path.Build.(relative root) alias_dir_basename

type target_kind =
  | Regular of Context_name.t * Path.Source.t
  | Alias of Context_name.t * Path.Source.t
  | Install of Context_name.t * Path.Source.t
  | Other of Path.Build.t

module Target_dir = struct
  type context_related =
    | Root
    | With_context of Context_name.t * Path.Source.t

  let build_dir = function
    | Root -> Path.Build.root
    | With_context (ctx, s) ->
      Path.Build.append_source (Context_name.build_dir ctx) s

  type t =
    | Install of context_related
    | Alias of context_related
    | Regular of context_related
    | Invalid of Path.Build.t

  (* _build/foo or _build/install/foo where foo is invalid *)

  let of_target (fn as original_fn) =
    match Path.Build.extract_first_component fn with
    | None -> Regular Root
    | Some (name, sub) -> (
      if name = alias_dir_basename then
        match Path.Local.split_first_component sub with
        | None -> Alias Root
        | Some (ctx, fn) ->
          let ctx = Context_name.of_string ctx in
          Alias (With_context (ctx, Path.Source.of_local fn))
      else if name = install_dir_basename then
        match Path.Local.split_first_component sub with
        | None -> Install Root
        | Some (ctx, fn) -> (
          match Context_name.of_string_opt ctx with
          | None -> Invalid original_fn
          | Some ctx -> Install (With_context (ctx, Path.Source.of_local fn)))
      else
        match Context_name.of_string_opt name with
        | None -> Invalid fn
        | Some ctx -> Regular (With_context (ctx, Path.Source.of_local sub)))
end

type 'build path_kind =
  | Source of Path.Source.t
  | External of Path.External.t
  | Build of 'build

let analyse_target (fn as original_fn) : target_kind =
  match Target_dir.of_target fn with
  | Invalid _
  | Alias Root
  | Install Root
  | Regular Root ->
    Other fn
  | Install (With_context (ctx, src_dir)) -> Install (ctx, src_dir)
  | Regular (With_context (ctx, src_dir)) -> Regular (ctx, src_dir)
  | Alias (With_context (ctx, fn)) -> (
    match String.rsplit2 (Path.Source.basename fn) ~on:'-' with
    | None -> assert false
    | Some (basename, digest) ->
      if String.length digest = 32 then
        Alias (ctx, Path.Source.relative (Path.Source.parent_exn fn) basename)
      else
        Other original_fn)

let describe_target fn =
  let ctx_suffix name =
    if Context_name.is_default name then
      ""
    else
      sprintf " (context %s)" (Context_name.to_string name)
  in
  match analyse_target fn with
  | Alias (ctx, p) ->
    sprintf "alias %s%s" (Path.Source.to_string_maybe_quoted p) (ctx_suffix ctx)
  | Install (ctx, p) ->
    sprintf "install %s%s"
      (Path.Source.to_string_maybe_quoted p)
      (ctx_suffix ctx)
  | Regular (ctx, fn) ->
    sprintf "%s%s" (Path.Source.to_string_maybe_quoted fn) (ctx_suffix ctx)
  | Other fn -> Path.Build.to_string_maybe_quoted fn

let describe_path (p : Path.t) =
  match p with
  | External _
  | In_source_tree _ ->
    Path.to_string_maybe_quoted p
  | In_build_dir p -> describe_target p

let analyse_path (fn : Path.t) =
  match fn with
  | In_source_tree src -> Source src
  | External e -> External e
  | In_build_dir build -> Build (analyse_target build)

let analyse_dir (fn : Path.t) =
  match fn with
  | In_source_tree src -> Source src
  | External e -> External e
  | In_build_dir build -> Build (Target_dir.of_target build)

type t = Path.t

let encode p =
  let make constr arg =
    Dune_lang.List
      [ Dune_lang.atom constr; Dune_lang.atom_or_quoted_string arg ]
  in
  let open Path in
  match p with
  | In_build_dir p -> make "In_build_dir" (Path.Build.to_string p)
  | In_source_tree p -> make "In_source_tree" (Path.Source.to_string p)
  | External p -> make "External" (Path.External.to_string p)

let decode =
  let open Dune_lang.Decoder in
  let external_ =
    plain_string (fun ~loc t ->
        if Filename.is_relative t then
          User_error.raise ~loc [ Pp.text "Absolute path expected" ]
        else
          Path.parse_string_exn ~loc t)
  in
  sum
    [ ("In_build_dir", string >>| Path.(relative build_dir))
    ; ("In_source_tree", string >>| Path.(relative root))
    ; ("External", external_)
    ]

module Local = struct
  let encode ~dir:from p =
    let open Dune_lang.Encoder in
    string (Path.reach ~from p)

  let decode ~dir =
    let open Dune_lang.Decoder in
    let+ error_loc, path = located string in
    Path.relative ~error_loc dir path
end

module Build = struct
  type t = Path.Build.t

  let encode p =
    let str = Path.reach ~from:Path.build_dir (Path.build p) in
    Dune_lang.atom_or_quoted_string str

  let decode =
    let open Dune_lang.Decoder in
    let+ base = string in
    Path.Build.(relative root) base

  let is_dev_null = Fun.const false

  let install_dir = install_dir

  let alias_dir = alias_dir

  let is_alias_stamp_file =
    let prefix = Path.Build.basename alias_dir ^ "/" in
    fun s ->
      String.is_prefix (Path.Local.to_string (Path.Build.local s)) ~prefix
end

module External = struct
  let encode p = Dune_lang.Encoder.string (Path.External.to_string p)

  let decode =
    let open Dune_lang.Decoder in
    let+ path = string in
    Path.External.of_string path
end
