open! Stdune
open Import
open Build.O
open! No_io

module SC = Super_context

let warn_dropped_pp loc ~allow_approx_merlin ~reason =
  if not allow_approx_merlin then
    Errors.warn loc
      ".merlin generated is inaccurate. %s.\n\
        Split the stanzas into different directories or silence this warning \
        by adding (allow_approximate_merlin) to your dune-project."
      reason

module Preprocess = struct

  let merge ~allow_approx_merlin
        (a : Dune_file.Preprocess.t) (b : Dune_file.Preprocess.t) =
    match a, b with
    (* the 2 cases below aren't entirely correct as it means that we have merlin
       preprocess files that don't need to be preprocessed *)
    | No_preprocessing, pp
    | pp, No_preprocessing -> pp
    | (Future_syntax _ as future_syntax), _
    | _, (Future_syntax _ as future_syntax) -> future_syntax
    | Action (loc, a1), Action (_, a2) ->
      if Action_dune_lang.compare_no_locs a1 a2 <> Ordering.Eq then
        warn_dropped_pp loc ~allow_approx_merlin
          ~reason:"this action preprocessor is not equivalent to other \
                   preproocessor specifications.";
      Action (loc, a1)
    | Pps _, Action (loc, _)
    | Action (loc, _), Pps _ ->
      warn_dropped_pp loc ~allow_approx_merlin
        ~reason:"cannot mix action and pps preprocessors";
      No_preprocessing
    | Pps { loc ; pps = pps1; flags = flags1; staged = s1 },
      Pps { loc = _; pps = pps2; flags = flags2; staged = s2 } ->
      if Bool.(<>) s1 s2
      || List.compare flags1 flags2 ~compare:String.compare <> Eq
      || List.compare pps1 pps2 ~compare:(fun (_, x) (_, y) ->
        Lib_name.compare x y) <> Eq
      then
        warn_dropped_pp loc ~allow_approx_merlin
          ~reason:"pps specification isn't identical in all stanzas";
      No_preprocessing
end

let quote_for_merlin s =
  let s =
    if Sys.win32 then
      (* We need this hack because merlin unescapes backslashes (except when
         protected by single quotes). It is only a problem on windows
         because Filename.quote is using double quotes. *)
      String.escape_only '\\' s
    else
      s
  in
  if need_quoting s then
    Filename.quote s
  else
    s

module Dot_file = struct
  let b = Buffer.create 256

  let printf = Printf.bprintf b
  let print = Buffer.add_string b

  let to_string ~obj_dirs ~src_dirs ~flags ~pp ~remaindir =
    let serialize_path = Path.reach ~from:remaindir in
    Buffer.clear b;
    print "EXCLUDE_QUERY_DIR\n";
    Path.Set.iter obj_dirs ~f:(fun p ->
      printf "B %s\n" (serialize_path p));
    Path.Set.iter src_dirs ~f:(fun p ->
      printf "S %s\n" (serialize_path p));
    Option.iter pp ~f:(printf "%s\n");
    begin match flags with
    | [] -> ()
    | flags ->
      print "FLG";
      List.iter flags ~f:(fun f -> printf " %s" (quote_for_merlin f));
      print "\n"
    end;
    Buffer.contents b
end

type t =
  { requires   : Lib.Set.t
  ; flags      : (unit, string list) Build.t
  ; preprocess : Dune_file.Preprocess.t
  ; libname    : Lib_name.Local.t option
  ; source_dirs: Path.Set.t
  ; objs_dirs  : Path.Set.t
  }

let make
      ?(requires=Ok [])
      ?(flags=Build.return [])
      ?(preprocess=Dune_file.Preprocess.No_preprocessing)
      ?libname
      ?(source_dirs=Path.Set.empty)
      ?(objs_dirs=Path.Set.empty)
      () =
  (* Merlin shouldn't cause the build to fail, so we just ignore errors *)
  let requires =
    match requires with
    | Ok    l -> Lib.Set.of_list l
    | Error _ -> Lib.Set.empty
  in
  { requires
  ; flags      = Build.catch flags    ~on_error:(fun _ -> [])
  ; preprocess
  ; libname
  ; source_dirs
  ; objs_dirs
  }

let add_source_dir t dir =
  { t with source_dirs = Path.Set.add t.source_dirs dir }

let pp_flags sctx ~expander ~dir_kind { preprocess; libname; _ } =
  let scope = Expander.scope expander in
  match Dune_file.Preprocess.remove_future_syntax preprocess
          (Super_context.context sctx).version
  with
  | Pps { loc = _; pps; flags; staged = _ } -> begin
    match Preprocessing.get_ppx_driver sctx ~scope ~dir_kind pps with
    | Error _ -> None
    | Ok exe ->
      (Path.to_absolute_filename exe
       :: "--as-ppx"
       :: Preprocessing.cookie_library_name libname
       @ flags)
      |> List.map ~f:quote_for_merlin
      |> String.concat ~sep:" "
      |> Filename.quote
      |> sprintf "FLG -ppx %s"
      |> Option.some
  end
  | Action (_, (action : Action_dune_lang.t)) ->
    begin match action with
    | Run (exe, args) ->
      let open Option.O in
      List.destruct_last args >>= (fun (args, input_file) ->
        if String_with_vars.is_var input_file ~name:"input-file" then
          Some args
        else
          None)
      >>= fun args ->
      Expander.Option.expand_path expander exe >>= fun exe ->
      List.map ~f:(Expander.Option.expand_str expander) args
      |> Option.List.all
      >>= fun args ->
      (Path.to_absolute_filename exe :: args)
      |> List.map ~f:quote_for_merlin
      |> String.concat ~sep:" "
      |> Filename.quote
      |> sprintf "FLG -pp %s"
      |> Option.some
    | _ -> None
    end
  | No_preprocessing ->
    None

let dot_merlin sctx ~dir ~more_src_dirs ~expander ~dir_kind
      ({ requires; flags; _ } as t) =
  match Path.drop_build_context dir with
  | None -> ()
  | Some remaindir ->
    let merlin_file = Path.relative dir ".merlin" in
    (* We make the compilation of .ml/.mli files depend on the
       existence of .merlin so that they are always generated, however
       the command themselves don't read the merlin file, so we don't
       want to declare a dependency on the contents of the .merlin
       file.

       Currently dune doesn't support declaring a dependency only
       on the existence of a file, so we have to use this trick. *)
    SC.add_rule sctx ~dir
      (Build.path merlin_file
       >>>
       Build.create_file (Path.relative dir ".merlin-exists"));
    Dep.Set.of_files [merlin_file]
    |> Build_system.Alias.add_deps (Alias.check ~dir);
    SC.add_rule sctx ~dir ~mode:(Promote (Until_clean, None)) (
      flags
      >>^ (fun flags ->
        let (src_dirs, obj_dirs) =
          Lib.Set.fold requires ~init:(t.source_dirs, t.objs_dirs)
            ~f:(fun (lib : Lib.t) (src_dirs, obj_dirs) ->
              ( Path.Set.add src_dirs (
                  Lib.orig_src_dir lib
                  |> Path.drop_optional_build_context)
              ,
              Path.Set.add obj_dirs (Lib.public_cmi_dir lib)
            ))
        in
        let src_dirs =
          Path.Set.union src_dirs (Path.Set.of_list more_src_dirs)
        in
        Dot_file.to_string
          ~remaindir
          ~pp:(pp_flags sctx ~expander ~dir_kind t)
          ~flags
          ~src_dirs
          ~obj_dirs)
      >>>
      Build.write_file_dyn merlin_file)

let merge_two ~allow_approx_merlin a b =
  { requires = Lib.Set.union a.requires b.requires
  ; flags = a.flags &&& b.flags >>^ (fun (a, b) -> a @ b)
  ; preprocess = Preprocess.merge ~allow_approx_merlin a.preprocess b.preprocess
  ; libname =
      (match a.libname with
       | Some _ as x -> x
       | None -> b.libname)
  ; source_dirs = Path.Set.union a.source_dirs b.source_dirs
  ; objs_dirs = Path.Set.union a.objs_dirs b.objs_dirs
  }

let merge_all ~allow_approx_merlin = function
  | [] -> None
  | init :: ts ->
    Some (List.fold_left ~init ~f:(merge_two ~allow_approx_merlin) ts)

let add_rules sctx ~dir ~more_src_dirs ~expander ~dir_kind merlin =
  if (SC.context sctx).merlin then
    dot_merlin sctx ~dir ~more_src_dirs ~expander ~dir_kind merlin
