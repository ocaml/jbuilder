open! Dune_engine
open! Stdune
open Import
open Build.O
open! No_io
module SC = Super_context

module Extensions = Comparable.Make (struct
  type t = string * string

  let compare = Tuple.T2.compare String.compare String.compare

  let to_dyn = Tuple.T2.to_dyn String.to_dyn String.to_dyn
end)

let warn_dropped_pp loc ~allow_approx_merlin ~reason =
  if not allow_approx_merlin then
    User_warning.emit ~loc
      [ Pp.textf ".merlin generated is inaccurate. %s." reason
      ; Pp.text
          "Split the stanzas into different directories or silence this \
           warning by adding (allow_approximate_merlin) to your dune-project."
      ]

module Pp = struct
  let merge ~allow_approx_merlin (a : _ Preprocess.t) (b : _ Preprocess.t) =
    match (a, b) with
    | No_preprocessing, No_preprocessing -> Preprocess.No_preprocessing
    | No_preprocessing, pp
    | pp, No_preprocessing ->
      let loc =
        Preprocess.loc pp |> Option.value_exn
        (* only No_preprocessing has no loc*)
      in
      warn_dropped_pp loc ~allow_approx_merlin
        ~reason:"Cannot mix preprocessed and non preprocessed specifications";
      Preprocess.No_preprocessing
    | (Future_syntax _ as future_syntax), _
    | _, (Future_syntax _ as future_syntax) ->
      future_syntax
    | Action (loc, a1), Action (_, a2) ->
      if Action_dune_lang.compare_no_locs a1 a2 <> Ordering.Eq then
        warn_dropped_pp loc ~allow_approx_merlin
          ~reason:
            "this action preprocessor is not equivalent to other preprocessor \
             specifications.";
      Action (loc, a1)
    | Pps _, Action (loc, _)
    | Action (loc, _), Pps _ ->
      warn_dropped_pp loc ~allow_approx_merlin
        ~reason:"cannot mix action and pps preprocessors";
      No_preprocessing
    | (Pps pp1 as pp), Pps pp2 ->
      if
        Ordering.neq
          (Preprocess.Pps.compare_no_locs
             Preprocess.Without_instrumentation.compare_no_locs pp1 pp2)
      then (
        warn_dropped_pp pp1.loc ~allow_approx_merlin
          ~reason:"pps specification isn't identical in all stanzas";
        No_preprocessing
      ) else
        pp
end

let quote_for_merlin s =
  let s =
    if Sys.win32 then
      (* We need this hack because merlin unescapes backslashes (except when
         protected by single quotes). It is only a problem on windows because
         Filename.quote is using double quotes. *)
      String.escape_only '\\' s
    else
      s
  in
  if String.need_quoting s then
    Filename.quote s
  else
    s

module Dot_file = struct
  let b = Buffer.create 256

  let printf f = Printf.bprintf b f

  let print = Buffer.add_string b

  let to_string ~obj_dirs ~src_dirs ~flags ~pp ~remaindir ~extensions =
    let serialize_path = Path.reach ~from:(Path.source remaindir) in
    Buffer.clear b;
    print "EXCLUDE_QUERY_DIR\n";
    Path.Set.iter obj_dirs ~f:(fun p -> printf "B %s\n" (serialize_path p));
    Path.Set.iter src_dirs ~f:(fun p -> printf "S %s\n" (serialize_path p));
    Option.iter pp ~f:(printf "%s\n");
    ( match flags with
    | [] -> ()
    | flags ->
      print "FLG";
      List.iter flags ~f:(fun f -> printf " %s" (quote_for_merlin f));
      print "\n" );
    Extensions.Set.iter extensions ~f:(fun (impl, intf) ->
        printf "SUFFIX %s %s\n" (quote_for_merlin impl) (quote_for_merlin intf));
    Buffer.contents b
end

type t =
  { requires : Lib.Set.t
  ; flags : string list Build.t
  ; preprocess : Preprocess.Without_instrumentation.t Preprocess.t
  ; libname : Lib_name.Local.t option
  ; source_dirs : Path.Source.Set.t
  ; objs_dirs : Path.Set.t
  ; extensions : Extensions.Set.t
  }

let make ?(requires = Ok []) ~flags ?(preprocess = Preprocess.No_preprocessing)
    ?libname ?(source_dirs = Path.Source.Set.empty) ~modules ~obj_dir ~dialects
    () =
  (* Merlin shouldn't cause the build to fail, so we just ignore errors *)
  let requires =
    match requires with
    | Ok l -> Lib.Set.of_list l
    | Error _ -> Lib.Set.empty
  in
  let objs_dirs =
    Path.Set.of_list
    @@ List.map ~f:Path.build
         [ Obj_dir.byte_dir obj_dir; Obj_dir.native_dir obj_dir ]
  in
  let flags =
    match Modules.alias_module modules with
    | None -> Ocaml_flags.common flags
    | Some m ->
      Ocaml_flags.prepend_common
        [ "-open"; Module_name.to_string (Module.name m) ]
        flags
      |> Ocaml_flags.common
  in
  let extensions =
    Dialect.DB.fold dialects ~init:Extensions.Set.empty ~f:(fun d s ->
        let impl = Dialect.extension d Ml_kind.Impl in
        let intf = Dialect.extension d Ml_kind.Intf in
        if
          (* Only include dialects with no preprocessing and skip default file
             extensions *)
          Dialect.preprocess d Ml_kind.Impl <> None
          || Dialect.preprocess d Ml_kind.Intf <> None
          || impl = Dialect.extension Dialect.ocaml Ml_kind.Impl
             && intf = Dialect.extension Dialect.ocaml Ml_kind.Intf
        then
          s
        else
          Extensions.Set.add s (impl, intf))
  in
  { requires
  ; flags = Build.catch flags ~on_error:(fun _ -> [])
  ; preprocess
  ; libname
  ; source_dirs
  ; objs_dirs
  ; extensions
  }

let merlin_file_name = ".merlin"

let add_source_dir t dir =
  { t with source_dirs = Path.Source.Set.add t.source_dirs dir }

let pp_flag_of_action ~expander ~loc ~action :
    string option Build.With_targets.t =
  match (action : Action_dune_lang.t) with
  | Run (exe, args) -> (
    let args =
      let open Option.O in
      let* args, input_file = List.destruct_last args in
      if String_with_vars.is_var input_file ~name:"input-file" then
        Some args
      else
        None
    in
    match args with
    | None -> Build.With_targets.return None
    | Some args ->
      let action =
        let targets_dir = Expander.dir expander in
        let targets : Targets.Or_forbidden.t =
          Forbidden "preprocessing actions"
        in
        let action = Preprocessing.chdir (Run (exe, args)) in
        Action_unexpanded.expand ~loc ~expander ~dep_kind:Optional ~targets
          ~targets_dir action
          (Build.return Bindings.empty)
      in
      let pp_of_action exe args =
        match exe with
        | Error _ -> None
        | Ok exe ->
          Path.to_absolute_filename exe :: args
          |> List.map ~f:quote_for_merlin
          |> String.concat ~sep:" " |> Filename.quote |> sprintf "FLG -pp %s"
          |> Option.some
      in
      Build.With_targets.map action ~f:(function
        | Run (exe, args) -> pp_of_action exe args
        | Chdir (_, Run (exe, args)) -> pp_of_action exe args
        | Chdir (_, Chdir (_, Run (exe, args))) -> pp_of_action exe args
        | _ -> None) )
  | _ -> Build.With_targets.return None

let pp_flags sctx ~expander { preprocess; libname; _ } :
    string option Build.With_targets.t =
  let scope = Expander.scope expander in
  match
    Preprocess.remove_future_syntax preprocess ~for_:Merlin
      (Super_context.context sctx).version
  with
  | Pps { loc; pps; flags; staged = _ } -> (
    match
      Preprocessing.get_ppx_driver sctx ~loc ~expander ~lib_name:libname ~flags
        ~scope pps
    with
    | Error _exn -> Build.With_targets.return None
    | Ok (exe, flags) ->
      Path.to_absolute_filename (Path.build exe) :: "--as-ppx" :: flags
      |> List.map ~f:quote_for_merlin
      |> String.concat ~sep:" " |> Filename.quote |> sprintf "FLG -ppx %s"
      |> Option.some |> Build.With_targets.return )
  | Action (loc, (action : Action_dune_lang.t)) ->
    pp_flag_of_action ~expander ~loc ~action
  | No_preprocessing -> Build.With_targets.return None

(* This is used to determine the list of source directories to give to Merlin.
   This is similar to [Gen_rules.lib_src_dirs], but it's used for dependencies
   instead of the library itself. It would be nice to unify these some day. *)
let lib_src_dirs ~sctx lib =
  match Lib.Local.of_lib lib with
  | None ->
    let info = Lib.info lib in
    Path.Set.singleton (Lib_info.best_src_dir info)
  | Some info ->
    let info = Lib.Local.info info in
    let dir = Lib_info.src_dir info in
    let name = Lib_info.name info in
    let modules =
      Dir_contents.get sctx ~dir |> Dir_contents.ocaml
      |> Ml_sources.modules_of_library ~name
    in
    Path.Set.map ~f:Path.drop_optional_build_context
      (Modules.source_dirs modules)

let dot_merlin sctx ~dir ~more_src_dirs ~expander
    ({ requires; flags; extensions; _ } as t) =
  Path.Build.drop_build_context dir
  |> Option.iter ~f:(fun remaindir ->
         let open Build.With_targets.O in
         let merlin_file = Path.Build.relative dir merlin_file_name in

         (* We make the compilation of .ml/.mli files depend on the existence of
            .merlin so that they are always generated, however the command
            themselves don't read the merlin file, so we don't want to declare a
            dependency on the contents of the .merlin file.

            Currently dune doesn't support declaring a dependency only on the
            existence of a file, so we have to use this trick. *)
         SC.add_rule sctx ~dir
           ( Build.with_no_targets (Build.path (Path.build merlin_file))
           >>> Build.create_file (Path.Build.relative dir ".merlin-exists") );
         Path.Set.singleton (Path.build merlin_file)
         |> Rules.Produce.Alias.add_deps (Alias.check ~dir);
         let pp_flags = pp_flags sctx ~expander t in
         let action =
           Build.With_targets.write_file_dyn merlin_file
             (let+ flags = Build.with_no_targets flags
              and+ pp = pp_flags in
              let src_dirs, obj_dirs =
                Lib.Set.fold requires
                  ~init:(Path.set_of_source_paths t.source_dirs, t.objs_dirs)
                  ~f:(fun (lib : Lib.t) (src_dirs, obj_dirs) ->
                    let more_src_dirs = lib_src_dirs ~sctx lib in
                    ( Path.Set.union src_dirs more_src_dirs
                    , let lib_obj_dirs =
                        Obj_dir.all_obj_dirs ~mode:Native (Lib.obj_dir lib)
                        |> Path.Set.of_list
                      in
                      Path.Set.union obj_dirs lib_obj_dirs ))
              in
              let src_dirs =
                Path.Set.union src_dirs
                  (Path.Set.of_list_map ~f:Path.source more_src_dirs)
              in
              Dot_file.to_string ~remaindir ~pp ~flags ~src_dirs ~obj_dirs
                ~extensions)
         in
         SC.add_rule sctx ~dir
           ~mode:(Promote { lifetime = Until_clean; into = None; only = None })
           action)

let merge_two ~allow_approx_merlin a b =
  { requires = Lib.Set.union a.requires b.requires
  ; flags =
      (let+ a = a.flags
       and+ b = b.flags in
       a @ b)
  ; preprocess = Pp.merge ~allow_approx_merlin a.preprocess b.preprocess
  ; libname =
      ( match a.libname with
      | Some _ as x -> x
      | None -> b.libname )
  ; source_dirs = Path.Source.Set.union a.source_dirs b.source_dirs
  ; objs_dirs = Path.Set.union a.objs_dirs b.objs_dirs
  ; extensions = Extensions.Set.union a.extensions b.extensions
  }

let merge_all ~allow_approx_merlin = function
  | [] -> None
  | init :: ts ->
    Some (List.fold_left ~init ~f:(merge_two ~allow_approx_merlin) ts)

let add_rules sctx ~dir ~more_src_dirs ~expander merlin =
  if (SC.context sctx).merlin then
    dot_merlin sctx ~dir ~more_src_dirs ~expander merlin
