open! Dune_engine
open! Stdune
open Import
open Action_builder.O
module CC = Compilation_context
module SC = Super_context

let parse_module_names ~(unit : Module.t) ~modules words =
  List.filter_map words ~f:(fun m ->
      let m = Module_name.of_string m in
      Modules.find_dep modules ~of_:unit m)

let parse_deps_exn ~file lines =
  let invalid () =
    User_error.raise
      [ Pp.textf "ocamldep returned unexpected output for %s:"
          (Path.to_string_maybe_quoted file)
      ; Pp.vbox
          (Pp.concat_map lines ~sep:Pp.cut ~f:(fun line ->
               Pp.seq (Pp.verbatim "> ") (Pp.verbatim line)))
      ]
  in
  match lines with
  | []
  | _ :: _ :: _ ->
    invalid ()
  | [ line ] -> (
    match String.lsplit2 line ~on:':' with
    | None -> invalid ()
    | Some (basename, deps) ->
      let basename = Filename.basename basename in
      if basename <> Path.basename file then invalid ();
      String.extract_blank_separated_words deps)

let interpret_deps cctx ~unit deps =
  let dir = CC.dir cctx in
  let modules = CC.modules cctx in
  let deps = parse_module_names ~unit ~modules deps in
  let stdlib = CC.stdlib cctx in
  if Option.is_none stdlib then
    Modules.main_module_name modules
    |> Option.iter ~f:(fun (main_module_name : Module_name.t) ->
           if
             Module_name.Infix.(Module.name unit <> main_module_name)
             && (not (Module.kind unit = Alias))
             && List.exists deps ~f:(fun x -> Module.name x = main_module_name)
           then
             User_error.raise
               [ Pp.textf "Module %s in directory %s depends on %s."
                   (Module_name.to_string (Module.name unit))
                   (Path.to_string_maybe_quoted (Path.build dir))
                   (Module_name.to_string main_module_name)
               ; Pp.textf "This doesn't make sense to me."
               ; Pp.nop
               ; Pp.textf
                   "%s is the main module of the library and is the only \
                    module exposed outside of the library. Consequently, it \
                    should be the one depending on all the other modules in \
                    the library."
                   (Module_name.to_string main_module_name)
               ]);
  match Modules.alias_for modules unit with
  | None -> deps
  | Some m -> m :: deps

let deps_of ~cctx ~ml_kind unit =
  let modules = Compilation_context.modules cctx in
  let sctx = CC.super_context cctx in
  let source = Option.value_exn (Module.source unit ~ml_kind) in
  let obj_dir = Compilation_context.obj_dir cctx in
  let dir = Compilation_context.dir cctx in
  let dep = Obj_dir.Module.dep obj_dir in
  let context = SC.context sctx in
  let parse_module_names = parse_module_names ~modules in
  let all_deps_file = dep (Transitive (unit, ml_kind)) in
  let ocamldep_output = dep (Immediate source) in
  SC.add_rule sctx ~dir
    (let flags =
       Option.value (Module.pp_flags unit) ~default:(Action_builder.return [])
     in
     Command.run context.ocamldep
       ~dir:(Path.build context.build_dir)
       [ A "-modules"
       ; Command.Args.dyn flags
       ; Command.Ml_kind.flag ml_kind
       ; Dep (Module.File.path source)
       ]
       ~stdout_to:ocamldep_output);
  let build_paths dependencies =
    let dependency_file_path m =
      let ml_kind m =
        if Module.kind m = Alias then
          None
        else if Module.has m ~ml_kind:Intf then
          Some Ml_kind.Intf
        else
          Some Impl
      in
      ml_kind m
      |> Option.map ~f:(fun ml_kind ->
             Path.build (dep (Transitive (m, ml_kind))))
    in
    List.filter_map dependencies ~f:dependency_file_path
  in
  let action =
    let paths =
      let+ lines = Action_builder.lines_of (Path.build ocamldep_output) in
      lines
      |> parse_deps_exn ~file:(Module.File.path source)
      |> interpret_deps cctx ~unit
      |> fun modules ->
      ( build_paths modules
      , List.map modules ~f:(fun m -> Module_name.to_string (Module.name m)) )
    in
    Action_builder.with_targets ~targets:[ all_deps_file ]
      (let+ sources, extras =
         Action_builder.dyn_paths
           (let+ sources, extras = paths in
            ((sources, extras), sources))
       in
       Action.Merge_files_into (sources, extras, all_deps_file))
  in
  SC.add_rule sctx ~dir action;
  let all_deps_file = Path.build all_deps_file in
  Action_builder.memoize
    (Path.to_string all_deps_file)
    (Action_builder.map ~f:(parse_module_names ~unit)
       (Action_builder.lines_of all_deps_file))

let read_deps_of ~obj_dir ~modules ~ml_kind unit =
  let all_deps_file = Obj_dir.Module.dep obj_dir (Transitive (unit, ml_kind)) in
  Action_builder.memoize
    (Path.Build.to_string all_deps_file)
    (Action_builder.map
       ~f:(parse_module_names ~unit ~modules)
       (Action_builder.lines_of (Path.build all_deps_file)))
