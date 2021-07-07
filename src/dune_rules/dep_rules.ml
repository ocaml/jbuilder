open! Dune_engine
open! Import
open Memo.Build.O
module CC = Compilation_context
module SC = Super_context

let interpret_deps cctx ~unit deps =
  let dir = CC.dir cctx in
  let modules = CC.modules cctx in
  let deps = Ocamldep.parse_module_names ~unit ~modules deps in
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
  let modules = CC.modules cctx in
  let sctx = CC.super_context cctx in
  let source = Option.value_exn (Module.source unit ~ml_kind) in
  let obj_dir = CC.obj_dir cctx in
  let dir = CC.dir cctx in
  let dep = Obj_dir.Module.dep obj_dir in
  let context = SC.context sctx in
  let parse_module_names = Ocamldep.parse_module_names ~modules in
  let all_deps_file = dep (Transitive (unit, ml_kind)) in
  let ocamldep_output = dep (Immediate source) in
  let open Memo.Build.O in
  let* () =
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
         ~stdout_to:ocamldep_output)
  in
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
    let open Action_builder.O in
    let paths =
      let+ lines = Action_builder.lines_of (Path.build ocamldep_output) in
      lines
      |> Ocamldep.parse_deps_exn ~file:(Module.File.path source)
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
  let+ () = SC.add_rule sctx ~dir action in
  let all_deps_file = Path.build all_deps_file in
  Action_builder.memoize
    (Path.to_string all_deps_file)
    (Action_builder.map ~f:(parse_module_names ~unit)
       (Action_builder.lines_of all_deps_file))

let transitive_deps_contents modules =
  List.map modules ~f:(fun m -> Module_name.to_string (Module.name m))
  |> String.concat ~sep:"\n"

let ooi_deps cctx ~vlib_obj_map ~(ml_kind : Ml_kind.t) (m : Module.t) =
  let cm_kind =
    match ml_kind with
    | Intf -> Cm_kind.Cmi
    | Impl -> CC.vimpl cctx |> Option.value_exn |> Vimpl.impl_cm_kind
  in
  let sctx = CC.super_context cctx in
  let dir = CC.dir cctx in
  let obj_dir = CC.obj_dir cctx in
  let write, read =
    let ctx = SC.context sctx in
    let unit =
      Obj_dir.Module.cm_file_exn obj_dir m ~kind:cm_kind |> Path.build
    in
    Ocamlobjinfo.rules ~dir ~ctx ~unit
  in
  let add_rule = SC.add_rule sctx ~dir in
  let read =
    Action_builder.memoize "ocamlobjinfo"
      (let open Action_builder.O in
      let+ (ooi : Ocamlobjinfo.t) = read in
      Module_name.Unique.Set.to_list ooi.intf
      |> List.filter_map ~f:(fun dep ->
             if Module.obj_name m = dep then
               None
             else
               Module_name.Unique.Map.find vlib_obj_map dep))
  in
  let+ () = add_rule write
  and+ () =
    add_rule
      (let target = Obj_dir.Module.dep obj_dir (Transitive (m, ml_kind)) in
       Action_builder.map read ~f:transitive_deps_contents
       |> Action_builder.write_file_dyn target)
  in
  read

let deps_of_module cctx ~ml_kind m =
  match Module.kind m with
  | Wrapped_compat ->
    let modules = CC.modules cctx in
    let interface_module =
      match Modules.lib_interface modules with
      | Some m -> m
      | None -> Modules.compat_for_exn modules m
    in
    Action_builder.return (List.singleton interface_module) |> Memo.Build.return
  | _ -> deps_of ~cctx ~ml_kind m

let deps_of_vlib_module cctx ~ml_kind m =
  let vimpl = Option.value_exn (CC.vimpl cctx) in
  let vlib = Vimpl.vlib vimpl in
  match Lib.Local.of_lib vlib with
  | None ->
    let vlib_obj_map = Vimpl.vlib_obj_map vimpl in
    ooi_deps cctx ~vlib_obj_map ~ml_kind m
  | Some lib ->
    let modules = Vimpl.vlib_modules vimpl in
    let info = Lib.Local.info lib in
    let vlib_obj_dir = Lib_info.obj_dir info in
    let dir = CC.dir cctx in
    let src =
      Obj_dir.Module.dep vlib_obj_dir (Transitive (m, ml_kind)) |> Path.build
    in
    let dst =
      let obj_dir = CC.obj_dir cctx in
      Obj_dir.Module.dep obj_dir (Transitive (m, ml_kind))
    in
    let sctx = CC.super_context cctx in
    let+ () = SC.add_rule sctx ~dir (Action_builder.symlink ~src ~dst) in
    Ocamldep.read_deps_of ~obj_dir:vlib_obj_dir ~modules ~ml_kind m

let rec deps_of cctx ~ml_kind (m : Modules.Sourced_module.t) =
  let is_alias =
    match m with
    | Imported_from_vlib m
    | Normal m ->
      Module.kind m = Alias
    | Impl_of_virtual_module _ -> false
  in
  if is_alias then
    Memo.Build.return (Action_builder.return [])
  else
    let skip_if_source_absent f m =
      if Module.has m ~ml_kind then
        f m
      else
        Memo.Build.return (Action_builder.return [])
    in
    match m with
    | Imported_from_vlib m ->
      skip_if_source_absent (deps_of_vlib_module cctx ~ml_kind) m
    | Normal m -> skip_if_source_absent (deps_of_module cctx ~ml_kind) m
    | Impl_of_virtual_module impl_or_vlib -> (
      let m = Ml_kind.Dict.get impl_or_vlib ml_kind in
      match ml_kind with
      | Intf -> deps_of cctx ~ml_kind (Imported_from_vlib m)
      | Impl -> deps_of cctx ~ml_kind (Normal m))

let dict_of_func_concurrently f =
  let+ impl = f ~ml_kind:Ml_kind.Impl
  and+ intf = f ~ml_kind:Ml_kind.Intf in
  Ml_kind.Dict.make ~impl ~intf

let for_module cctx module_ =
  dict_of_func_concurrently (deps_of cctx (Normal module_))

let rules cctx ~modules =
  let dir = CC.dir cctx in
  dict_of_func_concurrently (fun ~ml_kind ->
      let+ per_module =
        Modules.obj_map_build modules ~f:(deps_of cctx ~ml_kind)
      in
      Dep_graph.make ~dir ~per_module)
