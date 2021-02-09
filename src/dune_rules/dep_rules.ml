open! Dune_engine
open! Import
open Action_builder.O

let transitive_deps_contents modules =
  List.map modules ~f:(fun m -> Module_name.to_string (Module.name m))
  |> String.concat ~sep:"\n"

let ooi_deps cctx ~vlib_obj_map ~(ml_kind : Ml_kind.t) (m : Module.t) =
  let cm_kind =
    match ml_kind with
    | Intf -> Cm_kind.Cmi
    | Impl ->
      Compilation_context.vimpl cctx |> Option.value_exn |> Vimpl.impl_cm_kind
  in
  let sctx = Compilation_context.super_context cctx in
  let dir = Compilation_context.dir cctx in
  let obj_dir = Compilation_context.obj_dir cctx in
  let write, read =
    let ctx = Super_context.context sctx in
    let unit =
      Obj_dir.Module.cm_file_exn obj_dir m ~kind:cm_kind |> Path.build
    in
    Ocamlobjinfo.rules ~dir ~ctx ~unit
  in
  let add_rule = Super_context.add_rule sctx ~dir in
  add_rule write;
  let read =
    Action_builder.memoize "ocamlobjinfo"
      (let+ (ooi : Ocamlobjinfo.t) = read in
       Module_name.Unique.Set.to_list ooi.intf
       |> List.filter_map ~f:(fun dep ->
              if Module.obj_name m = dep then
                None
              else
                Module_name.Unique.Map.find vlib_obj_map dep))
  in
  add_rule
    (let target = Obj_dir.Module.dep obj_dir (Transitive (m, ml_kind)) in
     Action_builder.map read ~f:transitive_deps_contents
     |> Action_builder.write_file_dyn target);
  read

let deps_of_module cctx ~ml_kind m =
  match Module.kind m with
  | Wrapped_compat ->
    let modules = Compilation_context.modules cctx in
    let interface_module =
      match Modules.lib_interface modules with
      | Some m -> m
      | None -> Modules.compat_for_exn modules m
    in
    Action_builder.return (List.singleton interface_module)
  | _ -> Ocamldep.deps_of ~cctx ~ml_kind m

let deps_of_vlib_module cctx ~ml_kind m =
  let vimpl = Option.value_exn (Compilation_context.vimpl cctx) in
  let vlib = Vimpl.vlib vimpl in
  match Lib.Local.of_lib vlib with
  | None ->
    let vlib_obj_map = Vimpl.vlib_obj_map vimpl in
    ooi_deps cctx ~vlib_obj_map ~ml_kind m
  | Some lib ->
    let modules = Vimpl.vlib_modules vimpl in
    let info = Lib.Local.info lib in
    let vlib_obj_dir = Lib_info.obj_dir info in
    let dir = Compilation_context.dir cctx in
    let src =
      Obj_dir.Module.dep vlib_obj_dir (Transitive (m, ml_kind)) |> Path.build
    in
    let dst =
      let obj_dir = Compilation_context.obj_dir cctx in
      Obj_dir.Module.dep obj_dir (Transitive (m, ml_kind))
    in
    let sctx = Compilation_context.super_context cctx in
    Super_context.add_rule sctx ~dir (Action_builder.symlink ~src ~dst);
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
    Action_builder.return []
  else
    let skip_if_source_absent f m =
      if Module.has m ~ml_kind then
        f m
      else
        Action_builder.return []
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

let for_module cctx module_ =
  Ml_kind.Dict.of_func (fun ~ml_kind -> deps_of cctx ~ml_kind (Normal module_))

let rules cctx ~modules =
  match Modules.as_singleton modules with
  | Some m -> Dep_graph.Ml_kind.dummy m
  | None ->
    let dir = Compilation_context.dir cctx in
    Ml_kind.Dict.of_func (fun ~ml_kind ->
        let per_module = Modules.obj_map modules ~f:(deps_of cctx ~ml_kind) in
        Dep_graph.make ~dir ~per_module)
