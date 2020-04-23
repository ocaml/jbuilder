open Import
open! No_io

module Pp_spec : sig
  type t

  val make :
    Dune_file.Preprocess.t Module_name.Per_item.t -> Ocaml_version.t -> t

  val pped_module : t -> Module.t -> Module.t
end = struct
  type t = (Module.t -> Module.t) Module_name.Per_item.t

  let make preprocess v =
    Module_name.Per_item.map preprocess ~f:(fun pp ->
        match Dune_file.Preprocess.remove_future_syntax ~for_:Compiler pp v with
        | No_preprocessing -> Module.ml_source
        | Action (_, _) -> fun m -> Module.ml_source (Module.pped m)
        | Pps { loc = _; pps = _; flags = _; staged } ->
          if staged then
            Module.ml_source
          else
            fun m ->
          Module.pped (Module.ml_source m))

  let pped_module (t : t) m = Module_name.Per_item.get t (Module.name m) m
end

let setup_copy_rules_for_impl ~sctx ~dir vimpl =
  let ctx = Super_context.context sctx in
  let vlib = Vimpl.vlib vimpl in
  let impl = Vimpl.impl vimpl in
  let impl_obj_dir = Dune_file.Library.obj_dir ~dir impl in
  let vlib_obj_dir = Lib.obj_dir vlib in
  let add_rule = Super_context.add_rule sctx ~dir in
  let copy_to_obj_dir ~src ~dst =
    add_rule ~loc:(Loc.of_pos __POS__) (Build.symlink ~src ~dst)
  in
  let { Lib_config.has_native; ext_obj; _ } = ctx.lib_config in
  let { Mode.Dict.byte; native } =
    Dune_file.Mode_conf.Set.eval impl.modes ~has_native
  in
  let copy_obj_file m kind =
    let src = Obj_dir.Module.cm_file_unsafe vlib_obj_dir m ~kind in
    let dst = Obj_dir.Module.cm_file_unsafe impl_obj_dir m ~kind in
    copy_to_obj_dir ~src ~dst
  in
  let copy_objs src =
    copy_obj_file src Cmi;
    ( if
      Module.visibility src = Public
      && Obj_dir.need_dedicated_public_dir impl_obj_dir
    then
      let dst =
        Obj_dir.Module.cm_public_file_unsafe impl_obj_dir src ~kind:Cmi
      in
      let src =
        Obj_dir.Module.cm_public_file_unsafe vlib_obj_dir src ~kind:Cmi
      in
      copy_to_obj_dir ~src ~dst );
    if Module.has src ~ml_kind:Impl then (
      if byte then copy_obj_file src Cmo;
      if native then (
        copy_obj_file src Cmx;
        let object_file dir =
          Obj_dir.Module.obj_file dir src ~kind:Cmx ~ext:ext_obj
        in
        copy_to_obj_dir ~src:(object_file vlib_obj_dir)
          ~dst:(object_file impl_obj_dir)
      )
    )
  in
  let vlib_modules = Vimpl.vlib_modules vimpl in
  Modules.iter_no_vlib vlib_modules ~f:(fun m -> copy_objs m)

let impl sctx ~(lib : Dune_file.Library.t) ~scope =
  Option.map lib.implements ~f:(fun (loc, implements) ->
      match Lib.DB.find (Scope.libs scope) implements with
      | None ->
        User_error.raise ~loc
          [ Pp.textf "Cannot implement %s as that library isn't available"
              (Lib_name.to_string implements)
          ]
      | Some vlib ->
        let info = Lib.info vlib in
        let virtual_ =
          let virtual_ = Lib_info.virtual_ info in
          match virtual_ with
          | None ->
            User_error.raise ~loc:lib.buildable.loc
              [ Pp.textf "Library %s isn't virtual and cannot be implemented"
                  (Lib_name.to_string implements)
              ]
          | Some v -> v
        in
        let vlib_modules, vlib_foreign_objects =
          let foreign_objects = Lib_info.foreign_objects info in
          match (virtual_, foreign_objects) with
          | External _, Local
          | Local, External _ ->
            assert false
          | External modules, External fa -> (modules, fa)
          | Local, Local ->
            let name = Lib.name vlib in
            let vlib = Lib.Local.of_lib_exn vlib in
            let dir_contents =
              let info = Lib.Local.info vlib in
              let dir = Lib_info.src_dir info in
              Dir_contents.get sctx ~dir
            in
            let modules =
              let pp_spec =
                Pp_spec.make lib.buildable.preprocess
                  (Super_context.context sctx).version
              in
              Dir_contents.ocaml dir_contents
              |> Ml_sources.modules_of_library ~name
              |> Result.ok_exn
              |> Modules.map_user_written ~f:(Pp_spec.pped_module pp_spec)
            in
            let foreign_objects =
              let ext_obj = (Super_context.context sctx).lib_config.ext_obj in
              let dir = Obj_dir.obj_dir (Lib.Local.obj_dir vlib) in
              Dir_contents.foreign_sources dir_contents
              |> Foreign_sources.for_lib ~name
              |> Foreign.Sources.object_files ~ext_obj ~dir
              |> List.map ~f:Path.build
            in
            (modules, foreign_objects)
        in
        Vimpl.make ~impl:lib ~vlib ~vlib_modules ~vlib_foreign_objects)
