open! Dune_engine
open! Stdune

type t =
  { vlib : Lib.t
  ; impl : Dune_file.Library.t
  ; vlib_modules : Modules.t
  ; vlib_foreign_objects : Path.t list
  ; impl_cm_kind : Cm_kind.t
  ; vlib_obj_map : Module.t Module_name.Unique.Map.t Lazy.t
  }

let vlib_modules t = t.vlib_modules

let vlib t = t.vlib

let impl t = t.impl

let impl_cm_kind t = t.impl_cm_kind

let impl_modules t m =
  match t with
  | None -> m
  | Some t -> Modules.impl ~vlib:t.vlib_modules m

let make ~vlib ~impl ~vlib_modules ~vlib_foreign_objects =
  let impl_cm_kind =
    let vlib_info = Lib.info vlib in
    let { Mode.Dict.byte; native = _ } = Lib_info.modes vlib_info in
    Mode.cm_kind
      (if byte then
        Byte
      else
        Native)
  in
  let vlib_obj_map =
    lazy
      (Modules.obj_map vlib_modules ~f:(function
         | Normal m -> m
         | _ -> assert false)
      |> Module.Obj_map.fold ~init:Module_name.Unique.Map.empty ~f:(fun m acc ->
             Module_name.Unique.Map.add_exn acc (Module.obj_name m) m))
  in
  { impl; impl_cm_kind; vlib; vlib_modules; vlib_foreign_objects; vlib_obj_map }

let vlib_stubs_o_files = function
  | None -> []
  | Some t -> t.vlib_foreign_objects

let vlib_obj_map t = Lazy.force t.vlib_obj_map
