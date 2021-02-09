open! Dune_engine
open! Stdune
open Import

module Paths = struct
  let library_object_directory ~dir name =
    Path.Build.relative dir ("." ^ Lib_name.Local.to_string name ^ ".objs")

  let library_native_dir ~obj_dir = Path.Build.relative obj_dir "native"

  let library_byte_dir ~obj_dir = Path.Build.relative obj_dir "byte"

  let library_public_cmi_dir ~obj_dir = Path.Build.relative obj_dir "public_cmi"

  (* Use "eobjs" rather than "objs" to avoid a potential conflict with a library
     of the same name *)
  let executable_object_directory ~dir name =
    Path.Build.relative dir ("." ^ name ^ ".eobjs")
end

module External = struct
  type t =
    { public_dir : Path.t
    ; private_dir : Path.t option
    ; public_cmi_dir : Path.t option
    }

  let make ~dir ~has_private_modules ~private_lib =
    let private_dir =
      if has_private_modules then
        Some (Path.relative dir ".private")
      else
        None
    in
    let public_cmi_dir =
      if private_lib then
        Some (Path.relative dir ".public_cmi")
      else
        None
    in
    { public_dir = dir; private_dir; public_cmi_dir }

  let public_cmi_dir t = Option.value ~default:t.public_dir t.public_cmi_dir

  let to_dyn { public_dir; private_dir; public_cmi_dir } =
    let open Dyn.Encoder in
    record
      [ ("public_dir", Path.to_dyn public_dir)
      ; ("private_dir", option Path.to_dyn private_dir)
      ; ("public_cmi_dir", option Path.to_dyn public_cmi_dir)
      ]

  let cm_dir t (cm_kind : Cm_kind.t) (visibility : Visibility.t) =
    match (cm_kind, visibility, t.private_dir) with
    | Cmi, Private, Some p -> p
    | Cmi, Private, None ->
      Code_error.raise "External.cm_dir" [ ("t", to_dyn t) ]
    | Cmi, Public, _ -> public_cmi_dir t
    | (Cmo | Cmx), _, _ -> t.public_dir

  let encode { public_dir; private_dir; public_cmi_dir } =
    let open Dune_lang.Encoder in
    let extract d =
      Path.descendant ~of_:public_dir d |> Option.value_exn |> Path.to_string
    in
    let private_dir = Option.map ~f:extract private_dir in
    let public_cmi_dir = Option.map ~f:extract public_cmi_dir in
    record_fields
      [ field_o "private_dir" string private_dir
      ; field_o "public_cmi_dir" string public_cmi_dir
      ]

  let decode ~dir =
    let public_dir = dir in
    let open Dune_lang.Decoder in
    fields
      (let+ private_dir = field_o "private_dir" string
       and+ public_cmi_dir = field_o "public_cmi_dir" string in
       let private_dir = Option.map ~f:(Path.relative dir) private_dir in
       let public_cmi_dir = Option.map ~f:(Path.relative dir) public_cmi_dir in
       { public_dir; private_dir; public_cmi_dir })

  let byte_dir t = t.public_dir

  let native_dir t = t.public_dir

  let dir t = t.public_dir

  let obj_dir t = t.public_dir

  let odoc_dir t = t.public_dir

  let all_obj_dirs t ~mode:_ = [ t.public_dir ]

  let all_cmis { public_dir; private_dir; public_cmi_dir } =
    List.filter_opt [ Some public_dir; private_dir; public_cmi_dir ]

  let cm_public_dir t (cm_kind : Cm_kind.t) =
    match cm_kind with
    | Cmx -> native_dir t
    | Cmo -> byte_dir t
    | Cmi -> public_cmi_dir t
end

module Local = struct
  type t =
    { dir : Path.Build.t
    ; obj_dir : Path.Build.t
    ; native_dir : Path.Build.t
    ; byte_dir : Path.Build.t
    ; public_cmi_dir : Path.Build.t option
    ; private_lib : bool
    }

  let to_dyn { dir; obj_dir; native_dir; byte_dir; public_cmi_dir; private_lib }
      =
    let open Dyn.Encoder in
    record
      [ ("dir", Path.Build.to_dyn dir)
      ; ("obj_dir", Path.Build.to_dyn obj_dir)
      ; ("native_dir", Path.Build.to_dyn native_dir)
      ; ("byte_dir", Path.Build.to_dyn byte_dir)
      ; ("public_cmi_dir", option Path.Build.to_dyn public_cmi_dir)
      ; ("private_lib", bool private_lib)
      ]

  let make ~dir ~obj_dir ~native_dir ~byte_dir ~public_cmi_dir ~private_lib =
    { dir; obj_dir; native_dir; byte_dir; public_cmi_dir; private_lib }

  let need_dedicated_public_dir t = Option.is_some t.public_cmi_dir

  let public_cmi_dir t = Option.value ~default:t.byte_dir t.public_cmi_dir

  let dir t = t.dir

  let obj_dir t = t.obj_dir

  let byte_dir t = t.byte_dir

  let native_dir t = t.native_dir

  let odoc_dir t = t.byte_dir

  let all_obj_dirs t ~(mode : Mode.t) =
    let dirs = [ t.byte_dir; public_cmi_dir t ] in
    let dirs =
      match mode with
      | Byte -> dirs
      | Native -> t.native_dir :: dirs
    in
    Path.Build.Set.of_list dirs |> Path.Build.Set.to_list

  let make_lib ~dir ~has_private_modules ~private_lib lib_name =
    let obj_dir = Paths.library_object_directory ~dir lib_name in
    let public_cmi_dir =
      Option.some_if has_private_modules (Paths.library_public_cmi_dir ~obj_dir)
    in
    make ~dir ~obj_dir
      ~native_dir:(Paths.library_native_dir ~obj_dir)
      ~byte_dir:(Paths.library_byte_dir ~obj_dir)
      ~public_cmi_dir ~private_lib

  let make_exe ~dir ~name =
    let obj_dir = Paths.executable_object_directory ~dir name in
    make ~dir ~obj_dir
      ~native_dir:(Paths.library_native_dir ~obj_dir)
      ~byte_dir:(Paths.library_byte_dir ~obj_dir)
      ~public_cmi_dir:None ~private_lib:false

  let cm_dir t cm_kind _ =
    match cm_kind with
    | Cm_kind.Cmx -> native_dir t
    | Cmo
    | Cmi ->
      byte_dir t

  let cm_public_dir t (cm_kind : Cm_kind.t) =
    match cm_kind with
    | Cmx -> native_dir t
    | Cmo -> byte_dir t
    | Cmi -> public_cmi_dir t
end

type _ t =
  | External : External.t -> Path.t t
  | Local : Local.t -> Path.Build.t t
  | Local_as_path : Local.t -> Path.t t

let of_local : Path.Build.t t -> Path.t t =
 fun t ->
  match t with
  | Local t -> Local_as_path t
  | _ -> assert false

let encode = function
  | Local_as_path obj_dir ->
    Code_error.raise "Obj_dir.encode: local obj_dir cannot be encoded"
      [ ("obj_dir", Local.to_dyn obj_dir) ]
  | Local obj_dir ->
    Code_error.raise "Obj_dir.encode: local obj_dir cannot be encoded"
      [ ("obj_dir", Local.to_dyn obj_dir) ]
  | External e -> External.encode e

let decode ~dir =
  let open Dune_lang.Decoder in
  let+ external_ = External.decode ~dir in
  External external_

let make_lib ~dir ~has_private_modules ~private_lib lib_name =
  Local (Local.make_lib ~dir ~has_private_modules ~private_lib lib_name)

let make_external_no_private ~dir =
  External (External.make ~dir ~has_private_modules:false ~private_lib:false)

let get_path :
    type a. a t -> l:(Local.t -> Path.Build.t) -> e:(External.t -> Path.t) -> a
    =
 fun t ~l ~e ->
  match t with
  | External e' -> e e'
  | Local l' -> l l'
  | Local_as_path l' -> Path.build (l l')

let public_cmi_dir = get_path ~l:Local.public_cmi_dir ~e:External.public_cmi_dir

let byte_dir = get_path ~l:Local.byte_dir ~e:External.byte_dir

let native_dir = get_path ~l:Local.native_dir ~e:External.native_dir

let dir = get_path ~l:Local.dir ~e:External.dir

let obj_dir = get_path ~l:Local.obj_dir ~e:External.obj_dir

let to_dyn (type path) (t : path t) =
  let open Dyn.Encoder in
  match t with
  | Local e -> constr "Local" [ Local.to_dyn e ]
  | Local_as_path e -> constr "Local_as_path" [ Local.to_dyn e ]
  | External e -> constr "External" [ External.to_dyn e ]

let convert_to_external (t : Path.Build.t t) ~dir =
  match t with
  | Local e ->
    let has_private_modules = Local.need_dedicated_public_dir e in
    External
      (External.make ~dir ~has_private_modules ~private_lib:e.private_lib)
  | _ -> assert false

let all_cmis (type path) (t : path t) : path list =
  match t with
  | Local e -> [ Local.byte_dir e ]
  | Local_as_path e -> [ Path.build (Local.byte_dir e) ]
  | External e -> External.all_cmis e

let all_obj_dirs (type path) (t : path t) ~mode : path list =
  match t with
  | External e -> External.all_obj_dirs e ~mode
  | Local e -> Local.all_obj_dirs e ~mode
  | Local_as_path e -> Local.all_obj_dirs e ~mode |> List.map ~f:Path.build

let cm_dir t cm_kind visibility =
  get_path t
    ~l:(fun l -> Local.cm_dir l cm_kind visibility)
    ~e:(fun e -> External.cm_dir e cm_kind visibility)

let cm_public_dir t cm_kind =
  get_path t
    ~l:(fun l -> Local.cm_public_dir l cm_kind)
    ~e:(fun e -> External.cm_public_dir e cm_kind)

let odoc_dir t = get_path t ~l:Local.odoc_dir ~e:External.odoc_dir

let need_dedicated_public_dir (t : Path.Build.t t) =
  match t with
  | Local t -> Local.need_dedicated_public_dir t
  | _ -> assert false

let as_local_exn (t : Path.t t) =
  match t with
  | Local _ -> assert false
  | Local_as_path e -> Local e
  | External _ -> Code_error.raise "Obj_dir.as_local_exn: external dir" []

let make_exe ~dir ~name = Local (Local.make_exe ~dir ~name)

let for_pp ~dir =
  Local
    (Local.make ~dir ~obj_dir:dir ~native_dir:dir ~byte_dir:dir
       ~public_cmi_dir:None ~private_lib:false)

let to_local (t : Path.t t) =
  match t with
  | Local _ -> assert false
  | Local_as_path t -> Some (Local t)
  | External _ -> None

module Module = struct
  let relative (type path) (t : path t) (dir : path) name : path =
    match t with
    | Local _ -> Path.Build.relative dir name
    | Local_as_path _ -> Path.relative dir name
    | External _ -> Path.relative dir name

  let path_of_build (type path) (t : path t) (dir : path) : Path.t =
    match t with
    | Local _ -> Path.build dir
    | Local_as_path _ -> dir
    | External _ -> dir

  let obj_file (type path) (t : path t) m ~kind ~ext : path =
    let visibility = Module.visibility m in
    let obj_name =
      Module_name.Unique.artifact_filename (Module.obj_name m) ~ext
    in
    let dir = cm_dir t kind visibility in
    relative t dir obj_name

  let has_impl_if_needed m ~kind =
    match (kind : Cm_kind.t) with
    | Cmo
    | Cmx ->
      Module.has m ~ml_kind:Impl
    | Cmi -> true

  let raise_no_impl m ~kind =
    Code_error.raise "module has no implementation"
      [ ("m", Module.to_dyn m); ("kind", Cm_kind.to_dyn kind) ]

  let o_file t m ~ext_obj =
    let kind = Cm_kind.Cmx in
    if Module.has m ~ml_kind:Impl then
      Some (obj_file t m ~kind ~ext:ext_obj)
    else
      None

  let o_file_exn t m ~ext_obj =
    match o_file t m ~ext_obj with
    | Some o -> o
    | None -> raise_no_impl m ~kind:Cmx

  let cm_file t m ~(kind : Cm_kind.t) =
    if has_impl_if_needed m ~kind then
      let ext = Cm_kind.ext kind in
      Some (obj_file t m ~kind ~ext)
    else
      None

  let cm_file_exn t m ~kind =
    match cm_file t m ~kind with
    | Some s -> s
    | None -> raise_no_impl m ~kind

  let cm_public_file (type path) (t : path t) m ~(kind : Cm_kind.t) :
      path option =
    let is_private = Module.visibility m = Private in
    let has_impl = Module.has m ~ml_kind:Impl in
    match kind with
    | Cmx
    | Cmo
      when not has_impl ->
      None
    | Cmi when is_private -> None
    | _ ->
      let ext = Cm_kind.ext kind in
      let base = cm_public_dir t kind in
      let obj_name = Module.obj_name m in
      let fname = Module_name.Unique.artifact_filename obj_name ~ext in
      Some (relative t base fname)

  let cm_public_file_exn t m ~kind =
    match cm_public_file t m ~kind with
    | Some x -> x
    | None ->
      Code_error.raise
        "cm_public_file_exn: invalid access. module has no implementation or \
         is private"
        [ ("m", Module.to_dyn m); ("kind", Cm_kind.to_dyn kind) ]

  let cmt_file t m ~(ml_kind : Ml_kind.t) =
    let file = Module.file m ~ml_kind in
    let ext = Ml_kind.cmt_ext ml_kind in
    Option.map file ~f:(fun _ -> obj_file t m ~kind:Cmi ~ext)

  let cmti_file t m =
    let ext =
      Ml_kind.cmt_ext
        (match Module.file m ~ml_kind:Intf with
        | None -> Impl
        | Some _ -> Intf)
    in
    obj_file t m ~kind:Cmi ~ext

  let odoc t m =
    let obj_name = Module.obj_name m in
    let basename = Module_name.Unique.artifact_filename obj_name ~ext:".odoc" in
    relative t (odoc_dir t) basename

  module Dep = struct
    type t =
      | Immediate of Module.File.t
      | Transitive of Module.t * Ml_kind.t

    let basename = function
      | Immediate f -> Path.basename (Module.File.path f) ^ ".d"
      | Transitive (m, ml_kind) ->
        let ext = sprintf ".%s.all-deps" (Ml_kind.to_string ml_kind) in
        let obj = Module.obj_name m in
        Module_name.Unique.artifact_filename obj ~ext
  end

  let dep t dep =
    let dir = obj_dir t in
    let name = Dep.basename dep in
    Path.Build.relative dir name

  module L = struct
    let o_files t modules ~ext_obj =
      List.filter_map modules ~f:(fun m ->
          if Module.has m ~ml_kind:Impl then
            Some (path_of_build t (obj_file t m ~kind:Cmx ~ext:ext_obj))
          else
            None)

    let cm_files t modules ~kind =
      List.filter_map modules ~f:(fun m ->
          cm_file t m ~kind |> Option.map ~f:(path_of_build t))
  end
end
