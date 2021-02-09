open! Dune_engine
open Import

module Inherited = struct
  type 'a t =
    | This of 'a
    | From of (Loc.t * Lib_name.t)

  let to_dyn f x =
    let open Dyn.Encoder in
    match x with
    | This x -> constr "This" [ f x ]
    | From (_, name) -> constr "From" [ Lib_name.to_dyn name ]
end

module Main_module_name = struct
  type t = Module_name.t option Inherited.t

  let to_dyn x = Inherited.to_dyn (Dyn.Encoder.option Module_name.to_dyn) x
end

type _ path =
  | Local : Path.Build.t path
  | External : Path.t path

module Special_builtin_support = struct
  let api_version_field supported_api_versions =
    let open Dune_lang.Decoder in
    field "api_version"
      (let+ loc = loc
       and+ ver = int in
       match List.assoc supported_api_versions ver with
       | Some x -> x
       | None ->
         User_error.raise ~loc
           [ Pp.textf
               "API version %d is not supported. Only the following versions \
                are currently supported:"
               ver
           ; Pp.enumerate supported_api_versions ~f:(fun (n, _) ->
                 Pp.textf "%d" n)
           ])

  module Build_info = struct
    type api_version = V1

    let api_version_to_dyn = function
      | V1 -> Dyn.Encoder.constr "V1" []

    let supported_api_versions = [ (1, V1) ]

    type t =
      { data_module : Module_name.t
      ; api_version : api_version
      }

    let to_dyn { data_module; api_version } =
      let open Dyn.Encoder in
      record
        [ ("data_module", Module_name.to_dyn data_module)
        ; ("api_version", api_version_to_dyn api_version)
        ]

    let decode =
      let open Dune_lang.Decoder in
      fields
        (let+ data_module = field "data_module" Module_name.decode
         and+ api_version = api_version_field supported_api_versions in
         { data_module; api_version })

    let encode { data_module; api_version } =
      let open Dune_lang.Encoder in
      record_fields
        [ field "data_module" Module_name.encode data_module
        ; field "api_version" int
            (match api_version with
            | V1 -> 1)
        ]
  end

  module Configurator = struct
    type api_version = V1

    let api_version_to_dyn = function
      | V1 -> Dyn.Encoder.constr "V1" []

    let supported_api_versions = [ (1, V1) ]

    type t = { api_version : api_version }

    let to_dyn { api_version } =
      let open Dyn.Encoder in
      record [ ("api_version", api_version_to_dyn api_version) ]

    let decode =
      let open Dune_lang.Decoder in
      fields
        (let+ api_version = api_version_field supported_api_versions in
         { api_version })

    let encode { api_version } =
      let open Dune_lang.Encoder in
      record_fields
        [ field "api_version" int
            (match api_version with
            | V1 -> 1)
        ]
  end

  module Dune_site = struct
    type t =
      { data_module : Module_name.t
      ; plugins : bool
      }

    let to_dyn { data_module; plugins } =
      let open Dyn.Encoder in
      record
        [ ("data_module", Module_name.to_dyn data_module)
        ; ("plugins", bool plugins)
        ]

    let decode =
      let open Dune_lang.Decoder in
      fields
        (let+ data_module = field "data_module" Module_name.decode
         and+ plugins = field_b "plugins" in
         { data_module; plugins })

    let encode { data_module; plugins } =
      let open Dune_lang.Encoder in
      record_fields
        [ field "data_module" Module_name.encode data_module
        ; field_b "plugins" plugins
        ]
  end

  type t =
    | Findlib_dynload
    | Build_info of Build_info.t
    | Configurator of Configurator.t
    | Dune_site of Dune_site.t

  let to_dyn x =
    let open Dyn.Encoder in
    match x with
    | Findlib_dynload -> constr "Findlib_dynload" []
    | Build_info info -> constr "Build_info" [ Build_info.to_dyn info ]
    | Configurator info -> constr "Configurator" [ Configurator.to_dyn info ]
    | Dune_site info -> constr "Dune_site" [ Dune_site.to_dyn info ]

  let decode =
    let open Dune_lang.Decoder in
    sum
      [ ("findlib_dynload", return Findlib_dynload)
      ; ( "build_info"
        , let+ () = Dune_lang.Syntax.since Stanza.syntax (1, 11)
          and+ info = Build_info.decode in
          Build_info info )
      ; ( "configurator"
        , let+ () = Dune_lang.Syntax.since Stanza.syntax (2, 3)
          and+ info = Configurator.decode in
          Configurator info )
      ; ( "dune_site"
        , let+ () = Dune_lang.Syntax.since Stanza.syntax (2, 8)
          and+ info = Dune_site.decode in
          Dune_site info )
      ]

  let encode t =
    match t with
    | Findlib_dynload -> Dune_lang.atom "findlib_dynload"
    | Build_info x ->
      Dune_lang.List (Dune_lang.atom "build_info" :: Build_info.encode x)
    | Configurator x ->
      Dune_lang.List (Dune_lang.atom "configurator" :: Configurator.encode x)
    | Dune_site x ->
      Dune_lang.List (Dune_lang.atom "dune_site" :: Dune_site.encode x)
end

module Status = struct
  type t =
    | Installed_private
    | Installed
    | Public of Dune_project.t * Package.t
    | Private of Dune_project.t * Package.t option

  let to_dyn x =
    let open Dyn.Encoder in
    match x with
    | Installed_private -> constr "Installed_private" []
    | Installed -> constr "Installed" []
    | Public (project, package) ->
      constr "Public" [ Dune_project.to_dyn project; Package.to_dyn package ]
    | Private (proj, package) ->
      constr "Private"
        [ Dune_project.to_dyn proj; option Package.to_dyn package ]

  let is_private = function
    | Installed_private
    | Private _ ->
      true
    | Installed
    | Public _ ->
      false

  let project = function
    | Installed_private
    | Installed ->
      None
    | Private (project, _)
    | Public (project, _) ->
      Some project
end

module Source = struct
  type 'a t =
    | Local
    | External of 'a

  let to_dyn f x =
    let open Dyn.Encoder in
    match x with
    | Local -> constr "Local" []
    | External x -> constr "External" [ f x ]

  let map t ~f =
    match t with
    | Local -> Local
    | External a -> External (f a)
end

module Enabled_status = struct
  type t =
    | Normal
    | Optional
    | Disabled_because_of_enabled_if

  let to_dyn x =
    let open Dyn.Encoder in
    match x with
    | Normal -> constr "Normal" []
    | Optional -> constr "Optional" []
    | Disabled_because_of_enabled_if ->
      constr "Disabled_because_of_enabled_if" []
end

type 'path native_archives =
  | Needs_module_info of 'path
  | Files of 'path list

let dyn_of_native_archives path =
  let open Dyn.Encoder in
  function
  | Needs_module_info f -> constr "Needs_module_info" [ path f ]
  | Files files -> constr "Files" [ (list path) files ]

(** {1 Lib_info_invariants}

    Many of the fields here are optional and are "entangled" in the sense that
    they are all either set to [None] (for external libraries) or to [Some] (for
    local libraries). This record used to be a sum type, to distinguish between
    these two cases more cleanly, but was later refactored into a single record
    for the sake of convenience. We might revisit this decision in future. *)
type 'path t =
  { loc : Loc.t
  ; name : Lib_name.t
  ; kind : Lib_kind.t
  ; status : Status.t
  ; src_dir : 'path
  ; orig_src_dir : 'path option
  ; obj_dir : 'path Obj_dir.t
  ; version : string option
  ; synopsis : string option
  ; archives : 'path list Mode.Dict.t
  ; plugins : 'path list Mode.Dict.t
  ; foreign_objects : 'path list Source.t
  ; foreign_archives : 'path list
  ; native_archives : 'path native_archives
  ; foreign_dll_files : 'path list
  ; jsoo_runtime : 'path list
  ; jsoo_archive : 'path option
  ; requires : Lib_dep.t list
  ; ppx_runtime_deps : (Loc.t * Lib_name.t) list
  ; preprocess : Preprocess.With_instrumentation.t Preprocess.Per_module.t
  ; enabled : Enabled_status.t
  ; virtual_deps : (Loc.t * Lib_name.t) list
  ; dune_version : Dune_lang.Syntax.Version.t option
  ; sub_systems : Sub_system_info.t Sub_system_name.Map.t
  ; virtual_ : Modules.t Source.t option
  ; entry_modules : Module_name.t list Or_exn.t Source.t
  ; implements : (Loc.t * Lib_name.t) option
  ; default_implementation : (Loc.t * Lib_name.t) option
  ; wrapped : Wrapped.t Inherited.t option
  ; main_module_name : Main_module_name.t
  ; modes : Mode.Dict.Set.t
  ; special_builtin_support : Special_builtin_support.t option
  ; exit_module : Module_name.t option
  ; instrumentation_backend : (Loc.t * Lib_name.t) option
  ; path_kind : 'path path
  }

let name t = t.name

let version t = t.version

let dune_version t = t.dune_version

let loc t = t.loc

let requires t = t.requires

let preprocess t = t.preprocess

let ppx_runtime_deps t = t.ppx_runtime_deps

let sub_systems t = t.sub_systems

let modes t = t.modes

let archives t = t.archives

let foreign_archives t = t.foreign_archives

let native_archives t = t.native_archives

let foreign_dll_files t = t.foreign_dll_files

let foreign_objects t = t.foreign_objects

let exit_module t = t.exit_module

let instrumentation_backend t = t.instrumentation_backend

let plugins t = t.plugins

let src_dir t = t.src_dir

let enabled t = t.enabled

let status t = t.status

let kind t = t.kind

let default_implementation t = t.default_implementation

let obj_dir t = t.obj_dir

let virtual_ t = t.virtual_

let implements t = t.implements

let synopsis t = t.synopsis

let wrapped t = t.wrapped

let special_builtin_support t = t.special_builtin_support

let jsoo_runtime t = t.jsoo_runtime

let jsoo_archive t = t.jsoo_archive

let main_module_name t = t.main_module_name

let orig_src_dir t = t.orig_src_dir

let best_src_dir t = Option.value ~default:t.src_dir t.orig_src_dir

let set_version t version = { t with version }

let eval_native_archives_exn (type path) (t : path t) ~modules =
  match (t.native_archives, modules) with
  | Files f, _ -> f
  | Needs_module_info _, None ->
    Code_error.raise "missing module information" []
  | Needs_module_info f, Some modules ->
    if Modules.has_impl modules then
      [ f ]
    else
      []

let for_dune_package t ~name ~ppx_runtime_deps ~requires ~foreign_objects
    ~obj_dir ~implements ~default_implementation ~sub_systems ~modules =
  let foreign_objects = Source.External foreign_objects in
  let orig_src_dir =
    match !Clflags.store_orig_src_dir with
    | false -> t.orig_src_dir
    | true ->
      Some
        (match t.orig_src_dir with
        | Some src_dir -> src_dir
        | None -> (
          match Path.drop_build_context t.src_dir with
          | None -> t.src_dir
          | Some src_dir ->
            Path.source src_dir |> Path.to_absolute_filename |> Path.of_string))
  in
  let native_archives =
    Files (eval_native_archives_exn t ~modules:(Some modules))
  in
  { t with
    ppx_runtime_deps
  ; name
  ; requires
  ; foreign_objects
  ; obj_dir
  ; implements
  ; default_implementation
  ; sub_systems
  ; orig_src_dir
  ; native_archives
  }

let user_written_deps t =
  List.fold_left (t.virtual_deps @ t.ppx_runtime_deps) ~init:t.requires
    ~f:(fun acc s -> Lib_dep.Direct s :: acc)

let create ~loc ~path_kind ~name ~kind ~status ~src_dir ~orig_src_dir ~obj_dir
    ~version ~synopsis ~main_module_name ~sub_systems ~requires ~foreign_objects
    ~plugins ~archives ~ppx_runtime_deps ~foreign_archives ~native_archives
    ~foreign_dll_files ~jsoo_runtime ~jsoo_archive ~preprocess ~enabled
    ~virtual_deps ~dune_version ~virtual_ ~entry_modules ~implements
    ~default_implementation ~modes ~wrapped ~special_builtin_support
    ~exit_module ~instrumentation_backend =
  { loc
  ; name
  ; kind
  ; status
  ; src_dir
  ; orig_src_dir
  ; obj_dir
  ; version
  ; synopsis
  ; requires
  ; main_module_name
  ; foreign_objects
  ; plugins
  ; archives
  ; ppx_runtime_deps
  ; foreign_archives
  ; native_archives
  ; foreign_dll_files
  ; jsoo_runtime
  ; jsoo_archive
  ; preprocess
  ; enabled
  ; virtual_deps
  ; dune_version
  ; sub_systems
  ; virtual_
  ; entry_modules
  ; implements
  ; default_implementation
  ; modes
  ; wrapped
  ; special_builtin_support
  ; exit_module
  ; instrumentation_backend
  ; path_kind
  }

type external_ = Path.t t

type local = Path.Build.t t

let map t ~path_kind ~f_path ~f_obj_dir =
  let f = f_path in
  let list = List.map ~f in
  let mode_list = Mode.Dict.map ~f:list in
  let native_archives =
    match t.native_archives with
    | Needs_module_info t -> Needs_module_info (f t)
    | Files t -> Files (List.map t ~f)
  in
  { t with
    src_dir = f t.src_dir
  ; orig_src_dir = Option.map ~f t.orig_src_dir
  ; obj_dir = f_obj_dir t.obj_dir
  ; archives = mode_list t.archives
  ; plugins = mode_list t.plugins
  ; foreign_objects = Source.map ~f:(List.map ~f) t.foreign_objects
  ; foreign_archives = List.map ~f t.foreign_archives
  ; foreign_dll_files = List.map ~f t.foreign_dll_files
  ; native_archives
  ; jsoo_runtime = List.map ~f t.jsoo_runtime
  ; jsoo_archive = Option.map ~f t.jsoo_archive
  ; path_kind
  }

let map_path t ~f = map t ~path_kind:External ~f_path:f ~f_obj_dir:Fun.id

let of_local =
  map ~path_kind:External ~f_path:Path.build ~f_obj_dir:Obj_dir.of_local

let as_local_exn =
  map ~path_kind:Local ~f_path:Path.as_in_build_dir_exn
    ~f_obj_dir:Obj_dir.as_local_exn

let to_dyn path
    { loc
    ; path_kind = _
    ; name
    ; kind
    ; status
    ; src_dir
    ; orig_src_dir
    ; obj_dir
    ; version
    ; synopsis
    ; requires
    ; main_module_name
    ; foreign_objects
    ; plugins
    ; archives
    ; ppx_runtime_deps
    ; foreign_archives
    ; native_archives
    ; foreign_dll_files
    ; jsoo_runtime
    ; jsoo_archive
    ; preprocess = _
    ; enabled
    ; virtual_deps
    ; dune_version
    ; sub_systems
    ; virtual_
    ; implements
    ; default_implementation
    ; modes
    ; wrapped
    ; special_builtin_support
    ; exit_module
    ; instrumentation_backend
    ; entry_modules
    } =
  let open Dyn.Encoder in
  let snd f (_, x) = f x in
  record
    [ ("loc", Loc.to_dyn_hum loc)
    ; ("name", Lib_name.to_dyn name)
    ; ("kind", Lib_kind.to_dyn kind)
    ; ("status", Status.to_dyn status)
    ; ("src_dir", path src_dir)
    ; ("orig_src_dir", option path orig_src_dir)
    ; ("obj_dir", Obj_dir.to_dyn obj_dir)
    ; ("version", option string version)
    ; ("synopsis", option string synopsis)
    ; ("archives", Mode.Dict.to_dyn (list path) archives)
    ; ("plugins", Mode.Dict.to_dyn (list path) plugins)
    ; ("foreign_objects", Source.to_dyn (list path) foreign_objects)
    ; ("foreign_archives", list path foreign_archives)
    ; ("native_archives", dyn_of_native_archives path native_archives)
    ; ("foreign_dll_files", list path foreign_dll_files)
    ; ("jsoo_runtime", list path jsoo_runtime)
    ; ("jsoo_archive", option path jsoo_archive)
    ; ("requires", list Lib_dep.to_dyn requires)
    ; ("ppx_runtime_deps", list (snd Lib_name.to_dyn) ppx_runtime_deps)
    ; ("enabled", Enabled_status.to_dyn enabled)
    ; ("virtual_deps", list (snd Lib_name.to_dyn) virtual_deps)
    ; ("dune_version", option Dune_lang.Syntax.Version.to_dyn dune_version)
    ; ("sub_systems", Sub_system_name.Map.to_dyn Dyn.Encoder.opaque sub_systems)
    ; ("virtual_", option (Source.to_dyn Modules.to_dyn) virtual_)
    ; ( "entry_modules"
      , Source.to_dyn (Or_exn.to_dyn (list Module_name.to_dyn)) entry_modules )
    ; ("implements", option (snd Lib_name.to_dyn) implements)
    ; ( "default_implementation"
      , option (snd Lib_name.to_dyn) default_implementation )
    ; ("wrapped", option (Inherited.to_dyn Wrapped.to_dyn) wrapped)
    ; ("main_module_name", Main_module_name.to_dyn main_module_name)
    ; ("modes", Mode.Dict.Set.to_dyn modes)
    ; ( "special_builtin_support"
      , option Special_builtin_support.to_dyn special_builtin_support )
    ; ("exit_module", option Module_name.to_dyn exit_module)
    ; ( "instrumentation_backend"
      , option (snd Lib_name.to_dyn) instrumentation_backend )
    ]

let package t =
  match t.status with
  | Installed_private
  | Installed ->
    Some (Lib_name.package_name t.name)
  | Public (_, p) -> Some (Package.name p)
  | Private (_, p) -> Option.map p ~f:Package.name

let entry_modules t = t.entry_modules
