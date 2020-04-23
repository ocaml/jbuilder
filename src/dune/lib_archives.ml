open Stdune

type t =
  { dll_files : Path.Build.t list
  ; lib_files : Path.Build.t list
  }

let lib_files t = t.lib_files

let dll_files t = t.dll_files

let has_native_archive lib config contents =
  Lib_config.linker_can_create_empty_archives config
  ||
  let name = Dune_file.Library.best_name lib in
  let ml_sources = Dir_contents.ocaml contents in
  let modules =
    Result.ok_exn (Ml_sources.modules_of_library ml_sources ~name)
  in
  not (Modules.is_empty modules)

module Library = Dune_file.Library

let make ~(ctx : Context.t) ~dir ~dir_contents (lib : Library.t) =
  let { Lib_config.has_native; ext_obj; ext_dll; ext_lib; _ } =
    ctx.lib_config
  in
  let { Mode.Dict.byte; native } =
    Dune_file.Mode_conf.Set.eval lib.modes ~has_native
  in
  let if_ cond l =
    if cond then
      l
    else
      []
  in
  let lib_files =
    let virtual_library = Library.is_virtual lib in
    List.concat
      [ if_
          (byte && not virtual_library)
          [ Library.archive ~dir lib ~ext:(Mode.compiled_lib_ext Byte) ]
      ; ( if virtual_library then
          let foreign_sources = Dir_contents.foreign_sources dir_contents in
          let name = Library.best_name lib in
          let files = Foreign_sources.for_lib foreign_sources ~name in
          Foreign.Sources.object_files files ~dir ~ext_obj
        else
          Library.foreign_lib_files lib ~dir ~ext_lib )
      ; if_
          (native && not virtual_library)
          (let files =
             if has_native_archive lib ctx.lib_config dir_contents then
               [ Library.archive ~dir lib ~ext:ext_lib ]
             else
               []
           in
           let files =
             Library.archive ~dir lib ~ext:(Mode.compiled_lib_ext Native)
             :: files
           in
           if
             Dynlink_supported.get lib.dynlink
               ctx.lib_config.natdynlink_supported
           then
             files @ [ Library.archive ~dir lib ~ext:(Mode.plugin_ext Native) ]
           else
             files)
      ; List.map lib.buildable.js_of_ocaml.javascript_files
          ~f:(Path.Build.relative dir)
      ; List.map lib.install_c_headers ~f:(fun fn ->
            Path.Build.relative dir (fn ^ Foreign.header_extension))
      ]
  in
  let dll_files =
    if_
      ( byte
      && Dynlink_supported.get lib.dynlink ctx.supports_shared_libraries
      && ctx.dynamically_linked_foreign_archives )
      (Library.foreign_dll_files lib ~dir ~ext_dll)
  in
  { lib_files; dll_files }
