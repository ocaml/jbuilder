open Import
open Dune_file
module Modules_group = Modules

module Modules = struct
  type t =
    { libraries : Modules.t Lib_name.Map.t
    ; executables : Modules.t String.Map.t
    ; (* Map from modules to the buildable they are part of *)
      rev_map : Buildable.t Module_name.Map.t
    }

  let empty =
    { libraries = Lib_name.Map.empty
    ; executables = String.Map.empty
    ; rev_map = Module_name.Map.empty
    }

  let make (libs, exes) =
    let libraries =
      match
        Lib_name.Map.of_list_map libs ~f:(fun (lib, m) ->
            (Library.best_name lib, Result.ok_exn m))
      with
      | Ok x -> x
      | Error (name, _, (lib2, _)) ->
        User_error.raise ~loc:lib2.buildable.loc
          [ Pp.textf "Library %S appears for the second time in this directory"
              (Lib_name.to_string name)
          ]
    in
    let executables =
      match
        String.Map.of_list_map exes ~f:(fun (exes, m) ->
            (snd (List.hd exes.Executables.names), m))
      with
      | Ok x -> x
      | Error (name, _, (exes2, _)) ->
        User_error.raise ~loc:exes2.buildable.loc
          [ Pp.textf
              "Executable %S appears for the second time in this directory" name
          ]
    in
    let rev_map =
      let rev_modules =
        let by_name buildable =
          Modules.fold_user_written ~init:[] ~f:(fun m acc ->
              (Module.name m, buildable) :: acc)
        in
        List.rev_append
          (List.concat_map libs ~f:(fun (l, m) ->
               by_name l.buildable (Result.ok_exn m)))
          (List.concat_map exes ~f:(fun (e, m) -> by_name e.buildable m))
      in
      match Module_name.Map.of_list rev_modules with
      | Ok x -> x
      | Error (name, _, _) ->
        let open Module_name.Infix in
        let locs =
          List.filter_map rev_modules ~f:(fun (n, b) ->
              Option.some_if (n = name) b.loc)
          |> List.sort ~compare:Poly.compare
        in
        User_error.raise
          ~loc:(Loc.drop_position (List.hd locs))
          [ Pp.textf "Module %S is used in several stanzas:"
              (Module_name.to_string name)
          ; Pp.enumerate locs ~f:(fun loc ->
                Pp.verbatim (Loc.to_file_colon_line loc))
          ; Pp.text
              "To fix this error, you must specify an explicit \"modules\" \
               field in every library, executable, and executables stanzas in \
               this dune file. Note that each module cannot appear in more \
               than one \"modules\" field - it must belong to a single library \
               or executable."
          ]
    in
    { libraries; executables; rev_map }
end

module Artifacts = struct
  type t =
    { libraries : Library.t Lib_name.Map.t
    ; modules : (Path.Build.t Obj_dir.t * Module.t) Module_name.Map.t Or_exn.t
    }

  let empty =
    { libraries = Lib_name.Map.empty; modules = Ok Module_name.Map.empty }

  let lookup_module { modules; libraries = _ } m =
    let open Result.O in
    let+ modules = modules in
    Module_name.Map.find modules m

  let lookup_library { libraries; modules = _ } = Lib_name.Map.find libraries

  let make (d : _ Dir_with_dune.t) (libs, exes) =
    let libraries =
      List.fold_left
        ~f:(fun libraries (lib, _) ->
          let name = Lib_name.of_local lib.Library.name in
          (* We check for duplicates when creating modules.libraries *)
          Lib_name.Map.add_exn libraries name lib)
        ~init:Lib_name.Map.empty libs
    in
    let modules =
      let by_name modules obj_dir =
        Modules_group.fold_user_written ~init:modules ~f:(fun m modules ->
            (* We check for duplicates when creating modules.rev_map *)
            Module_name.Map.add_exn modules (Module.name m) (obj_dir, m))
      in
      let init =
        List.fold_left exes ~init:Module_name.Map.empty
          ~f:(fun modules (e, m) ->
            by_name modules (Executables.obj_dir ~dir:d.ctx_dir e) m)
      in
      Result.List.fold_left libs ~init ~f:(fun modules (l, m) ->
          let open Result.O in
          let+ m = m in
          by_name modules (Library.obj_dir ~dir:d.ctx_dir l) m)
    in
    { libraries; modules }
end

type t =
  { modules : Modules.t Or_exn.t Memo.Lazy.t
  ; artifacts : Artifacts.t Or_exn.t Memo.Lazy.t
  }

let empty =
  { modules = Memo.Lazy.of_val (Ok Modules.empty)
  ; artifacts = Memo.Lazy.of_val (Ok Artifacts.empty)
  }

let artifacts t = Memo.Lazy.force t.artifacts

let modules_of_files ~dialects ~dir ~files =
  let dir = Path.build dir in
  let impl_files, intf_files =
    let make_module dialect name fn =
      (name, Module.File.make dialect (Path.relative dir fn))
    in
    let loc = Loc.in_dir dir in
    String.Set.to_list files
    |> List.filter_partition_map ~f:(fun fn ->
           (* we aren't using Filename.extension because we want to handle
              filenames such as foo.cppo.ml *)
           match String.lsplit2 fn ~on:'.' with
           | None -> Skip
           | Some (s, ext) -> (
             match Dialect.DB.find_by_extension dialects ("." ^ ext) with
             | None -> Skip
             | Some (dialect, ml_kind) -> (
               let name = Module_name.of_string_allow_invalid (loc, s) in
               let module_ = make_module dialect name fn in
               match ml_kind with
               | Impl -> Left module_
               | Intf -> Right module_ ) ))
  in
  let parse_one_set (files : (Module_name.t * Module.File.t) list) =
    match Module_name.Map.of_list files with
    | Ok x -> x
    | Error (name, f1, f2) ->
      let src_dir = Path.drop_build_context_exn dir in
      User_error.raise
        [ Pp.textf "Too many files for module %s in %s:"
            (Module_name.to_string name)
            (Path.Source.to_string_maybe_quoted src_dir)
        ; Pp.textf "- %s" (Path.to_string_maybe_quoted f1.path)
        ; Pp.textf "- %s" (Path.to_string_maybe_quoted f2.path)
        ]
  in
  let impls = parse_one_set impl_files in
  let intfs = parse_one_set intf_files in
  Module_name.Map.merge impls intfs ~f:(fun name impl intf ->
      Some (Module.Source.make name ?impl ?intf))

let modules_of_library t ~name =
  let open Result.O in
  let+ modules = Memo.Lazy.force t.modules in
  Lib_name.Map.find_exn modules.libraries name

let modules_of_executables t ~obj_dir ~first_exe =
  let open Result.O in
  let+ modules = Memo.Lazy.force t.modules in
  (* we need to relocate the alias module to its own directory. *)
  let src_dir = Path.build (Obj_dir.obj_dir obj_dir) in
  String.Map.find_exn modules.executables first_exe
  |> Modules_group.relocate_alias_module ~src_dir

let lookup_module (t : t) name =
  let open Result.O in
  let+ modules = Memo.Lazy.force t.modules in
  Module_name.Map.find modules.rev_map name

let virtual_modules lookup_vlib vlib =
  let info = Lib.info vlib in
  let open Result.O in
  let+ modules =
    match Option.value_exn (Lib_info.virtual_ info) with
    | External modules -> Ok modules
    | Local ->
      let src_dir = Lib_info.src_dir info |> Path.as_in_build_dir_exn in
      let t = lookup_vlib ~dir:src_dir in
      modules_of_library t ~name:(Lib.name vlib)
  in
  let existing_virtual_modules = Modules_group.virtual_module_names modules in
  let allow_new_public_modules =
    Modules_group.wrapped modules |> Wrapped.to_bool |> not
  in
  { Modules_field_evaluator.Implementation.existing_virtual_modules
  ; allow_new_public_modules
  }

let make_lib_modules (d : _ Dir_with_dune.t) ~lookup_vlib ~(lib : Library.t)
    ~modules =
  let src_dir = d.ctx_dir in
  let open Result.O in
  let+ kind, main_module_name, wrapped =
    match lib.implements with
    | None ->
      (* In the two following pattern matching, we can only get [From _] if
         [lib] is an implementation. Since we know that it is not one because of
         the above [match lib.implements with ...], we know that we can't get
         [From _]. That's why we have these [assert false]. *)
      let main_module_name =
        match Library.main_module_name lib with
        | This x -> x
        | From _ -> assert false
      in
      let wrapped =
        match lib.wrapped with
        | This x -> x
        | From _ -> assert false
      in
      let kind : Modules_field_evaluator.kind =
        match lib.virtual_modules with
        | None -> Exe_or_normal_lib
        | Some virtual_modules -> Virtual { virtual_modules }
      in
      Ok (kind, main_module_name, wrapped)
    | Some _ ->
      assert (Option.is_none lib.virtual_modules);
      let resolved =
        let name = Library.best_name lib in
        Lib.DB.find_even_when_hidden (Scope.libs d.scope) name
        (* can't happen because this library is defined using the current stanza *)
        |> Option.value_exn
      in
      let* vlib =
        (* This [Option.value_exn] is correct because the above [lib.implements]
           is [Some _] and this [lib] variable correspond to the same library. *)
        Option.value_exn (Lib.implements resolved)
      in
      let* (kind : Modules_field_evaluator.kind) =
        let+ virtual_modules = virtual_modules lookup_vlib vlib in
        Modules_field_evaluator.Implementation virtual_modules
      in
      let+ main_module_name, wrapped =
        let* main_module_name = Lib.main_module_name resolved in
        let+ wrapped = Lib.wrapped resolved in
        (main_module_name, Option.value_exn wrapped)
      in
      (kind, main_module_name, wrapped)
  in
  let modules =
    Modules_field_evaluator.eval ~modules ~buildable:lib.buildable ~kind
      ~private_modules:
        (Option.value ~default:Ordered_set_lang.standard lib.private_modules)
  in
  let stdlib = lib.stdlib in
  let implements = Option.is_some lib.implements in
  let _loc, lib_name = lib.name in
  Modules_group.lib ~stdlib ~implements ~lib_name ~src_dir ~modules
    ~main_module_name ~wrapped

let libs_and_exes (d : _ Dir_with_dune.t) ~lookup_vlib ~modules =
  List.filter_partition_map d.data ~f:(fun stanza ->
      match (stanza : Stanza.t) with
      | Library lib ->
        let modules = make_lib_modules d ~lookup_vlib ~modules ~lib in
        Left (lib, modules)
      | Executables exes
      | Tests { exes; _ } ->
        let modules =
          Modules_field_evaluator.eval ~modules ~buildable:exes.buildable
            ~kind:Modules_field_evaluator.Exe_or_normal_lib
            ~private_modules:Ordered_set_lang.standard
        in
        let modules =
          let project = Scope.project d.scope in
          if Dune_project.wrapped_executables project then
            Modules_group.exe_wrapped ~src_dir:d.ctx_dir ~modules
          else
            Modules_group.exe_unwrapped modules
        in
        Right (exes, modules)
      | _ -> Skip)

let check_no_qualified (loc, include_subdirs) =
  if include_subdirs = Dune_file.Include_subdirs.Include Qualified then
    User_error.raise ~loc
      [ Pp.text "(include_subdirs qualified) is not supported yet" ]

let make (d : _ Dir_with_dune.t) ~loc ~lookup_vlib ~include_subdirs ~dirs =
  let open Result.O in
  let libs_and_exes =
    Memo.lazy_ (fun () ->
        let+ modules =
          User_error.catch (fun () ->
              check_no_qualified include_subdirs;
              let dialects = Dune_project.dialects (Scope.project d.scope) in
              List.fold_left dirs ~init:Module_name.Map.empty
                ~f:(fun acc ((dir : Path.Build.t), _local, files) ->
                  let modules = modules_of_files ~dialects ~dir ~files in
                  Module_name.Map.union acc modules ~f:(fun name x y ->
                      User_error.raise ~loc
                        [ Pp.textf "Module %S appears in several directories:"
                            (Module_name.to_string name)
                        ; Pp.textf "- %s"
                            (Path.to_string_maybe_quoted
                               (Module.Source.src_dir x))
                        ; Pp.textf "- %s"
                            (Path.to_string_maybe_quoted
                               (Module.Source.src_dir y))
                        ; Pp.text
                            "This is not allowed, please rename one of them."
                        ])))
        in
        libs_and_exes d ~lookup_vlib ~modules)
  in
  let modules =
    Memo.Lazy.map
      ~f:(fun libs_and_exes ->
        let* libs_and_exes = libs_and_exes in
        User_error.catch (fun () -> Modules.make libs_and_exes))
      libs_and_exes
  in
  let artifacts =
    Memo.Lazy.map ~f:(Result.map ~f:(Artifacts.make d)) libs_and_exes
  in
  { modules; artifacts }
