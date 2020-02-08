open! Stdune
open Import
module Menhir_rules = Menhir
open Dune_file
open! No_io

module Dir_modules = struct
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
            (Library.best_name lib, m))
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
          (List.concat_map libs ~f:(fun (l, m) -> by_name l.buildable m))
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

module Dir_artifacts = struct
  type t =
    { libraries : Library.t Lib_name.Map.t
    ; modules : (Path.Build.t Obj_dir.t * Module.t) Module_name.Map.t
    }

  let empty =
    { libraries = Lib_name.Map.empty; modules = Module_name.Map.empty }

  let lookup_module { modules; libraries = _ } = Module_name.Map.find modules

  let lookup_library { libraries; modules = _ } = Lib_name.Map.find libraries

  let make (d : _ Dir_with_dune.t) (libs, exes) =
    let libraries =
      List.fold_left
        ~f:(fun libraries (lib, _) ->
          let name = Lib_name.of_local lib.Library.name in
          Lib_name.Map.add_exn libraries name lib)
        ~init:Lib_name.Map.empty libs
    in
    let modules =
      let by_name modules obj_dir =
        Modules.fold_user_written ~init:modules ~f:(fun m modules ->
            Module_name.Map.add_exn modules (Module.name m) (obj_dir, m))
      in
      let init =
        List.fold_left ~init:Module_name.Map.empty
          ~f:(fun modules (e, m) ->
            by_name modules (Executables.obj_dir ~dir:d.ctx_dir e) m)
          exes
      in
      List.fold_left ~init
        ~f:(fun modules (l, m) ->
          by_name modules (Library.obj_dir ~dir:d.ctx_dir l) m)
        libs
    in
    { libraries; modules }
end

type t =
  { kind : kind
  ; dir : Path.Build.t
  ; text_files : String.Set.t
  ; modules : Dir_modules.t Memo.Lazy.Async.t
  ; foreign_sources : Foreign_sources.t Memo.Lazy.Async.t
  ; mlds : (Dune_file.Documentation.t * Path.Build.t list) list Memo.Lazy.t
  ; coq_modules : Coq_module.t list Lib_name.Map.t Memo.Lazy.t
  ; artifacts : Dir_artifacts.t Memo.Lazy.Async.t
  }

and kind =
  | Standalone
  | Group_root of t list
  | Group_part

let empty kind ~dir =
  { kind
  ; dir
  ; text_files = String.Set.empty
  ; modules = Memo.Lazy.Async.of_val Dir_modules.empty
  ; mlds = Memo.Lazy.of_val []
  ; foreign_sources = Memo.Lazy.Async.of_val Foreign_sources.empty
  ; coq_modules = Memo.Lazy.of_val Lib_name.Map.empty
  ; artifacts = Memo.Lazy.Async.of_val Dir_artifacts.empty
  }

type gen_rules_result =
  | Standalone_or_root of t * t list
  | Group_part of Path.Build.t

let dir t = t.dir

let artifacts t = Memo.Lazy.Async.force t.artifacts

let dirs t =
  match t.kind with
  | Standalone -> [ t ]
  | Group_root subs -> t :: subs
  | Group_part ->
    Code_error.raise "Dir_contents.dirs called on a group part"
      [ ("dir", Path.Build.to_dyn t.dir) ]

let text_files t = t.text_files

let modules_of_library t ~name =
  let open Fiber.O in
  let+ modules = Memo.Lazy.Async.force t.modules in
  Lib_name.Map.find_exn modules.libraries name

let modules_of_executables t ~obj_dir ~first_exe =
  let open Fiber.O in
  let+ modules = Memo.Lazy.Async.force t.modules in
  (* we need to relocate the alias module to its own directory. *)
  let src_dir = Path.build (Obj_dir.obj_dir obj_dir) in
  String.Map.find_exn modules.executables first_exe
  |> Modules.relocate_alias_module ~src_dir

let foreign_sources_of_executables t ~first_exe =
  let open Fiber.O in
  let+ foreign_sources = Memo.Lazy.Async.force t.foreign_sources in
  Foreign_sources.for_exes foreign_sources ~first_exe

let foreign_sources_of_library t ~name =
  let open Fiber.O in
  let+ foreign_sources = Memo.Lazy.Async.force t.foreign_sources in
  Foreign_sources.for_lib foreign_sources ~name

let foreign_sources_of_archive t ~archive_name =
  let open Fiber.O in
  let+ foreign_sources = Memo.Lazy.Async.force t.foreign_sources in
  Foreign_sources.for_archive foreign_sources ~archive_name

let lookup_module t name =
  let open Fiber.O in
  let+ modules = Memo.Lazy.Async.force t.modules in
  Module_name.Map.find modules.rev_map name

let mlds t (doc : Documentation.t) =
  let map = Memo.Lazy.force t.mlds in
  match
    List.find_map map ~f:(fun (doc', x) ->
        Option.some_if (Loc.equal doc.loc doc'.loc) x)
  with
  | Some x -> x
  | None ->
    Code_error.raise "Dir_contents.mlds"
      [ ("doc", Loc.to_dyn doc.loc)
      ; ( "available"
        , Dyn.Encoder.(list Loc.to_dyn)
            (List.map map ~f:(fun (d, _) -> d.Documentation.loc)) )
      ]

let coq_modules_of_library t ~name =
  let map = Memo.Lazy.force t.coq_modules in
  Lib_name.Map.find_exn map name

let modules_of_files ~dialects ~dir ~files =
  let dir = Path.build dir in
  let make_module dialect base fn =
    (Module_name.of_string base, Module.File.make dialect (Path.relative dir fn))
  in
  let impl_files, intf_files =
    String.Set.to_list files
    |> List.filter_partition_map ~f:(fun fn ->
           (* we aren't using Filename.extension because we want to handle
              filenames such as foo.cppo.ml *)
           match String.lsplit2 fn ~on:'.' with
           | None -> Skip
           | Some (s, ext) -> (
             match Dialect.DB.find_by_extension dialects ("." ^ ext) with
             | Some (dialect, Ml_kind.Impl) -> Left (make_module dialect s fn)
             | Some (dialect, Ml_kind.Intf) -> Right (make_module dialect s fn)
             | None -> Skip ))
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

let build_mlds_map (d : _ Dir_with_dune.t) ~files =
  let dir = d.ctx_dir in
  let mlds =
    Memo.lazy_ (fun () ->
        String.Set.fold files ~init:String.Map.empty ~f:(fun fn acc ->
            match String.lsplit2 fn ~on:'.' with
            | Some (s, "mld") -> String.Map.set acc s fn
            | _ -> acc))
  in
  List.filter_map d.data ~f:(function
    | Documentation doc ->
      let mlds =
        let mlds = Memo.Lazy.force mlds in
        Ordered_set_lang.Unordered_string.eval doc.mld_files
          ~key:(fun x -> x)
          ~parse:(fun ~loc s ->
            match String.Map.find mlds s with
            | Some s -> s
            | None ->
              User_error.raise ~loc
                [ Pp.textf "%s.mld doesn't exist in %s" s
                    (Path.to_string_maybe_quoted
                       (Path.drop_optional_build_context (Path.build dir)))
                ])
          ~standard:mlds
      in
      Some (doc, List.map (String.Map.values mlds) ~f:(Path.Build.relative dir))
    | _ -> None)

let coq_modules_of_files ~subdirs =
  let filter_v_files (dir, local, files) =
    ( dir
    , local
    , String.Set.filter files ~f:(fun f -> Filename.check_suffix f ".v") )
  in
  let subdirs = List.map subdirs ~f:filter_v_files in
  let build_mod_dir (dir, prefix, files) =
    String.Set.to_list files
    |> List.map ~f:(fun file ->
           let name, _ = Filename.split_extension file in
           let name = Coq_module.Name.make name in
           Coq_module.make ~source:(Path.Build.relative dir file) ~prefix ~name)
  in
  let modules = List.concat_map ~f:build_mod_dir subdirs in
  modules

(* TODO: Build reverse map and check duplicates, however, are duplicates harmful?
 * In Coq all libs are "wrapped" so including a module twice is not so bad.
 *)
let build_coq_modules_map (d : _ Dir_with_dune.t) ~dir ~modules =
  List.fold_left d.data ~init:Lib_name.Map.empty ~f:(fun map ->
    function
    | Coq.T coq ->
      let modules = Coq_module.eval coq.modules ~dir ~standard:modules in
      Lib_name.Map.set map (Dune_file.Coq.best_name coq) modules
    | _ -> map)

module rec Load : sig
  val get : Super_context.t -> dir:Path.Build.t -> t Fiber.t

  val gen_rules :
    Super_context.t -> dir:Path.Build.t -> gen_rules_result Fiber.t
end = struct
  let virtual_modules sctx vlib : _ Fiber.t =
    let info = Lib.info vlib in
    let open Fiber.O in
    let+ modules =
      match Option.value_exn (Lib_info.virtual_ info) with
      | External modules -> Fiber.return modules
      | Local ->
        let src_dir = Lib_info.src_dir info |> Path.as_in_build_dir_exn in
        let* t = Load.get sctx ~dir:src_dir in
        modules_of_library t ~name:(Lib.name vlib)
    in
    let existing_virtual_modules = Modules.virtual_module_names modules in
    let allow_new_public_modules =
      Modules.wrapped modules |> Wrapped.to_bool |> not
    in
    { Modules_field_evaluator.Implementation.existing_virtual_modules
    ; allow_new_public_modules
    }

  let make_lib_modules (d : _ Dir_with_dune.t) ~sctx ~(lib : Library.t) ~modules
      =
    let src_dir = d.ctx_dir in
    let kind, main_module_name, wrapped =
      match lib.implements with
      | None ->
        (* In the two following pattern matching, we can only get [From _] if
           [lib] is an implementation. Since we know that it is not one because
           of the above [match lib.implements with ...], we know that we can't
           get [From _]. That's why we have these [assert false]. *)
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
        (Fiber.return kind, main_module_name, wrapped)
      | Some _ ->
        assert (Option.is_none lib.virtual_modules);
        let resolved =
          let name = Library.best_name lib in
          Lib.DB.find_even_when_hidden (Scope.libs d.scope) name
          (* can't happen because this library is defined using the current
             stanza *)
          |> Option.value_exn
        in
        (* diml: this [Result.ok_exn] means that if the user writes an invalid
           [implements] field, we will get an error immediately even if the
           library is not built. We should change this to carry the [Or_exn.t] a
           bit longer. *)
        let vlib =
          Result.ok_exn
            (* This [Option.value_exn] is correct because the above
               [lib.implements] is [Some _] and this [lib] variable correspond
               to the same library. *)
            (Option.value_exn (Lib.implements resolved))
        in
        let kind =
          let open Fiber.O in
          let+ impl = virtual_modules sctx vlib in
          Modules_field_evaluator.Implementation impl
        in
        let main_module_name, wrapped =
          Result.ok_exn
            (let open Result.O in
            let* main_module_name = Lib.main_module_name resolved in
            let+ wrapped = Lib.wrapped resolved in
            (main_module_name, Option.value_exn wrapped))
        in
        (kind, main_module_name, wrapped)
    in
    let open Fiber.O in
    let+ kind = kind in
    let modules =
      Modules_field_evaluator.eval ~modules ~buildable:lib.buildable ~kind
        ~private_modules:
          (Option.value ~default:Ordered_set_lang.standard lib.private_modules)
    in
    let stdlib = lib.stdlib in
    let implements = Option.is_some lib.implements in
    let _loc, lib_name = lib.name in
    Modules.lib ~stdlib ~implements ~lib_name ~src_dir ~modules
      ~main_module_name ~wrapped

  let libs_and_exes sctx (d : _ Dir_with_dune.t) ~modules =
    List.filter_partition_map d.data ~f:(fun stanza ->
        match (stanza : Stanza.t) with
        | Library lib ->
          let modules = make_lib_modules d ~sctx ~modules ~lib in
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
              Modules.exe_wrapped ~src_dir:d.ctx_dir ~modules
            else
              Modules.exe_unwrapped modules
          in
          Right (exes, modules)
        | _ -> Skip)

  (* As a side-effect, setup user rules and copy_files rules. *)
  let load_text_files sctx ft_dir
      { Dir_with_dune.ctx_dir = dir
      ; src_dir
      ; scope = _
      ; data = stanzas
      ; dune_version = _
      } =
    (* Interpret a few stanzas in order to determine the list of files generated
       by the user. *)
    let open Fiber.O in
    let* loaded_dir = Load.get sctx ~dir in
    let lookup ~f ~dir name =
      let+ artifacts = artifacts loaded_dir in
      f artifacts name
    in
    let lookup_module = lookup ~f:Dir_artifacts.lookup_module in
    let lookup_library = lookup ~f:Dir_artifacts.lookup_library in
    let expander = Super_context.expander sctx ~dir in
    let expander = Expander.set_lookup_module expander ~lookup_module in
    let expander = Expander.set_lookup_library expander ~lookup_library in
    let expander = Expander.set_artifacts_dynamic expander true in
    let open Fiber.O in
    let+ generated_files =
      Fiber.parallel_map stanzas ~f:(fun stanza ->
          match (stanza : Stanza.t) with
          | Coqpp.T { modules; _ } ->
            Fiber.return (List.map modules ~f:(fun m -> m ^ ".ml"))
          | Menhir.T menhir -> Fiber.return (Menhir_rules.targets menhir)
          | Rule rule ->
            Simple_rules.user_rule sctx rule ~dir ~expander
            |> Path.Build.Set.to_list
            |> List.map ~f:Path.Build.basename
            |> Fiber.return
          | Copy_files def ->
            let open Fiber.O in
            let+ files =
              Simple_rules.copy_files sctx def ~src_dir ~dir ~expander
            in
            files |> Path.Set.to_list |> List.map ~f:Path.basename
          | Library { buildable; _ }
          | Executables { buildable; _ } ->
            (* Manually add files generated by the (select ...) dependencies *)
            List.filter_map buildable.libraries ~f:(fun dep ->
                match (dep : Lib_dep.t) with
                | Re_export _
                | Direct _ ->
                  None
                | Select s -> Some s.result_fn)
            |> Fiber.return
          | _ -> Fiber.return [])
      |> Fiber.map ~f:(fun files -> String.Set.of_list (List.concat files))
    in
    String.Set.union generated_files (File_tree.Dir.files ft_dir)

  type result0_here =
    { t : t
    ; (* [rules] includes rules for subdirectories too *)
      rules : Rules.t option
    ; subdirs : t Path.Build.Map.t
    }

  type result0 =
    | See_above of Path.Build.t
    | Here of result0_here

  module Key = struct
    module Super_context = Super_context.As_memo_key

    type t = Super_context.t * Path.Build.t

    let to_dyn (sctx, path) =
      Dyn.Tuple [ Super_context.to_dyn sctx; Path.Build.to_dyn path ]

    let equal = Tuple.T2.equal Super_context.equal Path.Build.equal

    let hash = Tuple.T2.hash Super_context.hash Path.Build.hash
  end

  let check_no_qualified loc qualif_mode =
    if qualif_mode = Include_subdirs.Qualified then
      User_error.raise ~loc
        [ Pp.text "(include_subdirs qualified) is not supported yet" ]

  let check_no_unqualified loc qualif_mode =
    if qualif_mode = Include_subdirs.Unqualified then
      User_error.raise ~loc
        [ Pp.text "(include_subdirs qualified) is not supported yet" ]

  let get0_impl (sctx, dir) : result0 Fiber.t =
    let dir_status_db = Super_context.dir_status_db sctx in
    let ctx = Super_context.context sctx in
    match Dir_status.DB.get dir_status_db ~dir with
    | Is_component_of_a_group_but_not_the_root { group_root; stanzas = _ } ->
      Fiber.return (See_above group_root)
    | Standalone x -> (
      match x with
      | Some (_, None)
      | None ->
        Fiber.return
          (Here
             { t = empty Standalone ~dir
             ; rules = None
             ; subdirs = Path.Build.Map.empty
             })
      | Some (ft_dir, Some d) ->
        let open Fiber.O in
        let+ files, rules =
          Rules.collect_opt_async (fun () -> load_text_files sctx ft_dir d)
        in
        let libs_and_exes =
          let dialects = Dune_project.dialects (Scope.project d.scope) in
          Memo.Lazy.Async.create (fun () ->
              let libs, exes =
                libs_and_exes sctx d
                  ~modules:(modules_of_files ~dialects ~dir:d.ctx_dir ~files)
              in
              let+ libs =
                Fiber.parallel_map libs ~f:(fun (libs, modules) ->
                    let+ modules = modules in
                    (libs, modules))
              in
              (libs, exes))
        in
        Here
          { t =
              { kind = Standalone
              ; dir
              ; text_files = files
              ; modules = Memo.Lazy.Async.map ~f:Dir_modules.make libs_and_exes
              ; mlds = Memo.Lazy.create (fun () -> build_mlds_map d ~files)
              ; foreign_sources =
                  Memo.Async.Lazy.create (fun () ->
                      let dune_version = d.dune_version in
                      Foreign_sources.make d ~ext_obj:ctx.lib_config.ext_obj
                        ~sources:
                          (Foreign.Sources.Unresolved.load ~dune_version
                             ~dir:d.ctx_dir ~files))
              ; coq_modules =
                  Memo.Lazy.Async.create (fun () ->
                      build_coq_modules_map d ~dir:d.ctx_dir
                        ~modules:
                          (coq_modules_of_files ~subdirs:[ (dir, [], files) ]))
              ; artifacts =
                  Memo.Lazy.Async.map ~f:(Dir_artifacts.make d) libs_and_exes
              }
          ; rules
          ; subdirs = Path.Build.Map.empty
          } )
    | Group_root (ft_dir, qualif_mode, d) ->
      let rec walk ft_dir ~dir ~local acc =
        match Dir_status.DB.get dir_status_db ~dir with
        | Is_component_of_a_group_but_not_the_root
            { stanzas = d; group_root = _ } ->
          let files =
            match d with
            | None -> Fiber.return (File_tree.Dir.files ft_dir)
            | Some d -> load_text_files sctx ft_dir d
          in
          walk_children ft_dir ~dir ~local ((dir, List.rev local, files) :: acc)
        | _ -> acc
      and walk_children ft_dir ~dir ~local acc =
        File_tree.Dir.fold_sub_dirs ft_dir ~init:acc ~f:(fun name ft_dir acc ->
            let dir = Path.Build.relative dir name in
            let local =
              if qualif_mode = Qualified then
                name :: local
              else
                local
            in
            walk ft_dir ~dir ~local acc)
      in
      let open Fiber.O in
      let* (files, (subdirs : (Path.Build.t * _ * _) list)), rules =
        Rules.collect_opt_async (fun () ->
            let* files = load_text_files sctx ft_dir d in
            let children = walk_children ft_dir ~dir ~local:[] [] in
            let+ subdirs =
              Fiber.sequential_map children ~f:(fun (dirs, locals, files) ->
                  let+ files = files in
                  (dirs, locals, files))
            in
            (files, subdirs))
      in
      let libs_and_exes =
        Memo.lazy_ (fun () ->
            let loc =
              Loc.in_file
                (Path.source
                   ( match File_tree.Dir.dune_file ft_dir with
                   | Some d -> File_tree.Dune_file.path d
                   | None ->
                     Path.Source.relative
                       (File_tree.Dir.path ft_dir)
                       "_unknown_" ))
            in
            check_no_qualified loc qualif_mode;
            let modules =
              let dialects = Dune_project.dialects (Scope.project d.scope) in
              List.fold_left ((dir, [], files) :: subdirs)
                ~init:Module_name.Map.empty
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
                        ]))
            in
            libs_and_exes sctx d ~modules)
      in
      let modules = Memo.Lazy.map ~f:Dir_modules.make libs_and_exes in
      let artifacts = Memo.Lazy.map ~f:(Dir_artifacts.make d) libs_and_exes in
      let foreign_sources =
        Memo.lazy_ (fun () ->
            check_no_qualified Loc.none qualif_mode;
            let dune_version = d.dune_version in
            let init = String.Map.empty in
            let sources =
              List.fold_left ((dir, [], files) :: subdirs) ~init
                ~f:(fun acc (dir, _local, files) ->
                  let sources =
                    Foreign.Sources.Unresolved.load ~dir ~dune_version ~files
                  in
                  String.Map.Multi.rev_union sources acc)
            in
            Foreign_sources.make d ~sources ~ext_obj:ctx.lib_config.ext_obj)
      in
      let coq_modules =
        Memo.lazy_ (fun () ->
            check_no_unqualified Loc.none qualif_mode;
            build_coq_modules_map d ~dir:d.ctx_dir
              ~modules:
                (coq_modules_of_files ~subdirs:((dir, [], files) :: subdirs)))
      in
      let subdirs =
        List.map subdirs ~f:(fun (dir, _local, files) ->
            { kind = Group_part
            ; dir
            ; text_files = files
            ; modules
            ; foreign_sources
            ; mlds = Memo.lazy_ (fun () -> build_mlds_map d ~files)
            ; coq_modules
            ; artifacts
            })
      in
      let t =
        { kind = Group_root subdirs
        ; dir
        ; text_files = files
        ; modules
        ; foreign_sources
        ; mlds = Memo.lazy_ (fun () -> build_mlds_map d ~files)
        ; coq_modules
        ; artifacts
        }
      in
      Fiber.return
        (Here
           { t
           ; rules
           ; subdirs =
               Path.Build.Map.of_list_map_exn subdirs ~f:(fun x -> (x.dir, x))
           })

  let memo0 =
    let module Output = struct
      type t = result0

      let to_dyn _ = Dyn.Opaque
    end in
    Memo.create "dir-contents-get0"
      ~input:(module Key)
      ~output:(Simple (module Output))
      ~doc:"dir contents" ~visibility:Hidden Async get0_impl

  let get sctx ~dir =
    let open Fiber.O in
    let* contents = Memo.exec memo0 (sctx, dir) in
    match contents with
    | Here { t; rules = _; subdirs = _ } -> Fiber.return t
    | See_above group_root -> (
      let+ group_root = Memo.exec memo0 (sctx, group_root) in
      match group_root with
      | See_above _ -> assert false
      | Here { t; rules = _; subdirs = _ } -> t )

  let gen_rules sctx ~dir =
    let open Fiber.O in
    let+ contents = Memo.exec memo0 (sctx, dir) in
    match contents with
    | See_above group_root -> Group_part group_root
    | Here { t; rules; subdirs } ->
      Rules.produce_opt rules;
      Standalone_or_root (t, Path.Build.Map.values subdirs)
end

include Load
