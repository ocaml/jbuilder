open! Dune_engine
open! Stdune
open Import
open! No_io

let exe_name = "utop"

let utop_dir_basename = ".utop"

let utop_exe =
  (* Use the [.exe] version. As the utop executable is declared with [(modes
     (byte))], the [.exe] correspond the bytecode linked in custom mode. We do
     that so that it works without hassle when generating a utop for a library
     with C stubs. *)
  Filename.concat utop_dir_basename (exe_name ^ Mode.exe_ext Mode.Native)

let source ~dir =
  Toplevel.Source.make
    ~dir:(Path.Build.relative dir utop_dir_basename)
    ~loc:(Loc.in_dir (Path.build dir))
    ~main:"UTop_main.main ();" ~name:exe_name

let is_utop_dir dir = Path.Build.basename dir = utop_dir_basename

let libs_and_ppx_under_dir sctx ~db ~dir =
  (let open Option.O in
  let* dir = Path.drop_build_context dir in
  let+ dir = File_tree.find_dir dir in
  File_tree.Dir.fold_dune_files dir ~init:([], [])
    ~f:(fun ~basename:_ dir _dune_file (acc, pps) ->
      let dir =
        Path.Build.append_source (Super_context.context sctx).build_dir
          (File_tree.Dir.path dir)
      in
      match Super_context.stanzas_in sctx ~dir with
      | None -> (acc, pps)
      | Some (d : _ Dir_with_dune.t) ->
        List.fold_left d.data ~init:(acc, pps) ~f:(fun (acc, pps) -> function
          | Dune_file.Library l -> (
            match
              Lib.DB.resolve_when_exists db
                (l.buildable.loc, Dune_file.Library.best_name l)
            with
            | None
            | Some (Error _) ->
              (acc, pps)
              (* library is defined but outside our scope or is disabled *)
            | Some (Ok lib) ->
              (* still need to make sure that it's not coming from an external
                 source *)
              let info = Lib.info lib in
              let src_dir = Lib_info.src_dir info in
              (* Only select libraries that are not implementations.
                 Implementations are selected using the default implementation
                 feature. *)
              let not_impl = Option.is_none (Lib_info.implements info) in
              if not_impl && Path.is_descendant ~of_:(Path.build dir) src_dir
              then
                match Lib_info.kind info with
                | Lib_kind.Ppx_rewriter _
                | Ppx_deriver _ ->
                  (lib :: acc, (Lib_info.loc info, Lib_info.name info) :: pps)
                | Normal -> (lib :: acc, pps)
              else
                (acc, pps)
              (* external lib with a name matching our private name *))
          | _ -> (acc, pps))))
  |> Option.value ~default:([], [])

let libs_under_dir sctx ~db ~dir = fst (libs_and_ppx_under_dir sctx ~db ~dir)

let setup sctx ~dir =
  let expander = Super_context.expander sctx ~dir in
  let scope = Super_context.find_scope_by_dir sctx dir in
  let db = Scope.libs scope in
  let libs, pps = libs_and_ppx_under_dir sctx ~db ~dir:(Path.build dir) in
  let pps =
    if List.is_empty pps then
      Preprocess.No_preprocessing
    else
      Preprocess.Pps { loc = Loc.none; pps; flags = []; staged = false }
  in
  let preprocess = Module_name.Per_item.for_all pps in
  let preprocessing =
    Preprocessing.make sctx ~dir ~expander ~scope ~dep_kind:Required
      ~lib_name:None ~lint:Dune_file.Lint.no_lint ~preprocess
      ~preprocessor_deps:[]
  in
  let source = source ~dir in
  let obj_dir = Toplevel.Source.obj_dir source in
  let loc = Toplevel.Source.loc source in
  let modules = Toplevel.Source.modules source preprocessing in
  let requires =
    let open Result.O in
    (loc, Lib_name.of_string "utop")
    |> Lib.DB.resolve db
    >>| (fun utop -> utop :: libs)
    >>= Lib.closure ~linking:true
  in
  let flags =
    let project = Scope.project scope in
    let dune_version = Dune_project.dune_version project in
    let profile = (Super_context.context sctx).profile in
    Ocaml_flags.append_common
      (Ocaml_flags.default ~dune_version ~profile)
      [ "-w"; "-24" ]
  in
  let cctx =
    Compilation_context.create () ~super_context:sctx ~expander ~scope ~obj_dir
      ~modules ~opaque:(Explicit false)
      ~requires_link:(lazy requires)
      ~requires_compile:requires ~flags ~js_of_ocaml:None ~package:None
      ~preprocessing
  in
  let toplevel = Toplevel.make ~cctx ~source ~preprocess:pps in
  Toplevel.setup_rules toplevel
