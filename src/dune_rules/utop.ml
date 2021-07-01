open! Dune_engine
open! Stdune
open Import
open! No_io
open Memo.Build.O

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

module Libs_and_ppxs =
  Monoid.Product
    (Monoid.Appendable_list (struct
      type t = Lib.t
    end))
    (Monoid.Appendable_list (struct
      type t = Loc.t * Lib_name.t
    end))

module Source_tree_map_reduce =
  Source_tree.Dir.Make_map_reduce (Memo.Build) (Libs_and_ppxs)

let libs_and_ppx_under_dir sctx ~db ~dir =
  (match Path.drop_build_context dir with
  | None -> Memo.Build.return None
  | Some dir -> Source_tree.find_dir dir)
  >>= function
  | None -> Memo.Build.return ([], [])
  | Some dir ->
    Source_tree_map_reduce.map_reduce dir
      ~traverse:{ data_only = false; vendored = true; normal = true }
      ~f:(fun dir ->
        let dir =
          Path.Build.append_source (Super_context.context sctx).build_dir
            (Source_tree.Dir.path dir)
        in
        match Super_context.stanzas_in sctx ~dir with
        | None -> Memo.Build.return Libs_and_ppxs.empty
        | Some (d : _ Dir_with_dune.t) ->
          Memo.Build.List.fold_left d.data ~init:Libs_and_ppxs.empty
            ~f:(fun (acc, pps) -> function
            | Dune_file.Library l -> (
              let+ lib =
                let open Memo.Build.O in
                let+ resolve =
                  Lib.DB.resolve_when_exists db
                    (l.buildable.loc, Dune_file.Library.best_name l)
                in
                Option.map resolve ~f:Resolve.peek
              in
              match lib with
              | None
              | Some (Error ()) ->
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
                    ( Appendable_list.( @ ) (Appendable_list.singleton lib) acc
                    , Appendable_list.( @ )
                        (Appendable_list.singleton
                           (Lib_info.loc info, Lib_info.name info))
                        pps )
                  | Normal ->
                    ( Appendable_list.( @ ) (Appendable_list.singleton lib) acc
                    , pps )
                else
                  (acc, pps)
              (* external lib with a name matching our private name *))
            | _ -> Memo.Build.return (acc, pps)))
    >>| fun (libs, pps) ->
    (Appendable_list.to_list libs, Appendable_list.to_list pps)

let libs_under_dir sctx ~db ~dir = libs_and_ppx_under_dir sctx ~db ~dir >>| fst

let setup sctx ~dir =
  let open Memo.Build.O in
  let* expander = Super_context.expander sctx ~dir in
  let scope = Super_context.find_scope_by_dir sctx dir in
  let db = Scope.libs scope in
  let* libs, pps = libs_and_ppx_under_dir sctx ~db ~dir:(Path.build dir) in
  let pps =
    if List.is_empty pps then
      Preprocess.No_preprocessing
    else
      Preprocess.Pps { loc = Loc.none; pps; flags = []; staged = false }
  in
  let preprocess = Module_name.Per_item.for_all pps in
  let* preprocessing =
    Preprocessing.make sctx ~dir ~expander ~scope ~lib_name:None
      ~lint:Dune_file.Lint.no_lint ~preprocess ~preprocessor_deps:[]
      ~instrumentation_deps:[]
  in
  let source = source ~dir in
  let obj_dir = Toplevel.Source.obj_dir source in
  let loc = Toplevel.Source.loc source in
  let* modules = Toplevel.Source.modules source preprocessing in
  let* requires =
    let open Resolve.Build.O in
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
