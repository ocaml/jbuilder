open! Stdune
open Import
module Menhir_rules = Menhir
open Dune_file
open! No_io

let loc_of_dune_file ft_dir =
  Loc.in_file
    (Path.source
       ( match File_tree.Dir.dune_file ft_dir with
       | Some d -> File_tree.Dune_file.path d
       | None -> Path.Source.relative (File_tree.Dir.path ft_dir) "_unknown_" ))

type t =
  { kind : kind
  ; dir : Path.Build.t
  ; text_files : String.Set.t
  ; foreign_sources : Foreign_sources.t Memo.Lazy.t
  ; mlds : (Dune_file.Documentation.t * Path.Build.t list) list Memo.Lazy.t
  ; coq : Coq_sources.t Memo.Lazy.t
  ; ml : Ml_sources.t Memo.Lazy.t
  }

and kind =
  | Standalone
  | Group_root of t list
  | Group_part

let empty kind ~dir =
  { kind
  ; dir
  ; text_files = String.Set.empty
  ; ml = Memo.Lazy.of_val Ml_sources.empty
  ; mlds = Memo.Lazy.of_val []
  ; foreign_sources = Memo.Lazy.of_val Foreign_sources.empty
  ; coq = Memo.Lazy.of_val Coq_sources.empty
  }

type gen_rules_result =
  | Standalone_or_root of t * t list
  | Group_part of Path.Build.t

let dir t = t.dir

let coq t = Memo.Lazy.force t.coq

let artifacts t = Memo.Lazy.force t.ml |> Ml_sources.artifacts

let dirs t =
  match t.kind with
  | Standalone -> [ t ]
  | Group_root subs -> t :: subs
  | Group_part ->
    Code_error.raise "Dir_contents.dirs called on a group part"
      [ ("dir", Path.Build.to_dyn t.dir) ]

let text_files t = t.text_files

let modules_of_library t ~name =
  Ml_sources.modules_of_library (Memo.Lazy.force t.ml) ~name

let modules_of_executables t ~obj_dir ~first_exe =
  Ml_sources.modules_of_executables (Memo.Lazy.force t.ml) ~obj_dir ~first_exe

let foreign_sources t = Memo.Lazy.force t.foreign_sources

let lookup_module t name = Ml_sources.lookup_module (Memo.Lazy.force t.ml) name

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

module rec Load : sig
  val get : Super_context.t -> dir:Path.Build.t -> t

  val gen_rules : Super_context.t -> dir:Path.Build.t -> gen_rules_result
end = struct
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
    let lookup ~f ~dir name = f (artifacts (Load.get sctx ~dir)) name in
    let lookup_module = lookup ~f:Ml_sources.Artifacts.lookup_module in
    let lookup_library = lookup ~f:Ml_sources.Artifacts.lookup_library in
    let expander = Super_context.expander sctx ~dir in
    let expander = Expander.set_lookup_module expander ~lookup_module in
    let expander = Expander.set_lookup_library expander ~lookup_library in
    let expander = Expander.set_artifacts_dynamic expander true in
    let generated_files =
      List.concat_map stanzas ~f:(fun stanza ->
          match (stanza : Stanza.t) with
          (* XXX What about mli files? *)
          | Coqpp.T { modules; _ } -> List.map modules ~f:(fun m -> m ^ ".ml")
          | Menhir.T menhir -> Menhir_rules.targets menhir
          | Rule rule ->
            Simple_rules.user_rule sctx rule ~dir ~expander
            |> Path.Build.Set.to_list
            |> List.map ~f:Path.Build.basename
          | Copy_files def ->
            Simple_rules.copy_files sctx def ~src_dir ~dir ~expander
            |> Path.Set.to_list |> List.map ~f:Path.basename
          | Library { buildable; _ }
          | Executables { buildable; _ } ->
            (* Manually add files generated by the (select ...) dependencies *)
            List.filter_map buildable.libraries ~f:(fun dep ->
                match (dep : Lib_dep.t) with
                | Re_export _
                | Direct _ ->
                  None
                | Select s -> Some s.result_fn)
          | _ -> [])
      |> String.Set.of_list
    in
    String.Set.union generated_files (File_tree.Dir.files ft_dir)

  type result0_here =
    { t : t
    ; (* [rules] includes rules for subdirectories too *)
      rules : Rules.t option
    ; (* The [kind] of the nodes must be Group_part *)
      subdirs : t Path.Build.Map.t
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

  let get0_impl (sctx, dir) : result0 =
    let dir_status_db = Super_context.dir_status_db sctx in
    let ctx = Super_context.context sctx in
    match Dir_status.DB.get dir_status_db ~dir with
    | Is_component_of_a_group_but_not_the_root { group_root; stanzas = _ } ->
      See_above group_root
    | Standalone x -> (
      match x with
      | Some (_, None)
      | None ->
        Here
          { t = empty Standalone ~dir
          ; rules = None
          ; subdirs = Path.Build.Map.empty
          }
      | Some (ft_dir, Some d) ->
        let loc = loc_of_dune_file ft_dir in
        let include_subdirs = Include_subdirs.No in
        let files, rules =
          Rules.collect_opt (fun () -> load_text_files sctx ft_dir d)
        in
        let ml =
          Memo.lazy_ (fun () ->
              let parent ~dir = Memo.Lazy.force (Load.get sctx ~dir).ml in
              Ml_sources.standalone d ~parent ~files)
        in
        Here
          { t =
              { kind = Standalone
              ; dir
              ; text_files = files
              ; ml
              ; mlds = Memo.lazy_ (fun () -> build_mlds_map d ~files)
              ; foreign_sources =
                  Memo.lazy_ (fun () ->
                      let lib_config = ctx.lib_config in
                      Foreign_sources.standalone d ~lib_config ~files)
              ; coq =
                  Memo.lazy_ (fun () ->
                      let subdirs = [ (dir, [], files) ] in
                      Coq_sources.of_dir d ~include_subdirs ~loc ~subdirs)
              }
          ; rules
          ; subdirs = Path.Build.Map.empty
          } )
    | Group_root (ft_dir, qualif_mode, d) ->
      let loc = loc_of_dune_file ft_dir in
      let include_subdirs = Dune_file.Include_subdirs.Include qualif_mode in
      (* XXX it's not clear what this [local] parameter is for *)
      let rec walk ft_dir ~dir ~local acc =
        match Dir_status.DB.get dir_status_db ~dir with
        | Is_component_of_a_group_but_not_the_root
            { stanzas = d; group_root = _ } ->
          let files =
            match d with
            | None -> File_tree.Dir.files ft_dir
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
      let (files, (subdirs : (Path.Build.t * _ * _) list)), rules =
        Rules.collect_opt (fun () ->
            let files = load_text_files sctx ft_dir d in
            let subdirs = walk_children ft_dir ~dir ~local:[] [] in
            (files, subdirs))
      in
      let ml =
        Memo.lazy_ (fun () ->
            let parent ~dir = Memo.Lazy.force (Load.get sctx ~dir).ml in
            let include_subdirs =
              Dune_file.Include_subdirs.Include qualif_mode
            in
            Ml_sources.group d ~loc ~parent ~include_subdirs ~dir ~files
              ~subdirs)
      in
      let foreign_sources =
        Memo.lazy_ (fun () ->
            let lib_config = ctx.lib_config in
            let subdirs = (dir, [], files) :: subdirs in
            Foreign_sources.group d ~loc ~include_subdirs ~lib_config ~subdirs)
      in
      let coq =
        Memo.lazy_ (fun () ->
            let subdirs = (dir, [], files) :: subdirs in
            Coq_sources.of_dir d ~subdirs ~loc ~include_subdirs)
      in
      let subdirs =
        List.map subdirs ~f:(fun (dir, _local, files) ->
            { kind = Group_part
            ; dir
            ; text_files = files
            ; ml
            ; foreign_sources
            ; mlds = Memo.lazy_ (fun () -> build_mlds_map d ~files)
            ; coq
            })
      in
      let t =
        { kind = Group_root subdirs
        ; dir
        ; text_files = files
        ; ml
        ; foreign_sources
        ; mlds = Memo.lazy_ (fun () -> build_mlds_map d ~files)
        ; coq
        }
      in
      Here
        { t
        ; rules
        ; subdirs =
            Path.Build.Map.of_list_map_exn subdirs ~f:(fun x -> (x.dir, x))
        }

  let memo0 =
    let module Output = struct
      type t = result0

      let to_dyn _ = Dyn.Opaque
    end in
    Memo.create "dir-contents-get0"
      ~input:(module Key)
      ~output:(Simple (module Output))
      ~doc:"dir contents" ~visibility:Hidden Sync get0_impl

  let get sctx ~dir =
    match Memo.exec memo0 (sctx, dir) with
    | Here { t; rules = _; subdirs = _ } -> t
    | See_above group_root -> (
      match Memo.exec memo0 (sctx, group_root) with
      | See_above _ -> assert false
      | Here { t; rules = _; subdirs = _ } -> t )

  let gen_rules sctx ~dir =
    match Memo.exec memo0 (sctx, dir) with
    | See_above group_root -> Group_part group_root
    | Here { t; rules; subdirs } ->
      Rules.produce_opt rules;
      Standalone_or_root (t, Path.Build.Map.values subdirs)
end

include Load
