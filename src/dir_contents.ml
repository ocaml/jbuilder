open! Stdune
open Import
module Menhir_rules = Menhir
open Dune_file
open! No_io

module Executables_modules = struct
  type t = Module.Name_map.t
end

module Modules = struct
  type t =
    { libraries : Lib_modules.t Lib_name.Map.t
    ; executables : Executables_modules.t String.Map.t
    ; (* Map from modules to the buildable they are part of *)
      rev_map : Buildable.t Module.Name.Map.t
    }

  let empty =
    { libraries = Lib_name.Map.empty
    ; executables = String.Map.empty
    ; rev_map = Module.Name.Map.empty
    }

  let make (d : _ Dir_with_dune.t) ~modules =
    let scope = d.scope in
    let libs, exes =
      List.filter_partition_map d.data ~f:(fun stanza ->
        match (stanza : Stanza.t) with
        | Library lib ->
          let obj_dir =
            Obj_dir.make_local ~dir:d.ctx_dir (snd lib.name)
              ~has_private_modules:(Option.is_some lib.private_modules)
          in
          let modules =
            Modules_field_evaluator.eval ~modules
              ~obj_dir
              ~buildable:lib.buildable
              ~virtual_modules:lib.virtual_modules
              ~private_modules:(
                Option.value ~default:Ordered_set_lang.standard
                  lib.private_modules)
          in
          let (main_module_name, wrapped) =
            (* the common case are normal libs where this is all specified so we
               special case it so that we don't have to resolve anything the db *)
            match Library.main_module_name lib, lib.wrapped with
            (* these values are always either inherited together or specified *)
            | This _, From _
            | From _, This _ -> assert false
            | This mmn, This wrapped -> mmn, wrapped
            | From _, From _ ->
              let name = (fst lib.name, Library.best_name lib) in
              Result.ok_exn (
                match
                  Lib.DB.find_even_when_hidden (Scope.libs scope) (snd name)
                with
                | None ->
                  (* can't happen because this library is defined using the
                     current stanza *)
                  assert false
                | Some lib ->
                  let open Result.O in
                  Lib.main_module_name lib >>= fun main_module_name ->
                  Lib.wrapped lib >>| fun wrapped ->
                  (main_module_name, Option.value_exn wrapped)
              )
          in
          Left ( lib
               , Lib_modules.make lib ~obj_dir modules ~main_module_name ~wrapped
               )
        | Executables exes
        | Tests { exes; _} ->
          let obj_dir =
            Obj_dir.make_exe ~dir:d.ctx_dir ~name:(snd (List.hd exes.names))
          in
          let modules =
            Modules_field_evaluator.eval ~modules
              ~obj_dir
              ~buildable:exes.buildable
              ~virtual_modules:None
              ~private_modules:Ordered_set_lang.standard
          in
          Right (exes, modules)
        | _ -> Skip)
    in
    let libraries =
      match
        Lib_name.Map.of_list_map libs ~f:(fun (lib, m) -> Library.best_name lib, m)
      with
      | Ok x -> x
      | Error (name, _, (lib2, _)) ->
        Errors.fail lib2.buildable.loc
          "Library %a appears for the second time \
           in this directory"
          Lib_name.pp_quoted name
    in
    let executables =
      match
        String.Map.of_list_map exes
          ~f:(fun (exes, m) -> snd (List.hd exes.names), m)
      with
      | Ok x -> x
      | Error (name, _, (exes2, _)) ->
        Errors.fail exes2.buildable.loc
          "Executable %S appears for the second time \
           in this directory"
          name
    in
    let rev_map =
      let rev_modules =
        List.rev_append
          (List.concat_map libs ~f:(fun (l, m) ->
             let modules = Lib_modules.modules m in
             List.map (Module.Name.Map.values modules) ~f:(fun m ->
               (Module.name m, l.buildable))))
          (List.concat_map exes ~f:(fun (e, m) ->
             List.map (Module.Name.Map.values m) ~f:(fun m ->
               (Module.name m, e.buildable))))
      in
      match d.kind with
      | Dune -> begin
          match Module.Name.Map.of_list rev_modules with
          | Ok x -> x
          | Error (name, _, _) ->
            let open Module.Name.Infix in
            let locs =
              List.filter_map rev_modules ~f:(fun (n, b) ->
                Option.some_if (n = name) b.loc)
              |> List.sort ~compare
            in
            Errors.fail (Loc.drop_position (List.hd locs))
              "Module %a is used in several stanzas:@\n\
               @[<v>%a@]@\n\
               @[%a@]"
              Module.Name.pp_quote name
              (Fmt.list (Fmt.prefix (Fmt.string "- ") Loc.pp_file_colon_line))
              locs
              Format.pp_print_text
              "To fix this error, you must specify an explicit \"modules\" \
               field in every library, executable, and executables stanzas in \
               this dune file. Note that each module cannot appear in more \
               than one \"modules\" field - it must belong to a single library \
               or executable."
        end
      | Jbuild ->
        Module.Name.Map.of_list_multi rev_modules
        |> Module.Name.Map.mapi ~f:(fun name buildables ->
          match buildables with
          | [] -> assert false
          | [b] -> b
          | b :: rest ->
            let locs =
              List.sort ~compare
                (b.Buildable.loc :: List.map rest ~f:(fun b -> b.Buildable.loc))
            in
            Errors.warn (Loc.drop_position b.loc)
              "Module %a is used in several stanzas:@\n\
               @[<v>%a@]@\n\
               @[%a@]@\n\
               This warning will become an error in the future."
              Module.Name.pp_quote name
              (Fmt.list (Fmt.prefix (Fmt.string "- ") Loc.pp_file_colon_line))
              locs
              Format.pp_print_text
              "To remove this warning, you must specify an explicit \"modules\" \
               field in every library, executable, and executables stanzas in \
               this jbuild file. Note that each module cannot appear in more \
               than one \"modules\" field - it must belong to a single library \
               or executable.";
            b)
    in
    { libraries; executables; rev_map }
end

type t =
  { kind : kind
  ; dir : Path.t
  ; text_files : String.Set.t
  ; modules : Modules.t Lazy.t
  ; c_sources : C_sources.t Lazy.t
  ; mlds : (Dune_file.Documentation.t * Path.t list) list Lazy.t
  }

and kind =
  | Standalone
  | Group_root of t list Lazy.t
  | Group_part of t

let kind t = t.kind
let dir t = t.dir

let dirs t =
  match t.kind with
  | Standalone -> [t]
  | Group_root (lazy l)
  | Group_part { kind = Group_root (lazy l); _ } -> t :: l
  | Group_part { kind = _; _ } -> assert false

let text_files t = t.text_files

let modules_of_library t ~name =
  let map = (Lazy.force t.modules).libraries in
  match Lib_name.Map.find map name with
  | Some m -> m
  | None ->
    Exn.code_error "Dir_contents.modules_of_library"
      [ "name", Lib_name.to_sexp name
      ; "available", Sexp.Encoder.(list Lib_name.to_sexp) (Lib_name.Map.keys map)
      ]

let modules_of_executables t ~first_exe =
  let map = (Lazy.force t.modules).executables in
  match String.Map.find map first_exe with
  | Some m -> m
  | None ->
    Exn.code_error "Dir_contents.modules_of_executables"
      [ "first_exe", Sexp.Encoder.string first_exe
      ; "available", Sexp.Encoder.(list string) (String.Map.keys map)
      ]

let c_sources_of_executables t ~first_exe =
  C_sources.for_exes (Lazy.force t.c_sources) ~first_exe

let c_sources_of_library t ~name =
  C_sources.for_lib (Lazy.force t.c_sources) ~dir:t.dir ~name

let lookup_module t name =
  Module.Name.Map.find (Lazy.force t.modules).rev_map name

let mlds t (doc : Documentation.t) =
  let map = Lazy.force t.mlds in
  match
    List.find_map map ~f:(fun (doc', x) ->
      Option.some_if (Loc.equal doc.loc doc'.loc) x)
  with
  | Some x -> x
  | None ->
    Exn.code_error "Dir_contents.mlds"
      [ "doc", Loc.to_sexp doc.loc
      ; "available", Sexp.Encoder.(list Loc.to_sexp)
                       (List.map map ~f:(fun (d, _) -> d.Documentation.loc))
      ]

(* As a side-effect, setup user rules and copy_files rules. *)
let load_text_files sctx ft_dir
      { Dir_with_dune.
        ctx_dir = dir
      ; src_dir
      ; scope = _
      ; data = stanzas
      ; kind = _
      ; dune_version = _
      } =
  (* Interpret a few stanzas in order to determine the list of
     files generated by the user. *)
  let expander = Super_context.expander sctx ~dir in
  let generated_files =
    List.concat_map stanzas ~f:(fun stanza ->
      match (stanza : Stanza.t) with
      | Menhir.T menhir ->
        Menhir_rules.targets menhir
      | Rule rule ->
        List.map (Simple_rules.user_rule sctx rule ~dir ~expander)
          ~f:Path.basename
      | Copy_files def ->
        List.map (Simple_rules.copy_files sctx def ~src_dir ~dir ~expander)
          ~f:Path.basename
      | Library { buildable; _ } | Executables { buildable; _ } ->
        (* Manually add files generated by the (select ...)
           dependencies *)
        List.filter_map buildable.libraries ~f:(fun dep ->
          match (dep : Dune_file.Lib_dep.t) with
          | Direct _ -> None
          | Select s -> Some s.result_fn)
      | _ -> [])
    |> String.Set.of_list
  in
  String.Set.union generated_files (File_tree.Dir.files ft_dir)

let modules_of_files ~dir ~files =
  let make_module syntax base fn =
    (Module.Name.of_string base,
     Module.File.make syntax (Path.relative dir fn))
  in
  let impl_files, intf_files =
    String.Set.to_list files
    |> List.filter_partition_map ~f:(fun fn ->
      (* we aren't using Filename.extension because we want to handle
         filenames such as foo.cppo.ml *)
      match String.lsplit2 fn ~on:'.' with
      | Some (s, "ml" ) -> Left  (make_module OCaml  s fn)
      | Some (s, "re" ) -> Left  (make_module Reason s fn)
      | Some (s, "mli") -> Right (make_module OCaml  s fn)
      | Some (s, "rei") -> Right (make_module Reason s fn)
      | _ -> Skip)
  in
  let parse_one_set (files : (Module.Name.t * Module.File.t) list)  =
    match Module.Name.Map.of_list files with
    | Ok x -> x
    | Error (name, f1, f2) ->
      let src_dir = Path.drop_build_context_exn dir in
      die "Too many files for module %a in %a:\
           \n- %a\
           \n- %a"
        Module.Name.pp name
        Path.pp src_dir
        Path.pp f1.path
        Path.pp f2.path
  in
  let impls = parse_one_set impl_files in
  let intfs = parse_one_set intf_files in
  Module.Name.Map.merge impls intfs ~f:(fun name impl intf ->
    Some (Module.Source.make name ?impl ?intf))


let build_mlds_map (d : _ Dir_with_dune.t) ~files =
  let dir = d.ctx_dir in
  let mlds = lazy (
    String.Set.fold files ~init:String.Map.empty ~f:(fun fn acc ->
      match String.lsplit2 fn ~on:'.' with
      | Some (s, "mld") -> String.Map.add acc s fn
      | _ -> acc))
  in
  List.filter_map d.data ~f:(function
    | Documentation doc ->
      let mlds =
        let mlds = Lazy.force mlds in
        Ordered_set_lang.String.eval_unordered doc.mld_files
          ~parse:(fun ~loc s ->
            match String.Map.find mlds s with
            | Some s ->
              s
            | None ->
              Errors.fail loc "%s.mld doesn't exist in %s" s
                (Path.to_string_maybe_quoted
                   (Path.drop_optional_build_context dir))
          )
          ~standard:mlds
      in
      Some (doc, List.map (String.Map.values mlds) ~f:(Path.relative dir))
    | _ -> None)

let cache = Hashtbl.create 32

let clear_cache () =
  Hashtbl.reset cache

let () = Hooks.End_of_build.always clear_cache

let rec get sctx ~dir =
  match Hashtbl.find cache dir with
  | Some t -> t
  | None ->
    let dir_status_db = Super_context.dir_status_db sctx in
    match Dir_status.DB.get dir_status_db ~dir with
    | Standalone x ->
      let t =
        match x with
        | Some (ft_dir, Some d) ->
          let files = load_text_files sctx ft_dir d in
          { kind = Standalone
          ; dir
          ; text_files = files
          ; modules = lazy (Modules.make d
                              ~modules:(modules_of_files ~dir:d.ctx_dir ~files))
          ; mlds = lazy (build_mlds_map d ~files)
          ; c_sources = lazy (
              let dune_version = d.dune_version in
              C_sources.make d
                ~c_sources:(C_sources.load_sources ~dune_version ~dir:d.ctx_dir
                              ~files))
          }
        | Some (_, None)
        | None ->
          { kind = Standalone
          ; dir
          ; text_files = String.Set.empty
          ; modules = lazy Modules.empty
          ; mlds = lazy []
          ; c_sources = lazy C_sources.empty
          }
      in
      Hashtbl.add cache dir t;
      t
    | Is_component_of_a_group_but_not_the_root _ -> begin
        match Hashtbl.find cache dir with
        | Some t -> t
        | None ->
          ignore (get sctx ~dir:(Path.parent_exn dir) : t);
          (* Filled while scanning the group root *)
          Hashtbl.find_exn cache dir
      end
    | Group_root (ft_dir, d) ->
      let rec walk ft_dir ~dir acc =
        match
          Dir_status.DB.get_assuming_parent_is_part_of_group dir_status_db
            ft_dir ~dir
        with
        | Is_component_of_a_group_but_not_the_root d ->
          let files =
            match d with
            | None -> File_tree.Dir.files ft_dir
            | Some d -> load_text_files sctx ft_dir d
          in
          walk_children ft_dir ~dir ((dir, files) :: acc)
        | _ -> acc
      and walk_children ft_dir ~dir acc =
        String.Map.foldi (File_tree.Dir.sub_dirs ft_dir) ~init:acc
          ~f:(fun name ft_dir acc ->
            let dir = Path.relative dir name in
            walk ft_dir ~dir acc)
      in
      let files = load_text_files sctx ft_dir d in
      let subdirs = walk_children ft_dir ~dir [] in
      let modules = lazy (
        let modules =
          List.fold_left ((dir, files) :: subdirs) ~init:Module.Name.Map.empty
            ~f:(fun acc (dir, files) ->
              let modules = modules_of_files ~dir ~files in
              Module.Name.Map.union acc modules ~f:(fun name x y ->
                Errors.fail (Loc.in_file
                               (match File_tree.Dir.dune_file ft_dir with
                                | None ->
                                  Path.relative (File_tree.Dir.path ft_dir)
                                    "_unknown_"
                                | Some d -> File_tree.Dune_file.path d))
                  "Module %a appears in several directories:\
                   @\n- %a\
                   @\n- %a"
                  Module.Name.pp_quote name
                  (Fmt.optional Path.pp) (Module.Source.src_dir x)
                  (Fmt.optional Path.pp) (Module.Source.src_dir y)))
        in
        Modules.make d ~modules)
      in
      let c_sources = lazy (
        let dune_version = d.dune_version in
        let init = C.Kind.Dict.make String.Map.empty in
        let c_sources =
          List.fold_left ((dir, files) :: subdirs) ~init
            ~f:(fun acc (dir, files) ->
              let sources = C_sources.load_sources ~dir ~dune_version ~files in
              let f acc sources =
                String.Map.union acc sources ~f:(fun name x y ->
                  Errors.fail (Loc.in_file
                                (match File_tree.Dir.dune_file ft_dir with
                                  | None ->
                                    Path.relative (File_tree.Dir.path ft_dir)
                                      "_unknown_"
                                  | Some d -> File_tree.Dune_file.path d))
                    "%a file %s appears in several directories:\
                    @\n- %a\
                    @\n- %a\
                    @\nThis is not allowed, please rename one of them."
                    (C.Kind.pp) (C.Source.kind x)
                    name
                    Path.pp_in_source (C.Source.src_dir x)
                    Path.pp_in_source (C.Source.src_dir y))
              in
              C.Kind.Dict.merge acc sources ~f)
        in
        C_sources.make d ~c_sources
      ) in
      let t =
        { kind = Group_root
                   (lazy (List.map subdirs ~f:(fun (dir, _) -> get sctx ~dir)))
        ; dir
        ; text_files = files
        ; modules
        ; c_sources
        ; mlds = lazy (build_mlds_map d ~files)
        }
      in
      Hashtbl.add cache dir t;
      List.iter subdirs ~f:(fun (dir, files) ->
        Hashtbl.add cache dir
          { kind = Group_part t
          ; dir
          ; text_files = files
          ; modules
          ; c_sources
          ; mlds = lazy (build_mlds_map d ~files)
          });
      t
