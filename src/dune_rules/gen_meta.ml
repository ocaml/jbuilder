open! Dune_engine
open! Stdune
open Import
open Meta

module Pub_name = struct
  type t =
    | Dot of t * string
    | Id of string

  let of_list = function
    | [] -> assert false
    | x :: l ->
      let rec loop acc l =
        match l with
        | [] -> acc
        | x :: l -> loop (Dot (acc, x)) l
      in
      loop (Id x) l

  let of_lib_name s =
    let pkg, xs = Lib_name.split s in
    of_list (Package.Name.to_string pkg :: xs)

  let rec root = function
    | Dot (t, _) -> root t
    | Id n -> n

  let to_list =
    let rec loop acc = function
      | Dot (t, n) -> loop (n :: acc) t
      | Id n -> n :: acc
    in
    fun t -> loop [] t

  let to_string t = String.concat ~sep:"." (to_list t)
end

let string_of_deps deps =
  Lib_name.Set.to_string_list deps |> String.concat ~sep:" "

let rule var predicates action value = Rule { var; predicates; action; value }

let requires ?(preds = []) pkgs =
  rule "requires" preds Set (string_of_deps pkgs)

let ppx_runtime_deps ?(preds = []) pkgs =
  rule "ppx_runtime_deps" preds Set (string_of_deps pkgs)

let description s = rule "description" [] Set s

let directory s = rule "directory" [] Set s

let archive preds s = rule "archive" preds Set s

let plugin preds s = rule "plugin" preds Set s

let archives ?(preds = []) lib =
  let info = Lib.info lib in
  let archives = Lib_info.archives info in
  let plugins = Lib_info.plugins info in
  let make ps = String.concat ~sep:" " (List.map ps ~f:Path.basename) in
  [ archive (preds @ [ Pos "byte" ]) (make archives.byte)
  ; archive (preds @ [ Pos "native" ]) (make archives.native)
  ; plugin (preds @ [ Pos "byte" ]) (make plugins.byte)
  ; plugin (preds @ [ Pos "native" ]) (make plugins.native)
  ]

let gen_lib pub_name lib ~path ~version =
  let info = Lib.info lib in
  let synopsis = Lib_info.synopsis info in
  let kind = Lib_info.kind info in
  let desc =
    match synopsis with
    | Some s -> s
    | None -> (
      (* CR-someday jdimino: wut? this looks old *)
      match (pub_name : Pub_name.t) with
      | Dot (p, "runtime-lib") ->
        sprintf "Runtime library for %s" (Pub_name.to_string p)
      | Dot (p, "expander") -> sprintf "Expander for %s" (Pub_name.to_string p)
      | _ -> "")
  in
  let preds =
    match kind with
    | Normal -> []
    | Ppx_rewriter _
    | Ppx_deriver _ ->
      [ Pos "ppx_driver" ]
  in
  let lib_deps = Lib.Meta.requires lib in
  let ppx_rt_deps = Lib.Meta.ppx_runtime_deps lib in
  List.concat
    [ version
    ; [ description desc; requires ~preds lib_deps ]
    ; archives ~preds lib
    ; (if Lib_name.Set.is_empty ppx_rt_deps then
        []
      else
        [ Comment
            "This is what dune uses to find out the runtime dependencies of"
        ; Comment "a preprocessor"
        ; ppx_runtime_deps ppx_rt_deps
        ])
    ; (match kind with
      | Normal -> []
      | Ppx_rewriter _
      | Ppx_deriver _ ->
        (* Deprecated ppx method support *)
        let no_ppx_driver = Neg "ppx_driver"
        and no_custom_ppx = Neg "custom_ppx" in
        List.concat
          [ [ Comment
                "This line makes things transparent for people mixing \
                 preprocessors"
            ; Comment "and normal dependencies"
            ; requires ~preds:[ no_ppx_driver ]
                (Lib.Meta.ppx_runtime_deps_for_deprecated_method lib)
            ]
          ; (match kind with
            | Normal -> assert false
            | Ppx_rewriter _ ->
              [ rule "ppx"
                  [ no_ppx_driver; no_custom_ppx ]
                  Set "./ppx.exe --as-ppx"
              ; rule "library_kind" [] Set "ppx_rewriter"
              ]
            | Ppx_deriver _ ->
              [ rule "requires"
                  [ no_ppx_driver; no_custom_ppx ]
                  Add "ppx_deriving"
              ; rule "ppxopt"
                  [ no_ppx_driver; no_custom_ppx ]
                  Set
                  ("ppx_deriving,package:" ^ Pub_name.to_string pub_name)
              ; rule "library_kind" [] Set "ppx_deriver"
              ])
          ])
    ; (match Lib_info.jsoo_runtime info with
      | [] -> []
      | l ->
        let root = Pub_name.root pub_name in
        let l = List.map l ~f:Path.basename in
        [ rule "linkopts" [ Pos "javascript" ] Set
            (List.map l
               ~f:(sprintf "+%s/%s" (String.concat ~sep:"/" (root :: path)))
            |> String.concat ~sep:" ")
        ; rule "jsoo_runtime" [] Set (String.concat l ~sep:" ")
        ])
    ]

let gen ~(package : Package.t) ~add_directory_entry entries =
  let version =
    match package.version with
    | None -> []
    | Some s -> [ rule "version" [] Set s ]
  in
  let pkgs =
    List.map entries ~f:(fun (e : Super_context.Lib_entry.t) ->
        match e with
        | Library lib -> (
          let info = Lib.Local.info lib in
          let pub_name =
            let name = Lib_info.name info in
            Pub_name.of_lib_name name
          in
          match Pub_name.to_list pub_name with
          | [] -> assert false
          | package :: path ->
            let pub_name, path =
              match Lib_info.status info with
              | Private (_, None) ->
                (* Not possible b/c we wouldn't be generating a META file for a
                   private library without a package. *)
                assert false
              | Private (_, Some pkg) ->
                assert (path = []);
                let path =
                  Lib_name.Local.mangled_path_under_package
                    (Lib_name.Local.of_string package)
                in
                let pub_name =
                  let name = Package.name pkg in
                  Pub_name.of_list (Package.Name.to_string name :: path)
                in
                (pub_name, path)
              | _ -> (pub_name, path)
            in
            (pub_name, gen_lib pub_name ~path (Lib.Local.to_lib lib) ~version))
        | Deprecated_library_name
            { old_name = old_public_name, _
            ; new_public_name = _, new_public_name
            ; _
            } ->
          ( Pub_name.of_lib_name (Dune_file.Public_lib.name old_public_name)
          , version @ [ requires (Lib_name.Set.singleton new_public_name) ] ))
  in
  let pkgs =
    List.map pkgs ~f:(fun (pn, meta) ->
        match Pub_name.to_list pn with
        | [] -> assert false
        | _package :: path -> (path, meta))
  in
  let pkgs = List.sort pkgs ~compare:(fun (a, _) (b, _) -> Poly.compare a b) in
  let rec loop name pkgs =
    let entries, sub_pkgs =
      List.partition_map pkgs ~f:(function
        | [], entries -> Left entries
        | x :: p, entries -> Right (x, (p, entries)))
    in
    let entries = List.concat entries in
    let subs =
      String.Map.of_list_multi sub_pkgs
      |> String.Map.to_list
      |> List.map ~f:(fun (name, pkgs) ->
             let pkg = loop name pkgs in
             let pkg =
               if add_directory_entry then
                 { pkg with entries = directory name :: pkg.entries }
               else
                 pkg
             in
             Package pkg)
    in
    { name = Some (Lib_name.parse_string_exn (Loc.none, name))
    ; entries = entries @ subs
    }
  in
  loop (Package.Name.to_string (Package.name package)) pkgs
