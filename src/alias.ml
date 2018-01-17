open! Import

(** Fully qualified name *)
module Fq_name : sig
  type t
  val pp : Format.formatter -> t -> unit
  val make : Path.t -> t
  val path : t -> Path.t
end = struct
  type t = Path.t
  let make t = t
  let path t = t
  let pp = Path.pp
end

type t =
  { name : Fq_name.t
  ; file : Path.t
  }

let pp fmt t =
  Format.fprintf fmt "@[<2>{ name@ =@ %a@ ;@ file@ =@ %a }@]"
    Path.pp (Fq_name.path t.name) Path.pp t.file

let aliases_path = Path.(relative root) "_build/.aliases"

let suffix = "-" ^ String.make 32 '0'

let of_path path =
  if not (Path.is_local path) then
    die "Aliases are only supported for local paths!\n\
         Tried to reference alias %S"
      (Path.to_string path);
  { name = Fq_name.make path
  ; file = Path.extend_basename (Path.append aliases_path path) ~suffix
  }

let name t = Path.basename (Fq_name.path t.name)
let dir  t = Path.parent   (Fq_name.path t.name)

let fully_qualified_name t = Fq_name.path t.name

let make name ~dir =
  assert (not (String.contains name '/'));
  of_path (Path.relative dir name)

let dep t = Build.path t.file

let is_standard = function
  | "runtest" | "install" | "doc" | "lint" | "bench" -> true
  | _ -> false

let dep_rec ~loc ~file_tree t =
  let path = Path.parent   (Fq_name.path t.name) |> Path.drop_build_context in
  let name = Path.basename (Fq_name.path t.name) in
  match File_tree.find_dir file_tree path with
  | None -> Build.fail { fail = fun () ->
    Loc.fail loc "Don't know about directory %s!" (Path.to_string_maybe_quoted path) }
  | Some dir ->
    let open Build.O in
    File_tree.Dir.fold dir ~traverse_ignored_dirs:false ~init:(Build.return true)
      ~f:(fun dir acc ->
        let path = File_tree.Dir.path dir in
        let t = of_path (Path.relative path name) in
        acc
        >>>
        Build.if_file_exists t.file
          ~then_:(Build.path t.file
                  >>^
                  fun _ -> false)
          ~else_:(Build.arr (fun x -> x)))
    >>^ fun is_empty ->
    if is_empty && not (is_standard name) then
      Loc.fail loc "This alias is empty.\n\
                    Alias %S is not defined in %s or any of its descendants."
        name (Path.to_string_maybe_quoted path)

let file t = t.file

let file_with_digest_suffix t ~digest =
  let dir = Path.parent t.file in
  let base = Path.basename t.file in
  let len = String.length base in
  Path.relative dir
    (String.sub base ~pos:0 ~len:(len - 32) ^ Digest.to_hex digest)

let of_file fn =
  match Path.extract_build_context fn with
  | Some (".aliases", fn) -> begin
      let dir  = Path.parent   fn in
      let name = Path.basename fn in
      match String.rsplit2 name ~on:'-' with
      | None -> assert false
      | Some (name, digest) ->
        assert (String.length digest = 32);
        Some (make name ~dir)
    end
  | _ -> None

let name_of_file fn =
  match Path.extract_build_context fn with
  | Some (".aliases", fn) -> begin
      let name = Path.basename fn in
      match String.rsplit2 name ~on:'-' with
      | None -> assert false
      | Some (name, digest) ->
        assert (String.length digest = 32);
        Some name
    end
  | _ -> None

let default = make "DEFAULT"
let runtest = make "runtest"
let install = make "install"
let doc     = make "doc"
let lint    = make "lint"

module Store = struct
  type entry =
    { alias : t
    ; mutable deps : Path.Set.t
    }
  let pp_entry fmt entry =
    let pp_deps fmt deps =
      Format.pp_print_list Path.pp fmt (Path.Set.elements deps) in
    Format.fprintf fmt "@[<2>{@ alias@ =@ %a@ ;@ deps@ = (%a)@ }@]"
      pp entry.alias pp_deps entry.deps

  type t = (Fq_name.t, entry) Hashtbl.t

  let pp fmt (t : t) =
    let bindings = Hashtbl.fold ~init:[] ~f:(fun ~key ~data acc ->
      (key, data)::acc
    ) t in
    let pp_bindings fmt b =
      Format.pp_print_list (fun fmt (k, v) ->
        Format.fprintf fmt "@[<2>(%a@ %a)@]" Fq_name.pp k pp_entry v
      ) fmt b in
    Format.fprintf fmt "Store.t@ @[@<2>(%a)@]" pp_bindings bindings

  let create () = Hashtbl.create 1024

  let unlink (store : t) = function
    | [] -> ()
    | alias_basenames ->
      store
      |> Hashtbl.fold ~init:Path.Set.empty ~f:(fun ~key:_ ~data:entry acc ->
        if List.mem (name entry.alias) ~set:alias_basenames then (
          Path.Set.union acc (Path.Set.add entry.alias.file entry.deps)
        ) else (
          acc
        ))
      |> Path.Set.iter ~f:Path.unlink_no_err
end

let add_deps store t deps =
  let deps = Path.Set.of_list deps in
  match Hashtbl.find store t.name with
  | None ->
    Hashtbl.add store ~key:t.name
      ~data:{ Store.alias = t
            ; deps = deps
            }
  | Some e -> e.deps <- Path.Set.union deps e.deps

let rules store =
  (* For each alias @_build/blah/../x, add a dependency: @../x --> @_build/blah/../x *)
  Hashtbl.fold store ~init:[] ~f:(fun ~key:_ ~data:{ Store. alias; _ } acc ->
    match Path.extract_build_context (Fq_name.path alias.name) with
    | None -> acc
    | Some (_, in_src) -> (of_path in_src, alias) :: acc)
  |> List.iter ~f:(fun (in_src, in_build_dir) ->
      add_deps store in_src [in_build_dir.file]);

  Hashtbl.fold store ~init:[] ~f:(fun ~key:_ ~data:{ Store. alias; deps } acc ->
    let open Build.O in
    let rule =
      Build_interpret.Rule.make
        (Build.path_set deps >>>
         Build.action ~targets:[alias.file]
           (Redirect (Stdout,
                      alias.file,
                      Digest_files
                        (Path.Set.elements deps))))
    in
    rule :: acc)

let add_build store t ~stamp build =
  let digest = Digest.string (Sexp.to_string stamp) in
  let digest_path = file_with_digest_suffix t ~digest in
  add_deps store t [digest_path];
  Build.progn
    [ build
    ; Build.create_file digest_path
    ]

let add_builds store t builds =
  let digest_files, actions =
    List.split
      (List.map builds ~f:(fun (stamp, build) ->
         let digest = Digest.string (Sexp.to_string stamp) in
         let digest_path = file_with_digest_suffix t ~digest in
         (digest_path,
          Build.progn
            [ build
            ; Build.create_file digest_path
            ])))
  in
  add_deps store t digest_files;
  actions
