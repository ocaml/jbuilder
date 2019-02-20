open! Stdune
open Import

module Opam_package = Package

module P  = Variant
module Ps = Variant.Set

(* An assignment or addition *)
module Rule = struct
  type t =
    { preds_required  : Ps.t
    ; preds_forbidden : Ps.t
    ; value           : string
    }

  let pp fmt { preds_required; preds_forbidden; value } =
    Fmt.record fmt
      [ "preds_required", Fmt.const Ps.pp preds_required
      ; "preds_forbidden", Fmt.const Ps.pp preds_forbidden
      ; "value", Fmt.const (fun fmt -> Format.fprintf fmt "%S") value
      ]


  let formal_predicates_count t =
    Ps.cardinal t.preds_required + Ps.cardinal t.preds_forbidden

  let matches t ~preds =
    Ps.is_subset t.preds_required ~of_:preds &&
    Ps.is_empty (Ps.inter preds t.preds_forbidden)

  let make (rule : Meta.rule) =
    let preds_required, preds_forbidden =
      List.partition_map rule.predicates ~f:(function
        | Pos x -> Left  x
        | Neg x -> Right x)
    in
    { preds_required  = Ps.make preds_required
    ; preds_forbidden = Ps.make preds_forbidden
    ; value           = rule.value
    }
end

(* Set of rules for a given variable of a package. Implements the
   algorithm described here:

   http://projects.camlcity.org/projects/dl/findlib-1.6.3/doc/ref-html/r729.html
*)
module Rules = struct
  (* To implement the algorithm, [set_rules] is sorted by decreasing
     number of formal predicates, then according to the order of the
     META file. [add_rules] are in the same order as in the META
     file. *)
  type t =
    { set_rules : Rule.t list
    ; add_rules : Rule.t list
    }

  let pp fmt { set_rules; add_rules } =
    Fmt.record fmt
      [ "set_rules", (fun fmt () -> Fmt.ocaml_list Rule.pp fmt set_rules)
      ; "add_rules", (fun fmt () -> Fmt.ocaml_list Rule.pp fmt add_rules)
      ]

  let interpret t ~preds =
    let rec find_set_rule = function
      | [] -> None
      | rule :: rules ->
        if Rule.matches rule ~preds then
          Some rule.value
        else
          find_set_rule rules
    in
    let v = find_set_rule t.set_rules in
    List.fold_left t.add_rules ~init:v ~f:(fun v rule ->
      if Rule.matches rule ~preds then
        Some ((Option.value ~default:"" v) ^ " " ^ rule.value)
      else
        v)

  let of_meta_rules (rules : Meta.Simplified.Rules.t) =
    let add_rules = List.map rules.add_rules ~f:Rule.make in
    let set_rules =
      List.map rules.set_rules ~f:Rule.make
      |> List.stable_sort ~compare:(fun a b ->
        compare
          (Rule.formal_predicates_count b)
          (Rule.formal_predicates_count a))
    in
    { add_rules; set_rules }
end

module Vars = struct
  type t = Rules.t String.Map.t

  let get (t : t) var preds =
    Option.map (String.Map.find t var) ~f:(fun r ->
      Option.value ~default:"" (Rules.interpret r ~preds))

  let get_words t var preds =
    match get t var preds with
    | None -> []
    | Some s -> String.extract_comma_space_separated_words s

  let empty = String.Map.empty
  let superpose = String.Map.superpose
end

module Config = struct
  type t =
    { vars  : Vars.t
    ; preds : Ps.t
    }

  let pp fmt { vars; preds } =
    Fmt.record fmt
      [ "vars"
      , Fmt.const (Fmt.ocaml_list (Fmt.tuple Format.pp_print_string Rules.pp))
          (String.Map.to_list vars)
      ; "preds"
      , Fmt.const Ps.pp preds
      ]

  let load config_file =
    let load p =
      (Meta.load ~name:None p).vars
      |> String.Map.map ~f:Rules.of_meta_rules
    in
    let vars = Vars.empty in
    let vars =
      if Path.exists config_file then
        load config_file
      else
        vars
    in
    let config_dir = Path.extend_basename config_file ~suffix:".d" in
    let vars =
      if Path.is_directory config_dir then
        Path.readdir_unsorted config_dir
        |> List.fold_left ~init:vars ~f:(fun acc p ->
          let p = Path.relative config_dir p in
          Vars.superpose acc (load p))
      else
        vars
    in
    { vars
    ; preds = Ps.empty
    }

  let get { vars; preds } var =
    Vars.get vars var preds

  let toolchain t ~toolchain =
    { t with preds = Ps.singleton (P.make toolchain) }

  let env t =
    let preds = Ps.add t.preds (P.make "env") in
    String.Map.filter_map ~f:(Rules.interpret ~preds) t.vars
    |> Env.of_string_map
end

module Package = struct
  type t =
    { meta_file : Path.t
    ; name      : Lib_name.t
    ; dir       : Path.t
    ; vars      : Vars.t
    }

  let loc  t = Loc.in_dir t.meta_file
  let name t = t.name

  let preds = Ps.of_list [P.ppx_driver; P.mt; P.mt_posix]

  let get_paths t var preds =
    List.map (Vars.get_words t.vars var preds) ~f:(Path.relative t.dir)

  let make_archives t var preds =
    Mode.Dict.of_func (fun ~mode ->
      get_paths t var (Ps.add preds (Mode.variant mode)))

  let version          t = Vars.get       t.vars "version"          Ps.empty
  let description      t = Vars.get       t.vars "description"      Ps.empty
  let jsoo_runtime     t = get_paths      t      "jsoo_runtime"     Ps.empty
  let requires         t =
    Vars.get_words t.vars "requires"         preds
    |> List.map ~f:(Lib_name.of_string_exn ~loc:None)
  let ppx_runtime_deps t =
    Vars.get_words t.vars "ppx_runtime_deps" preds
    |> List.map ~f:(Lib_name.of_string_exn ~loc:None)

  let archives t = make_archives t "archive" preds
  let plugins t =
    Mode.Dict.map2 ~f:(@)
      (make_archives t "archive" (Ps.add preds Variant.plugin))
      (make_archives t "plugin" preds)

  let dune_file t =
    let fn = Path.relative t.dir
               (sprintf "%s.dune" (Lib_name.to_string t.name)) in
    Option.some_if (Path.exists fn) fn

  let to_dune t =
    let loc = loc t in
    let add_loc x = (loc, x) in
    let sub_systems =
      match dune_file t with
      | None -> Sub_system_name.Map.empty
      | Some p -> Installed_dune_file.load p
    in
    let archives = archives t in
    let modes : Mode.Dict.Set.t =
      Mode.Dict.map ~f:(fun x -> not (List.is_empty x)) archives in
    Dune_package.Lib.make
      ~orig_src_dir:None
      ~loc
      ~kind:Normal
      ~name:(name t)
      ~synopsis:(description t)
      ~archives
      ~plugins:(plugins t)
      ~foreign_objects:[]
      ~foreign_archives:(Mode.Dict.make_both [])
      ~jsoo_runtime:(jsoo_runtime t)
      ~sub_systems
      ~requires:(List.map ~f:add_loc (requires t))
      ~ppx_runtime_deps:(List.map ~f:add_loc (ppx_runtime_deps t))
      ~virtual_:false
      ~implements:None
      ~modules:None
      ~main_module_name:None (* XXX remove *)
      ~version:(version t)
      ~modes
      ~dir:t.dir
end

module Unavailable_reason = struct
  type t =
    | Not_found
    | Hidden of Sub_system_info.t Dune_package.Lib.t

  let to_string = function
    | Not_found  -> "not found"
    | Hidden pkg ->
      sprintf "in %s is hidden (unsatisfied 'exist_if')"
        (Path.to_string_maybe_quoted (Dune_package.Lib.dir pkg))

  let pp ppf t = Format.pp_print_string ppf (to_string t)
end

type t =
  { stdlib_dir : Path.t
  ; paths      : Path.t list
  ; builtins   : Meta.Simplified.t Lib_name.Map.t
  ; packages   : ( Lib_name.t
                 , ( Sub_system_info.t Dune_package.Lib.t
                   , Unavailable_reason.t) result
                 ) Hashtbl.t
  }

let paths t = t.paths

let dummy_package t ~name =
  let dir =
    match t.paths with
    | [] -> t.stdlib_dir
    | dir :: _ ->
      Lib_name.package_name name
      |> Opam_package.Name.to_string
      |> Path.relative dir
  in
  { Package.
    meta_file = Path.relative dir "META"
  ; name      = name
  ; dir       = dir
  ; vars      = String.Map.empty
  }
  |> Package.to_dune

(* Parse a single package from a META file *)
let parse_package t ~meta_file ~name ~parent_dir ~vars =
  let pkg_dir = Vars.get vars "directory" Ps.empty in
  let dir =
    match pkg_dir with
    | None | Some "" -> parent_dir
    | Some pkg_dir ->
      if pkg_dir.[0] = '+' || pkg_dir.[0] = '^' then
        Path.relative t.stdlib_dir (String.drop pkg_dir 1)
      else if Filename.is_relative pkg_dir then
        Path.relative parent_dir pkg_dir
      else
        Path.of_filename_relative_to_initial_cwd pkg_dir
  in
  let pkg =
    { Package.
      meta_file
    ; name
    ; dir
    ; vars
    }
  in
  let exists_if = Vars.get_words vars "exists_if" Ps.empty in
  let exists =
    match exists_if with
    | _ :: _ ->
      List.for_all exists_if ~f:(fun fn ->
        Path.exists (Path.relative dir fn))
    | [] ->
      if not (Lib_name.Map.mem t.builtins (Lib_name.root_lib name)) then
        true
      else
        (* The META files for installed packages are sometimes broken,
           i.e. META files for libraries that were not installed by
           the compiler are still present:

           https://github.com/ocaml/dune/issues/563

           To workaround this problem, for builtin packages we check
           that at least one of the archive is present. *)
        match Package.archives pkg with
        | { byte = []; native = [] } -> true
        | { byte; native } -> List.exists (byte @ native) ~f:Path.exists
  in
  let res =
    let pkg = Package.to_dune pkg in
    if exists then
      Ok pkg
    else
      Error (Unavailable_reason.Hidden pkg)
  in
  (dir, res)

(* Parse all the packages defined in a META file and add them to
   [t.packages] *)
let parse_and_acknowledge_meta t ~dir ~meta_file (meta : Meta.Simplified.t) =
  let rec loop ~dir ~full_name (meta : Meta.Simplified.t) =
    let vars = String.Map.map meta.vars ~f:Rules.of_meta_rules in
    let dir, res =
      parse_package t ~meta_file ~name:full_name ~parent_dir:dir ~vars
    in
    Hashtbl.add t.packages full_name res;
    List.iter meta.subs ~f:(fun (meta : Meta.Simplified.t) ->
      let full_name =
        match meta.name with
        | None -> full_name
        | Some name -> Lib_name.nest full_name name in
      loop ~dir ~full_name meta)
  in
  loop ~dir ~full_name:(Option.value_exn meta.name) meta

type findlib =
  { dir       : Path.t
  ; meta_file : Path.t
  ; meta      : Meta.Simplified.t
  }

type pkg =
  | Dune of Sub_system_info.t Dune_package.t
  | Findlib of findlib

(* Search for a <package>/META file in the findlib search path, parse
   it and add its contents to [t.packages] *)
let find_and_acknowledge_meta t ~fq_name =
  let root_name = Lib_name.root_lib fq_name in
  let rec loop dirs : pkg option =
    match dirs with
    | [] ->
      Lib_name.Map.find t.builtins root_name
      |> Option.map ~f:(fun meta ->
        Findlib
          { dir = t.stdlib_dir
          ; meta_file = Path.of_string "<internal>"
          ; meta
          })
    | dir :: dirs ->
      let sub_dir = Path.relative dir (Lib_name.to_string root_name) in
      let dune = Path.relative sub_dir "dune-package" in
      let meta_file = Path.relative sub_dir "META" in
      match
        (if Path.exists dune then
           Dune_package.Or_meta.load dune
         else
           Dune_package.Or_meta.Use_meta)
      with
      | Dune_package p -> Some (Dune p)
      | Use_meta ->
        if Path.exists meta_file then
          let meta = Meta.load meta_file ~name:(Some root_name) in
          Some (Findlib {dir = sub_dir; meta; meta_file})
        else
          (* Alternative layout *)
          let meta_file =
            Path.relative dir ("META." ^ (Lib_name.to_string root_name)) in
          if Path.exists meta_file then
            let meta = Meta.load meta_file ~name:(Some root_name) in
            Some (Findlib {dir; meta_file; meta})
          else
            loop dirs
  in
  match loop t.paths with
  | None ->
    Hashtbl.add t.packages root_name (Error Not_found)
  | Some (Findlib { meta ; meta_file ; dir }) ->
    parse_and_acknowledge_meta t meta ~meta_file ~dir
  | Some (Dune pkg) ->
    List.iter pkg.libs ~f:(fun lib ->
      Hashtbl.add t.packages (Dune_package.Lib.name lib) (Ok lib))

let find t name =
  match Hashtbl.find t.packages name with
  | Some x -> x
  | None ->
    find_and_acknowledge_meta t ~fq_name:name;
    match Hashtbl.find t.packages name with
    | Some x -> x
    | None ->
      let res = Error Unavailable_reason.Not_found in
      Hashtbl.add t.packages name res;
      res

let available t name = Result.is_ok (find t name)

let root_packages t =
  let pkgs =
    List.concat_map t.paths ~f:(fun dir ->
      Sys.readdir (Path.to_string dir)
      |> Array.to_list
      |> List.filter_map ~f:(fun name ->
        if Path.exists (Path.relative dir (name ^ "/META")) then
          Some (Lib_name.of_string_exn ~loc:None name)
        else
          None))
    |> Lib_name.Set.of_list
  in
  Lib_name.Set.union pkgs
    (Lib_name.Set.of_list (Lib_name.Map.keys t.builtins))

let load_all_packages t =
  Lib_name.Set.iter (root_packages t) ~f:(fun pkg ->
    find_and_acknowledge_meta t ~fq_name:pkg)

let all_packages t =
  load_all_packages t;
  Hashtbl.fold t.packages ~init:[] ~f:(fun x acc ->
    match x with
    | Ok p    -> p :: acc
    | Error _ -> acc)
  |> List.sort ~compare:Dune_package.Lib.compare_name

let create ~stdlib_dir ~paths ~version =
  { stdlib_dir
  ; paths
  ; builtins = Meta.builtins ~stdlib_dir ~version
  ; packages = Hashtbl.create 1024
  }

let all_unavailable_packages t =
  load_all_packages t;
  Hashtbl.foldi t.packages ~init:[] ~f:(fun name x acc ->
    match x with
    | Ok    _ -> acc
    | Error e -> ((name, e) :: acc))
  |> List.sort ~compare:(fun (a, _) (b, _) -> Lib_name.compare a b)
