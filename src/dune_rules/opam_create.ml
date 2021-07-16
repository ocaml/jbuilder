open! Dune_engine
open Stdune

let default_build_command =
  let before_1_11 =
    lazy
      (Opam_file.parse_value
         (Lexbuf.from_string ~fname:"<internal>"
            {|
[
  [ "dune" "subst" ] {pinned}
  [ "dune" "build" "-p" name "-j" jobs]
  [ "dune" "runtest" "-p" name "-j" jobs] {with-test}
  [ "dune" "build" "-p" name "@doc"] {with-doc}
]
|}))
  and from_1_11_before_2_7 =
    lazy
      (Opam_file.parse_value
         (Lexbuf.from_string ~fname:"<internal>"
            {|
[
  [ "dune" "subst" ] {pinned}
  [ "dune" "build" "-p" name "-j" jobs
      "@install"
      "@runtest" {with-test}
      "@doc" {with-doc}
  ]
]
|}))
  and from_2_7 =
    lazy
      (Opam_file.parse_value
         (Lexbuf.from_string ~fname:"<internal>"
            {|
[
  [ "dune" "subst" ] {dev}
  [ "dune" "build" "-p" name "-j" jobs
      "@install"
      "@runtest" {with-test}
      "@doc" {with-doc}
  ]
]
|}))
  and from_2_9 =
    lazy
      (Opam_file.parse_value
         (Lexbuf.from_string ~fname:"<internal>"
            {|
[
  [ "dune" "subst" ] {dev}
  [ "dune" "build" "-p" name "-j" jobs "--promote-install-files" "false"
      "@install"
      "@runtest" {with-test}
      "@doc" {with-doc}
  ]
  [ "dune" "install" "-p" name "--create-install-files" name ]
]
|}))
  in
  fun project ->
    Lazy.force
      (if Dune_project.dune_version project < (1, 11) then
        before_1_11
      else if Dune_project.dune_version project < (2, 7) then
        from_1_11_before_2_7
      else if Dune_project.dune_version project < (2, 9) then
        from_2_7
      else
        from_2_9)

let package_fields
    { Package.synopsis
    ; description
    ; depends
    ; conflicts
    ; depopts
    ; info = _
    ; id = _
    ; version = _
    ; has_opam_file = _
    ; tags
    ; loc = _
    ; deprecated_package_names = _
    ; sites = _
    } ~project =
  let open Opam_file.Create in
  let tags =
    if tags = [] then
      []
    else
      [ ("tags", string_list tags) ]
  in
  let optional =
    [ ("synopsis", synopsis); ("description", description) ]
    |> List.filter_map ~f:(fun (k, v) ->
           match v with
           | None -> None
           | Some v -> Some (k, string v))
  in
  let dep_fields =
    [ ("depends", depends); ("conflicts", conflicts); ("depopts", depopts) ]
    |> List.filter_map ~f:(fun (k, v) ->
           match v with
           | [] -> None
           | _ :: _ -> Some (k, list Package.Dependency.opam_depend v))
  in
  let fields = [ optional; dep_fields ] in
  let fields =
    let dune_version = Dune_project.dune_version project in
    if dune_version >= (2, 0) && tags <> [] then
      tags :: fields
    else
      fields
  in
  List.concat fields

let dune_name = Package.Name.of_string "dune"

let odoc_name = Package.Name.of_string "odoc"

let insert_dune_dep depends dune_version =
  let constraint_ : Package.Dependency.Constraint.t =
    let dune_version = Dune_lang.Syntax.Version.to_string dune_version in
    Uop (Gte, QVar dune_version)
  in
  let rec loop acc = function
    | [] ->
      let dune_dep =
        { Package.Dependency.name = dune_name; constraint_ = Some constraint_ }
      in
      dune_dep :: List.rev acc
    | (dep : Package.Dependency.t) :: rest ->
      if Package.Name.equal dep.name dune_name then
        let dep =
          if dune_version < (2, 6) then
            dep
          else
            { dep with
              constraint_ =
                Some
                  (match dep.constraint_ with
                  | None -> constraint_
                  | Some c -> And [ constraint_; c ])
            }
        in
        List.rev_append acc (dep :: rest)
      else
        loop (dep :: acc) rest
  in
  loop [] depends

let rec already_requires_odoc : Package.Dependency.Constraint.t -> bool =
  function
  | Bvar (Var "with-doc" | Var "build" | Var "post")
  | Uop _
  | Bop _ ->
    true
  | Bvar _ -> false
  | And l -> List.for_all ~f:already_requires_odoc l
  | Or l -> List.exists ~f:already_requires_odoc l

let insert_odoc_dep depends =
  let with_doc : Package.Dependency.Constraint.t = Bvar (Var "with-doc") in
  let odoc_dep =
    { Package.Dependency.name = odoc_name; constraint_ = Some with_doc }
  in
  let rec loop acc = function
    | [] -> List.rev (odoc_dep :: acc)
    | (dep : Package.Dependency.t) :: rest ->
      if
        Package.Name.equal dep.name odoc_name
        && Option.forall ~f:already_requires_odoc dep.constraint_
      then
        (* Stop now as odoc will be required anyway *)
        List.rev_append (dep :: acc) rest
      else
        loop (dep :: acc) rest
  in
  loop [] depends

let opam_fields project (package : Package.t) =
  let dune_version = Dune_project.dune_version project in
  let package_name = Package.name package in
  let package =
    if dune_version < (1, 11) || Package.Name.equal package_name dune_name then
      package
    else
      { package with depends = insert_dune_dep package.depends dune_version }
  in
  let package =
    if dune_version < (2, 7) || Package.Name.equal package_name odoc_name then
      package
    else
      { package with depends = insert_odoc_dep package.depends }
  in
  let package_fields = package_fields package ~project in
  let open Opam_file.Create in
  let info = package.Package.info in
  let optional_fields =
    [ ("bug-reports", Package.Info.bug_reports info)
    ; ("homepage", Package.Info.homepage info)
    ; ("doc", Package.Info.documentation info)
    ; ("license", Package.Info.license info)
    ; ("version", package.Package.version)
    ; ( "dev-repo"
      , Option.map ~f:Package.Source_kind.to_string (Package.Info.source info)
      )
    ]
    |> List.filter_map ~f:(fun (k, v) ->
           Option.map v ~f:(fun v -> (k, string v)))
  in
  let list_fields =
    [ ("maintainer", Package.Info.maintainers info)
    ; ("authors", Package.Info.authors info)
    ]
    |> List.filter_map ~f:(fun (k, v) ->
           match v with
           | None
           | Some [] ->
             None
           | Some (_ :: _ as v) -> Some (k, string_list v))
  in
  let fields =
    [ ("opam-version", string "2.0"); ("build", default_build_command project) ]
  in
  let fields =
    List.concat [ fields; list_fields; optional_fields; package_fields ]
  in
  if Dune_project.dune_version project < (1, 11) then
    fields
  else
    Opam_file.Create.normalise_field_order fields

let template_file = Path.extend_basename ~suffix:".template"

let opam_template ~opam_path =
  let open Action_builder.O in
  let opam_template_path = template_file opam_path in
  Action_builder.if_file_exists opam_template_path
    ~then_:
      (let+ contents = Action_builder.contents opam_template_path in
       Some (opam_template_path, contents))
    ~else_:(Action_builder.return None)

let generate project pkg ~template =
  let opam_fname = Package.opam_file pkg in
  let filter_fields =
    match template with
    | None -> Fun.id
    | Some (fname, contents) ->
      let vars_in_template =
        Lexbuf.from_string ~fname:(Path.to_string fname) contents
        |> Opam_file.parse |> Opam_file.existing_variables
      in
      List.filter ~f:(fun (v, _) -> not (String.Set.mem vars_in_template v))
  in
  let generated_fields =
    opam_fields project pkg |> filter_fields
    |> Opam_file.Create.of_bindings ~file:(Path.source opam_fname)
  in
  sprintf "# This file is generated by dune, edit dune-project instead\n%s\n%s"
    (OpamPrinter.opamfile generated_fields)
    (match template with
    | None -> ""
    | Some (_, s) -> s)

let add_rule sctx ~project ~pkg =
  let open Action_builder.O in
  let build_dir = (Super_context.context sctx).build_dir in
  let opam_path = Path.Build.append_source build_dir (Package.opam_file pkg) in
  let opam_rule =
    (let+ template = opam_template ~opam_path:(Path.build opam_path) in
     generate project pkg ~template)
    |> Action_builder.write_file_dyn opam_path
  in
  let dir = Path.Build.append_source build_dir (Package.dir pkg) in
  let mode =
    Rule.Mode.Promote { lifetime = Unlimited; into = None; only = None }
  in
  let open Memo.Build.O in
  let* () = Super_context.add_rule sctx ~mode ~dir opam_rule in
  let aliases =
    [ Alias.install ~dir
    ; Alias.runtest ~dir
    ; Alias.check ~dir (* check doesn't pick up the promote target? *)
    ]
  in
  let deps = Path.Set.singleton (Path.build opam_path) in
  Memo.Build.sequential_iter aliases ~f:(fun alias ->
      Rules.Produce.Alias.add_deps alias (Action_builder.path_set deps))

let add_rules sctx ~dir =
  let project = Super_context.find_scope_by_dir sctx dir |> Scope.project in
  Memo.Build.when_ (Dune_project.generate_opam_files project) (fun () ->
      let packages = Dune_project.packages project in
      Package.Name.Map_traversals.parallel_iter packages
        ~f:(fun _name (pkg : Package.t) -> add_rule sctx ~project ~pkg))
