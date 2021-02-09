open! Import

let opam_ext = ".opam"

let is_opam_file path = String.is_suffix (Path.to_string path) ~suffix:opam_ext

module Name = struct
  module T =
    Interned.Make
      (struct
        let initial_size = 16

        let resize_policy = Interned.Conservative

        let order = Interned.Natural
      end)
      ()

  include T

  include (
    Stringlike.Make (struct
      type t = T.t

      let to_string = T.to_string

      let module_ = "Package.Name"

      let description = "package name"

      let description_of_valid_string = None

      let hint_valid = None

      let of_string_opt s =
        (* DUNE3 verify no dots or spaces *)
        if s = "" then
          None
        else
          Some (make s)
    end) :
      Stringlike_intf.S with type t := t)

  let of_opam_file_basename basename =
    let open Option.O in
    let* name = String.drop_suffix basename ~suffix:opam_ext in
    of_string_opt name

  let opam_fn (t : t) = to_string t ^ opam_ext

  let meta_fn (t : t) = "META." ^ to_string t

  let version_fn (t : t) = to_string t ^ ".version"

  module Infix = Comparator.Operators (T)
end

module Id = struct
  module T = struct
    type t =
      { name : Name.t
      ; dir : Path.Source.t
      }

    let compare { dir; name } pkg =
      match Name.compare pkg.name name with
      | Eq -> Path.Source.compare dir pkg.dir
      | s -> s

    let to_dyn { dir; name } =
      let open Dyn.Encoder in
      record [ ("name", Name.to_dyn name); ("dir", Path.Source.to_dyn dir) ]
  end

  include T

  let hash { name; dir } = Tuple.T2.hash Name.hash Path.Source.hash (name, dir)

  let name t = t.name

  module C = Comparable.Make (T)
  module Set = C.Set
  module Map = C.Map
end

module Dependency = struct
  module Op = struct
    type t =
      | Eq
      | Gte
      | Lte
      | Gt
      | Lt
      | Neq

    let map =
      [ ("=", Eq); (">=", Gte); ("<=", Lte); (">", Gt); ("<", Lt); ("<>", Neq) ]

    let to_dyn =
      let open Dyn.Encoder in
      function
      | Eq -> string "Eq"
      | Gt -> string "Gt"
      | Gte -> string "Gte"
      | Lte -> string "Lte"
      | Lt -> string "Lt"
      | Neq -> string "Neq"

    let to_relop : t -> OpamParserTypes.relop = function
      | Eq -> `Eq
      | Gte -> `Geq
      | Lte -> `Leq
      | Gt -> `Gt
      | Lt -> `Lt
      | Neq -> `Neq
  end

  module Constraint = struct
    module Var = struct
      type t =
        | QVar of string
        | Var of string

      let decode =
        let open Dune_lang.Decoder in
        let+ s = string in
        if String.is_prefix s ~prefix:":" then
          Var (String.drop s 1)
        else
          QVar s

      let to_opam : t -> OpamParserTypes.value =
        let nopos = Opam_file.nopos in
        function
        | QVar x -> String (nopos, x)
        | Var x -> Ident (nopos, x)

      let to_dyn = function
        | QVar v -> Dyn.String v
        | Var v -> Dyn.String (":" ^ v)
    end

    type t =
      | Bvar of Var.t
      | Uop of Op.t * Var.t
      | Bop of Op.t * Var.t * Var.t
      | And of t list
      | Or of t list

    let decode =
      let open Dune_lang.Decoder in
      let ops =
        List.map Op.map ~f:(fun (name, op) ->
            ( name
            , let+ x = Var.decode
              and+ y = maybe Var.decode
              and+ loc = loc
              and+ version = Dune_lang.Syntax.get_exn Stanza.syntax in
              match y with
              | None -> Uop (op, x)
              | Some y ->
                if version < (2, 1) then
                  Dune_lang.Syntax.Error.since loc Stanza.syntax (2, 1)
                    ~what:(sprintf "Passing two arguments to %s" name);
                Bop (op, x, y) ))
      in
      let ops =
        ( "!="
        , let+ loc = loc in
          User_error.raise ~loc [ Pp.text "Use <> instead of !=" ] )
        :: ops
      in
      fix (fun t ->
          let logops =
            [ ( "and"
              , let+ x = repeat t in
                And x )
            ; ( "or"
              , let+ x = repeat t in
                Or x )
            ]
          in
          peek_exn >>= function
          | Atom (_loc, A s) when String.is_prefix s ~prefix:":" ->
            let+ () = junk in
            Bvar (Var (String.drop s 1))
          | _ -> sum (ops @ logops))

    let rec to_dyn =
      let open Dyn.Encoder in
      function
      | Bvar v -> constr "Bvar" [ Var.to_dyn v ]
      | Uop (b, x) -> constr "Uop" [ Op.to_dyn b; Var.to_dyn x ]
      | Bop (b, x, y) ->
        constr "Bop" [ Op.to_dyn b; Var.to_dyn x; Var.to_dyn y ]
      | And t -> constr "And" (List.map ~f:to_dyn t)
      | Or t -> constr "Or" (List.map ~f:to_dyn t)
  end

  type t =
    { name : Name.t
    ; constraint_ : Constraint.t option
    }

  let decode =
    let open Dune_lang.Decoder in
    let constrained =
      let+ name = Name.decode
      and+ expr = Constraint.decode in
      { name; constraint_ = Some expr }
    in
    enter constrained
    <|> let+ name = Name.decode in
        { name; constraint_ = None }

  let rec opam_constraint : Constraint.t -> OpamParserTypes.value =
    let nopos = Opam_file.nopos in
    function
    | Bvar v -> Constraint.Var.to_opam v
    | Uop (op, x) ->
      Prefix_relop (nopos, Op.to_relop op, Constraint.Var.to_opam x)
    | Bop (op, x, y) ->
      Relop
        ( nopos
        , Op.to_relop op
        , Constraint.Var.to_opam x
        , Constraint.Var.to_opam y )
    | And [ c ] -> opam_constraint c
    | And (c :: cs) ->
      Logop (nopos, `And, opam_constraint c, opam_constraint (And cs))
    | Or [ c ] -> opam_constraint c
    | Or (c :: cs) ->
      Logop (nopos, `Or, opam_constraint c, opam_constraint (And cs))
    | And []
    | Or [] ->
      Code_error.raise "opam_constraint" []

  let opam_depend : t -> OpamParserTypes.value =
    let nopos = Opam_file.nopos in
    fun { name; constraint_ } ->
      let constraint_ = Option.map ~f:opam_constraint constraint_ in
      let pkg : OpamParserTypes.value = String (nopos, Name.to_string name) in
      match constraint_ with
      | None -> pkg
      | Some c -> Option (nopos, pkg, [ c ])

  let to_dyn { name; constraint_ } =
    let open Dyn.Encoder in
    record
      [ ("name", Name.to_dyn name)
      ; ("constr", Dyn.Option (Option.map ~f:Constraint.to_dyn constraint_))
      ]
end

module Source_kind = struct
  module Host = struct
    type kind =
      | Github
      | Bitbucket
      | Gitlab

    let to_string = function
      | Github -> "github"
      | Bitbucket -> "bitbucket"
      | Gitlab -> "gitlab"

    type t =
      { user : string
      ; repo : string
      ; kind : kind
      }

    let dyn_of_kind kind = kind |> to_string |> Dyn.Encoder.string

    let to_dyn { user; repo; kind } =
      let open Dyn.Encoder in
      record
        [ ("kind", dyn_of_kind kind)
        ; ("user", string user)
        ; ("repo", string repo)
        ]

    let host_of_kind = function
      | Github -> "github.com"
      | Bitbucket -> "bitbucket.org"
      | Gitlab -> "gitlab.com"

    let homepage { kind; user; repo } =
      let host = host_of_kind kind in
      sprintf "https://%s/%s/%s" host user repo

    let bug_reports t =
      homepage t
      ^
      match t.kind with
      | Bitbucket
      | Github ->
        "/issues"
      | Gitlab -> "/-/issues"

    let enum k =
      [ ("GitHub", Github, None)
      ; ("Bitbucket", Bitbucket, Some (2, 8))
      ; ("Gitlab", Gitlab, Some (2, 8))
      ]
      |> List.map ~f:(fun (name, kind, since) ->
             let decode =
               let of_string ~loc s =
                 match String.split ~on:'/' s with
                 | [ user; repo ] -> k { kind; user; repo }
                 | _ ->
                   User_error.raise ~loc
                     [ Pp.textf "%s repository must be of form user/repo" name ]
               in
               let open Dune_lang.Decoder in
               (match since with
               | None -> return ()
               | Some v -> Dune_lang.Syntax.since Stanza.syntax v)
               >>> plain_string of_string
             in
             let constr = to_string kind in
             (constr, decode))

    let to_string { user; repo; kind } =
      sprintf "git+https://%s/%s/%s.git" (host_of_kind kind) user repo
  end

  type t =
    | Host of Host.t
    | Url of string

  let to_dyn =
    let open Dyn.Encoder in
    function
    | Host h -> constr "Host" [ Host.to_dyn h ]
    | Url url -> constr "Url" [ string url ]

  let to_string = function
    | Host h -> Host.to_string h
    | Url u -> u

  let decode =
    let open Dune_lang.Decoder in
    sum (("uri", string >>| fun s -> Url s) :: Host.enum (fun x -> Host x))
end

module Info = struct
  type t =
    { source : Source_kind.t option
    ; license : string option
    ; authors : string list option
    ; homepage : string option
    ; bug_reports : string option
    ; documentation : string option
    ; maintainers : string list option
    }

  let source t = t.source

  let license t = t.license

  let authors t = t.authors

  let homepage t =
    match (t.homepage, t.source) with
    | None, Some (Host h) -> Some (Source_kind.Host.homepage h)
    | s, _ -> s

  let bug_reports t =
    match (t.bug_reports, t.source) with
    | None, Some (Host h) -> Some (Source_kind.Host.bug_reports h)
    | s, _ -> s

  let documentation t = t.documentation

  let maintainers t = t.maintainers

  let empty =
    { source = None
    ; license = None
    ; authors = None
    ; homepage = None
    ; bug_reports = None
    ; documentation = None
    ; maintainers = None
    }

  let to_dyn
      { source
      ; license
      ; authors
      ; homepage
      ; bug_reports
      ; documentation
      ; maintainers
      } =
    let open Dyn.Encoder in
    record
      [ ("source", (option Source_kind.to_dyn) source)
      ; ("license", (option string) license)
      ; ("homepage", (option string) homepage)
      ; ("documentation", (option string) documentation)
      ; ("bug_reports", (option string) bug_reports)
      ; ("maintainers", option (list string) maintainers)
      ; ("authors", option (list string) authors)
      ]

  let decode ?since () =
    let open Dune_lang.Decoder in
    let v default = Option.value since ~default in
    let+ source =
      field_o "source"
        (Dune_lang.Syntax.since Stanza.syntax (v (1, 7)) >>> Source_kind.decode)
    and+ authors =
      field_o "authors"
        (Dune_lang.Syntax.since Stanza.syntax (v (1, 9)) >>> repeat string)
    and+ license =
      field_o "license"
        (Dune_lang.Syntax.since Stanza.syntax (v (1, 9)) >>> string)
    and+ homepage =
      field_o "homepage"
        (Dune_lang.Syntax.since Stanza.syntax (v (1, 10)) >>> string)
    and+ documentation =
      field_o "documentation"
        (Dune_lang.Syntax.since Stanza.syntax (v (1, 10)) >>> string)
    and+ bug_reports =
      field_o "bug_reports"
        (Dune_lang.Syntax.since Stanza.syntax (v (1, 10)) >>> string)
    and+ maintainers =
      field_o "maintainers"
        (Dune_lang.Syntax.since Stanza.syntax (v (1, 10)) >>> repeat string)
    in
    { source
    ; authors
    ; license
    ; homepage
    ; documentation
    ; bug_reports
    ; maintainers
    }

  let superpose t1 t2 =
    let f o1 o2 =
      match o2 with
      | Some _ as x -> x
      | None -> o1
    in
    { source = f t1.source t2.source
    ; authors = f t1.authors t2.authors
    ; license = f t1.license t2.license
    ; homepage = f t1.homepage t2.homepage
    ; documentation = f t1.documentation t2.documentation
    ; bug_reports = f t1.bug_reports t2.bug_reports
    ; maintainers = f t1.maintainers t2.maintainers
    }
end

type t =
  { id : Id.t
  ; loc : Loc.t
  ; synopsis : string option
  ; description : string option
  ; depends : Dependency.t list
  ; conflicts : Dependency.t list
  ; depopts : Dependency.t list
  ; info : Info.t
  ; version : string option
  ; has_opam_file : bool
  ; tags : string list
  ; deprecated_package_names : Loc.t Name.Map.t
  ; sites : Section.t Section.Site.Map.t
  }

(* Package name are globally unique, so we can reasonably expect that there will
   always be only a single value of type [t] with a given name in memory. That's
   why we only hash the name. *)
let hash t = Id.hash t.id

let name t = t.id.name

let dir t = t.id.dir

let decode ~dir =
  let open Dune_lang.Decoder in
  let name_map syntax of_list_map to_string name decode print_value error_msg =
    field ~default:[] name (syntax >>> repeat decode) >>| fun l ->
    match of_list_map l ~f:(fun (loc, s) -> (s, loc)) with
    | Ok x -> x
    | Error (name, (loc1, _), (loc2, _)) ->
      User_error.raise
        [ Pp.textf "%s %s is declared twice:" error_msg (to_string name)
        ; Pp.textf "- %s" (print_value loc1)
        ; Pp.textf "- %s" (print_value loc2)
        ]
  in
  fields
  @@ let+ loc = loc
     and+ name = field "name" Name.decode
     and+ synopsis = field_o "synopsis" string
     and+ description = field_o "description" string
     and+ version =
       field_o "version" (Dune_lang.Syntax.since Stanza.syntax (2, 5) >>> string)
     and+ depends = field ~default:[] "depends" (repeat Dependency.decode)
     and+ conflicts = field ~default:[] "conflicts" (repeat Dependency.decode)
     and+ depopts = field ~default:[] "depopts" (repeat Dependency.decode)
     and+ info = Info.decode ~since:(2, 0) ()
     and+ tags = field "tags" (enter (repeat string)) ~default:[]
     and+ deprecated_package_names =
       name_map
         (Dune_lang.Syntax.since Stanza.syntax (2, 0))
         Name.Map.of_list_map Name.to_string "deprecated_package_names"
         (located Name.decode) Loc.to_file_colon_line "Deprecated package name"
     and+ sites =
       name_map
         (Dune_lang.Syntax.since Stanza.syntax (2, 8))
         Section.Site.Map.of_list_map Section.Site.to_string "sites"
         (pair Section.decode Section.Site.decode)
         Section.to_string "Site location name"
     in
     let id = { Id.name; dir } in
     { id
     ; loc
     ; synopsis
     ; description
     ; depends
     ; conflicts
     ; depopts
     ; info
     ; version
     ; has_opam_file = false
     ; tags
     ; deprecated_package_names
     ; sites
     }

let to_dyn
    { id
    ; version
    ; synopsis
    ; description
    ; depends
    ; conflicts
    ; depopts
    ; info
    ; has_opam_file
    ; tags
    ; loc = _
    ; deprecated_package_names
    ; sites
    } =
  let open Dyn.Encoder in
  record
    [ ("id", Id.to_dyn id)
    ; ("synopsis", option string synopsis)
    ; ("description", option string description)
    ; ("depends", list Dependency.to_dyn depends)
    ; ("conflicts", list Dependency.to_dyn conflicts)
    ; ("depopts", list Dependency.to_dyn depopts)
    ; ("info", Info.to_dyn info)
    ; ("has_opam_file", Bool has_opam_file)
    ; ("tags", list string tags)
    ; ("version", option string version)
    ; ( "deprecated_package_names"
      , Name.Map.to_dyn Loc.to_dyn_hum deprecated_package_names )
    ; ("sites", Section.Site.Map.to_dyn Section.to_dyn sites)
    ]

let opam_file t = Path.Source.relative t.id.dir (Name.opam_fn t.id.name)

let meta_file t = Path.Source.relative t.id.dir (Name.meta_fn t.id.name)

let file ~dir ~name = Path.relative dir (Name.to_string name ^ opam_ext)

let deprecated_meta_file t name =
  Path.Source.relative t.id.dir (Name.meta_fn name)

let load_opam_file file name =
  let open Option.O in
  let loc = Loc.in_file (Path.source file) in
  let opam =
    match Opam_file.load (Path.source file) with
    | s -> Some s
    | exception exn ->
      User_warning.emit ~loc
        [ Pp.text
            "Unable to read opam file. Some information about this package \
             such as its version will be ignored."
        ; Pp.textf "Reason: %s" (Printexc.to_string exn)
        ];
      None
  in
  let get_one name =
    let* opam = opam in
    let* value = Opam_file.get_field opam name in
    match value with
    | String (_, s) -> Some s
    | _ -> None
  in
  let get_many name =
    let* opam = opam in
    let* value = Opam_file.get_field opam name in
    match value with
    | String (_, s) -> Some [ s ]
    | List (_, l) ->
      let+ l =
        List.fold_left l ~init:(Some []) ~f:(fun acc v ->
            let* acc = acc in
            match v with
            | OpamParserTypes.String (_, s) -> Some (s :: acc)
            | _ -> None)
      in
      List.rev l
    | _ -> None
  in
  let id = { Id.name; dir = Path.Source.parent_exn file } in
  { id
  ; loc
  ; version = get_one "version"
  ; conflicts = []
  ; depends = []
  ; depopts = []
  ; info =
      { maintainers = get_many "maintainer"
      ; authors = get_many "authors"
      ; homepage = get_one "homepage"
      ; bug_reports = get_one "bug-reports"
      ; documentation = get_one "doc"
      ; license = get_one "license"
      ; source =
          (let+ url = get_one "dev-repo" in
           Source_kind.Url url)
      }
  ; synopsis = get_one "synopsis"
  ; description = get_one "description"
  ; has_opam_file = true
  ; tags = Option.value (get_many "tags") ~default:[]
  ; deprecated_package_names = Name.Map.empty
  ; sites = Section.Site.Map.empty
  }

let missing_deps (t : t) ~effective_deps =
  let specified_deps =
    List.map t.depends ~f:(fun (dep : Dependency.t) -> dep.name)
    |> Name.Set.of_list
  in
  Name.Set.diff effective_deps specified_deps
