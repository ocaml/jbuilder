open Import

type format = Text | JSON | Sexp

let dyn_list ~f l = Dyn.List (List.map ~f l)

module Json = struct
  type t =
    | Null
    | String of string
    | List of t list
    | Record of (string * t) list

  let rec of_dyn (dyn : Dyn.t) : t =
    match dyn with
    | Record kvs -> Record (List.map kvs ~f:(fun (k, v) -> (k, of_dyn v)))
    | List dyns -> List (List.map ~f:of_dyn dyns)
    | String s -> String s
    | Option None -> Null
    | Option (Some dyn) -> of_dyn dyn
    | dyn -> Code_error.raise "Json.of_dyn: unsupported case" [ ("dyn", dyn) ]

  type tag =
    | Key
    | String
    | Symbol
    | Null

  let ansi_color =
    let open Ansi_color.Style in
    function
    | Key -> [bold; fg_bright_blue]
    | String -> [fg_green]
    | Symbol -> [bold]
    | Null -> [bold; fg_black]

  let pp_symbol s =
    Pp.tag ~tag:Symbol (Pp.text s)

  let pp_comma = Pp.seq (pp_symbol ",") Pp.cut

  let surround pre post x = Pp.concat [ pp_symbol pre; x; pp_symbol post ]

  let pp_key s =
    Pp.tag (Pp.textf "%S:" s) ~tag:Key

  let rec pp = function
    | Record kvs ->
        Pp.concat_map ~sep:pp_comma kvs ~f:pp_kv |> Pp.vbox |> surround "{" "}"
    | List js ->
        Pp.concat_map ~sep:pp_comma js ~f:pp |> Pp.vbox |> surround "[" "]"
    | String s -> Pp.hbox (Pp.tag ~tag:String (Pp.textf "%S" s))
    | Null -> Pp.tag ~tag:Null (Pp.text "null")

  and pp_kv (s, json) =
    Pp.hvbox (Pp.concat [ pp_key s; Pp.space; pp json ])
end

module Lib_data = struct
  type t = {
    name : Package.Name.t;
    deps : Lib_name.t list;
    synopsis : string option
  }

  let pp ppf { name; deps; synopsis } =
    Format.fprintf ppf "Public library: %a\n" Package.Name.pp name;
    Format.fprintf ppf "Dependencies: %a\n"
      (Fmt.list ~pp_sep:(fun ppf () -> Format.fprintf ppf ", ") Lib_name.pp)
      deps;
    Option.iter ~f:(Format.fprintf ppf "Synopsis: %s\n") synopsis

  let to_dyn { name; deps; synopsis } =
    Dyn.Record
      [ ("name", Package.Name.to_dyn name);
        ("deps", dyn_list deps ~f:Lib_name.to_dyn);
        ("synopsis", Option (Option.map synopsis ~f:String.to_dyn))
      ]

  let of_lib (lib : Dune_file.Library.t) =
    Option.map lib.public ~f:(fun pub ->
        { name = pub.package.name;
          deps =
            List.concat_map lib.buildable.libraries
              ~f:Dune_file.Lib_dep.to_lib_names;
          synopsis = lib.synopsis
        } )
end

module Project_data = struct
  type t = { name : Dune_project.Name.t; libs : Lib_data.t list }

  let pp ppf { name; libs } =
    Format.fprintf ppf "Project name: %s\n"
      (Dune_project.Name.to_string_hum name);
    List.iter libs ~f:(Lib_data.pp ppf)

  let project_name_to_dyn (name : Dune_project.Name.t) =
    String.to_dyn (Dune_project.Name.to_encoded_string name)

  let to_dyn { name; libs } =
    Dyn.Record
      [ ("name", project_name_to_dyn name);
        ("libs", dyn_list libs ~f:Lib_data.to_dyn)
      ]

  let fold_stanzas_in_project project dune_files ~f ~init =
    List.fold_left dune_files ~init
      ~f:(fun acc (dune_file : Dune_load.Dune_file.t) ->
        if Dune_project.equal project dune_file.project then
          List.fold_left dune_file.stanzas ~f ~init:acc
        else acc )

  let of_project project dune_files =
    let libs =
      fold_stanzas_in_project project dune_files ~init:[] ~f:(fun acc stanza ->
          match stanza with
          | Dune_file.Library lib -> (
            match Lib_data.of_lib lib with Some d -> d :: acc | None -> acc )
          | _ -> acc )
      |> List.rev
    in
    { name = Dune_project.name project; libs }
end

let rec dune_lang_of_dyn : Dyn.t -> Dune_lang.t = function
  | Record kvs ->
    List
      (List.map kvs ~f:(fun (k, v) ->
         Dune_lang.List
           [ Atom (Dune_lang.Atom.of_string k)
           ; dune_lang_of_dyn v
           ]
        ))
  | String s -> Quoted_string s
  | List l -> List (List.map ~f:dune_lang_of_dyn l)
  | Option (Some d) -> dune_lang_of_dyn d
  | Option None -> List []
  | dyn ->
    Code_error.raise
      "dune_lang_of_dyn: unsupported case"
      [ ("dyn", dyn) ]

let describe project dune_files ~format =
  let project_data = Project_data.of_project project dune_files in
  match format with
  | Text -> Format.printf "%a" Project_data.pp project_data
  | JSON ->
    Project_data.to_dyn project_data
    |> Json.of_dyn
    |> Json.pp
    |> Pp.map_tags ~f:Json.ansi_color
    |> Ansi_color.print;
    print_newline ()
  | Sexp ->
    Project_data.to_dyn project_data
    |> dune_lang_of_dyn
    |> Dune_lang.add_loc ~loc:Loc.none
    |> Dune_lang.Cst.concrete
    |> List.singleton
    |> Format.printf "%a" Format_dune_lang.pp_top_sexps
