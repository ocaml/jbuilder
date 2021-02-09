open Stdune

type var_syntax =
  | Parens
  | Braces

type var =
  { loc : Loc.t
  ; name : string
  ; payload : string option
  ; syntax : var_syntax
  }

type part =
  | Text of string
  | Var of var

module Token = struct
  type t =
    | String of string
    | Open of var_syntax
    | Close of var_syntax

  let tokenise s =
    let len = String.length s in
    let sub i j = String.sub s ~pos:i ~len:(j - i) in
    let cons_str i j acc =
      if i = j then
        acc
      else
        String (sub i j) :: acc
    in
    let rec loop i j =
      if j = len then
        cons_str i j []
      else
        match s.[j] with
        | '}' -> cons_str i j (Close Braces :: loop (j + 1) (j + 1))
        | ')' -> cons_str i j (Close Parens :: loop (j + 1) (j + 1))
        | '$' when j + 1 < len -> (
          match s.[j + 1] with
          | '{' -> cons_str i j (Open Braces :: loop (j + 2) (j + 2))
          | '(' -> cons_str i j (Open Parens :: loop (j + 2) (j + 2))
          | _ -> loop i (j + 1))
        | _ -> loop i (j + 1)
    in
    loop 0 0

  let to_string = function
    | String s -> s
    | Open Braces -> "${"
    | Open Parens -> "$("
    | Close Braces -> "}"
    | Close Parens -> ")"
end

(* Remark: Consecutive [Text] items are concatenated. *)
let rec of_tokens : Loc.t -> Token.t list -> part list =
 fun loc -> function
  | [] -> []
  | Open a :: String s :: Close b :: rest when a = b ->
    let name, payload =
      match String.lsplit2 s ~on:':' with
      | None -> (s, None)
      | Some (n, p) -> (n, Some p)
    in
    Var { loc; name; payload; syntax = a } :: of_tokens loc rest
  | token :: rest -> (
    let s = Token.to_string token in
    match of_tokens loc rest with
    | Text s' :: l -> Text (s ^ s') :: l
    | l -> Text s :: l)

let parse ~loc s = of_tokens loc (Token.tokenise s)

module Upgrade_var = struct
  type info =
    | Keep
    | Deleted of string
    | Renamed_to of string

  let map =
    let macros =
      [ ("exe", Keep)
      ; ("bin", Keep)
      ; ("lib", Keep)
      ; ("libexec", Keep)
      ; ("lib-available", Keep)
      ; ("version", Keep)
      ; ("read", Keep)
      ; ("read-lines", Keep)
      ; ("read-strings", Keep)
      ; ("path", Renamed_to "dep")
      ; ("findlib", Renamed_to "lib")
      ; ("path-no-dep", Deleted "")
      ; ("ocaml-config", Keep)
      ]
    in
    let static_vars =
      [ ( "<"
        , Deleted
            "Use a named dependency instead:\n\n\
            \  (deps (:x <dep>) ...)\n\
            \   ... %{x} ..." )
      ; ("@", Renamed_to "targets")
      ; ("^", Renamed_to "deps")
      ; ("SCOPE_ROOT", Renamed_to "project_root")
      ]
    in
    let lowercased =
      [ ("cpp", Keep)
      ; ("pa_cpp", Keep)
      ; ("cc", Keep)
      ; ("cxx", Keep)
      ; ("ocaml", Keep)
      ; ("ocamlc", Keep)
      ; ("ocamlopt", Keep)
      ; ("arch_sixtyfour", Keep)
      ; ("make", Keep)
      ]
    in
    let uppercased =
      List.map lowercased ~f:(fun (k, _) -> (String.uppercase k, Renamed_to k))
    in
    let other =
      [ ("-verbose", Keep)
      ; ("ocaml_bin", Keep)
      ; ("ocaml_version", Keep)
      ; ("ocaml_where", Keep)
      ; ("null", Keep)
      ; ("ext_obj", Keep)
      ; ("ext_asm", Keep)
      ; ("ext_lib", Keep)
      ; ("ext_dll", Keep)
      ; ("ext_exe", Keep)
      ; ("profile", Keep)
      ; ("workspace_root", Keep)
      ; ("context_name", Keep)
      ; ("ROOT", Renamed_to "workspace_root")
      ; ("corrected-suffix", Keep)
      ; ("library-name", Keep)
      ; ("impl-files", Keep)
      ; ("intf-files", Keep)
      ]
    in
    String.Map.of_list_exn
      (List.concat [ macros; static_vars; lowercased; uppercased; other ])
end

let string_of_var { loc = _; name; payload; syntax } =
  let s =
    match payload with
    | None -> name
    | Some p -> sprintf "%s:%s" name p
  in
  match syntax with
  | Parens -> sprintf "$(%s)" s
  | Braces -> sprintf "${%s}" s

let upgrade_to_dune s ~loc ~quoted ~allow_first_dep_var =
  let map_var v =
    match String.Map.find Upgrade_var.map v.name with
    | None -> None
    | Some info -> (
      match info with
      | Deleted repl ->
        if v.name = "<" && allow_first_dep_var then
          Some v.name
        else
          User_error.raise ~loc:v.loc
            [ Pp.textf "this form is not allowed in dune files.%s" repl ]
      | Keep -> Some v.name
      | Renamed_to new_name -> Some new_name)
  in
  let map_part = function
    | Text s -> Dune_lang.Template.Text s
    | Var v -> (
      match map_var v with
      | None -> Text (string_of_var v)
      | Some name -> Pform { name; payload = v.payload; loc = v.loc })
  in
  let parts = List.map (parse ~loc s) ~f:map_part in
  match
    List.fold_left parts ~init:(Some "") ~f:(fun acc part ->
        match (acc, part) with
        | Some s, Dune_lang.Template.Text s' -> Some (s ^ s')
        | _ -> None)
  with
  | None -> Dune_lang.Ast.Template { quoted; parts; loc }
  | Some s -> Dune_lang.Ast.atom_or_quoted_string loc s
