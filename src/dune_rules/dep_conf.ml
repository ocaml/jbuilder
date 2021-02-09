open! Dune_engine
open! Stdune
open Import
open Dune_lang.Decoder

type t =
  | File of String_with_vars.t
  | Alias of String_with_vars.t
  | Alias_rec of String_with_vars.t
  | Glob_files of
      { glob : String_with_vars.t
      ; recursive : bool
      }
  | Source_tree of String_with_vars.t
  | Package of String_with_vars.t
  | Universe
  | Env_var of String_with_vars.t
  | Sandbox_config of Sandbox_config.t

let remove_locs = function
  | File sw -> File (String_with_vars.remove_locs sw)
  | Alias sw -> Alias (String_with_vars.remove_locs sw)
  | Alias_rec sw -> Alias_rec (String_with_vars.remove_locs sw)
  | Glob_files g ->
    Glob_files { g with glob = String_with_vars.remove_locs g.glob }
  | Source_tree sw -> Source_tree (String_with_vars.remove_locs sw)
  | Package sw -> Package (String_with_vars.remove_locs sw)
  | Universe -> Universe
  | Env_var sw -> Env_var sw
  | Sandbox_config s -> Sandbox_config s

let decode_sandbox_config =
  let+ () = Dune_lang.Syntax.since Stanza.syntax (1, 12)
  and+ loc, x =
    located
      (repeat
         (sum
            [ ("none", return Sandbox_config.Partial.no_sandboxing)
            ; ("always", return Sandbox_config.Partial.needs_sandboxing)
            ; ( "preserve_file_kind"
              , return (Sandbox_config.Partial.disallow Sandbox_mode.symlink) )
            ]))
  in
  Sandbox_config.Partial.merge ~loc x

let decode =
  let decode =
    let sw = String_with_vars.decode in
    sum ~force_parens:true
      [ ("file", sw >>| fun x -> File x)
      ; ("alias", sw >>| fun x -> Alias x)
      ; ("alias_rec", sw >>| fun x -> Alias_rec x)
      ; ( "glob_files"
        , sw >>| fun x -> Glob_files { glob = x; recursive = false } )
      ; ( "glob_files_rec"
        , let+ () = Dune_lang.Syntax.since Stanza.syntax (2, 9)
          and+ x = sw in
          Glob_files { glob = x; recursive = true } )
      ; ("package", sw >>| fun x -> Package x)
      ; ("universe", return Universe)
      ; ( "files_recursively_in"
        , let+ () =
            Dune_lang.Syntax.renamed_in Stanza.syntax (1, 0) ~to_:"source_tree"
          and+ x = sw in
          Source_tree x )
      ; ( "source_tree"
        , let+ () = Dune_lang.Syntax.since Stanza.syntax (1, 0)
          and+ x = sw in
          Source_tree x )
      ; ("env_var", sw >>| fun x -> Env_var x)
      ; ("sandbox", decode_sandbox_config >>| fun x -> Sandbox_config x)
      ]
  in
  decode
  <|> let+ x = String_with_vars.decode in
      File x

open Dune_lang

let encode = function
  | File t ->
    List [ Dune_lang.unsafe_atom_of_string "file"; String_with_vars.encode t ]
  | Alias t ->
    List [ Dune_lang.unsafe_atom_of_string "alias"; String_with_vars.encode t ]
  | Alias_rec t ->
    List
      [ Dune_lang.unsafe_atom_of_string "alias_rec"; String_with_vars.encode t ]
  | Glob_files { glob = t; recursive } ->
    List
      [ Dune_lang.unsafe_atom_of_string
          (if recursive then
            "glob_files_rec"
          else
            "glob_files")
      ; String_with_vars.encode t
      ]
  | Source_tree t ->
    List
      [ Dune_lang.unsafe_atom_of_string "source_tree"
      ; String_with_vars.encode t
      ]
  | Package t ->
    List
      [ Dune_lang.unsafe_atom_of_string "package"; String_with_vars.encode t ]
  | Universe -> Dune_lang.unsafe_atom_of_string "universe"
  | Env_var t ->
    List
      [ Dune_lang.unsafe_atom_of_string "env_var"; String_with_vars.encode t ]
  | Sandbox_config config ->
    if Sandbox_config.equal config Sandbox_config.no_special_requirements then
      List []
    else
      Code_error.raise "There's no syntax for [Sandbox_config] yet" []

let to_dyn t = Dune_lang.to_dyn (encode t)
