type stanza = Stanza.t = ..

module Stanza = struct
  open Stanza.Of_sexp

  let field_oslu name = Ordered_set_lang.Unexpanded.field name

  module Jsoo_compilation = struct
    type t =
      | Separate
      | Classic

    let default ~profile =
      match profile with
      | "dev" -> Separate
      | _     -> Classic

    let t =
      Syntax.since Stanza.syntax (1, 1) >>= fun () ->
      enum
        [ "separate", Separate
        ; "classic", Classic
        ]
  end

  type config =
    { flags          : Ordered_set_lang.Unexpanded.t
    ; ocamlc_flags   : Ordered_set_lang.Unexpanded.t
    ; ocamlopt_flags : Ordered_set_lang.Unexpanded.t

    ; jsoo_compilation : Jsoo_compilation.t option
    }

  type pattern =
    | Profile of string
    | Any

  type t =
    { loc   : Loc.t
    ; rules : (pattern * config) list
    }

  let config =
    let%map flags = field_oslu "flags"
    and ocamlc_flags = field_oslu "ocamlc_flags"
    and ocamlopt_flags = field_oslu "ocamlopt_flags"
    and jsoo_compilation = field_o "jsoo_compilation" Jsoo_compilation.t
    in
    { flags
    ; ocamlc_flags
    ; ocamlopt_flags
    ; jsoo_compilation
    }

  let rule =
    enter
      (let%map pat =
         match_keyword [("_", return Any)]
           ~fallback:(string >>| fun s -> Profile s)
       and configs = fields config
       in
       (pat, configs))

  let t =
    let%map () = Syntax.since Stanza.syntax (1, 0)
    and loc = loc
    and rules = repeat rule
    in
    { loc; rules }

end

type stanza +=
  | T of Stanza.t
