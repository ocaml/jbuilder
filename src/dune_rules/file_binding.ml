open! Dune_engine
open! Stdune

type ('src, 'dst) t =
  { src : 'src
  ; dst : 'dst option
  }

let equal f g { src; dst } t = f src t.src && Option.equal g dst t.dst

module Expanded = struct
  type nonrec t = (Loc.t * Path.Build.t, Loc.t * string) t

  let src t = snd t.src

  let dst t = Option.map ~f:snd t.dst

  let src_loc t = fst t.src

  let dst_basename { src = _, src; dst } =
    match dst with
    | Some (_, dst) -> dst
    | None ->
      let basename = Path.Build.basename src in
      String.drop_suffix basename ~suffix:".exe"
      |> Option.value ~default:basename

  let dst_path t ~dir = Path.Build.relative dir (dst_basename t)
end

module Unexpanded = struct
  type nonrec t = (String_with_vars.t, String_with_vars.t) t

  let equal = equal String_with_vars.equal_no_loc String_with_vars.equal_no_loc

  let make ~src:(locs, src) ~dst:(locd, dst) =
    { src = String_with_vars.make_text locs src
    ; dst = Some (String_with_vars.make_text locd dst)
    }

  let expand_src t ~dir ~f = Path.Build.relative dir (f t.src)

  let destination_relative_to_install_path t ~section ~expand ~expand_partial =
    let dst = Option.map ~f:expand t.dst in
    Install.Entry.adjust_dst ~section ~src:(expand_partial t.src) ~dst

  let expand t ~dir ~f =
    let f sw = (String_with_vars.loc sw, f sw) in
    let src =
      let loc, expanded = f t.src in
      (loc, Path.Build.relative dir expanded)
    in
    { src
    ; dst =
        (let f sw =
           let loc, p = f sw in
           (loc, p)
         in
         Option.map ~f t.dst)
    }

  module L = struct
    let decode_file =
      let open Dune_lang.Decoder in
      let decode =
        let+ is_atom =
          peek_exn >>| function
          | Atom _ -> true
          | _ -> false
        and+ s = String_with_vars.decode
        and+ version = Dune_lang.Syntax.get_exn Stanza.syntax in
        if (not is_atom) && version < (1, 6) then
          let what =
            (if String_with_vars.has_pforms s then
              "variables"
            else
              "quoted strings")
            |> sprintf "Using %s here"
          in
          Dune_lang.Syntax.Error.since (String_with_vars.loc s) Stanza.syntax
            (1, 6) ~what
        else
          s
      in
      peek_exn >>= function
      | Atom _
      | Quoted_string _
      | Template _ ->
        decode >>| fun src -> { src; dst = None }
      | List (_, [ _; Atom (_, A "as"); _ ]) ->
        enter
          (let* src = decode in
           keyword "as"
           >>> let* dst = decode in
               return { src; dst = Some dst })
      | sexp ->
        User_error.raise ~loc:(Dune_lang.Ast.loc sexp)
          [ Pp.text
              "invalid format, <name> or (<name> as <install-as>) expected"
          ]

    let decode =
      let open Dune_lang.Decoder in
      repeat decode_file
  end
end
