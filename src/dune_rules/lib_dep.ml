open! Dune_engine
open Stdune

module Select = struct
  module Choice = struct
    type t =
      { required : Lib_name.Set.t
      ; forbidden : Lib_name.Set.t
      ; file : string
      }

    let decode ~result_fn =
      let open Dune_lang.Decoder in
      enter
        (let* dune_version = Dune_lang.Syntax.get_exn Stanza.syntax in
         let+ loc = loc
         and+ preds, file =
           until_keyword "->"
             ~before:
               (let+ s = string
                and+ loc = loc in
                match String.drop_prefix s ~prefix:"!" with
                | Some s -> Right (Lib_name.parse_string_exn (loc, s))
                | None -> Left (Lib_name.parse_string_exn (loc, s)))
             ~after:(located filename)
         in
         match file with
         | None ->
           User_error.raise ~loc
             [ Pp.textf "(<[!]libraries>... -> <file>) expected" ]
         | Some (loc_file, file) ->
           let () =
             if dune_version >= (2, 0) then
               let prefix, suffix =
                 let name, ext = Filename.split_extension result_fn in
                 let prefix = name ^ "." in
                 (prefix, ext)
               in
               if
                 not
                   (String.is_prefix file ~prefix
                   && String.is_suffix file ~suffix)
               then
                 User_error.raise ~loc:loc_file
                   [ Pp.textf
                       "The format for files in this select branch must be \
                        %s{name}%s"
                       prefix suffix
                   ]
           in
           let rec loop required forbidden = function
             | [] ->
               let common = Lib_name.Set.inter required forbidden in
               Option.iter (Lib_name.Set.choose common) ~f:(fun name ->
                   User_error.raise ~loc
                     [ Pp.textf
                         "library %S is both required and forbidden in this \
                          clause"
                         (Lib_name.to_string name)
                     ]);
               { required; forbidden; file }
             | Left s :: l -> loop (Lib_name.Set.add required s) forbidden l
             | Right s :: l -> loop required (Lib_name.Set.add forbidden s) l
           in
           loop Lib_name.Set.empty Lib_name.Set.empty preds)

    let to_dyn { required; forbidden; file } =
      let open Dyn.Encoder in
      record
        [ ("required", Lib_name.Set.to_dyn required)
        ; ("forbidden", Lib_name.Set.to_dyn forbidden)
        ; ("file", string file)
        ]
  end

  type t =
    { result_fn : string
    ; choices : Choice.t list
    ; loc : Loc.t
    }

  let to_dyn { result_fn; choices; loc = _ } =
    let open Dyn.Encoder in
    record
      [ ("result_fn", string result_fn)
      ; ("choices", list Choice.to_dyn choices)
      ]

  let decode =
    let open Dune_lang.Decoder in
    let* result_fn = filename in
    let+ loc = loc
    and+ () = keyword "from"
    and+ choices = repeat (Choice.decode ~result_fn) in
    { result_fn; choices; loc }
end

type t =
  | Direct of (Loc.t * Lib_name.t)
  | Re_export of (Loc.t * Lib_name.t)
  | Select of Select.t

let to_dyn =
  let open Dyn.Encoder in
  function
  | Direct (_, name) -> Lib_name.to_dyn name
  | Re_export (_, name) -> constr "re_export" [ Lib_name.to_dyn name ]
  | Select s -> constr "select" [ Select.to_dyn s ]

let direct x = Direct x

let re_export x = Re_export x

let decode ~allow_re_export =
  let open Dune_lang.Decoder in
  let+ loc, t =
    located
      (sum ~force_parens:true
         [ ( "re_export"
           , let+ () = Dune_lang.Syntax.since Stanza.syntax (2, 0)
             and+ loc, name = located Lib_name.decode in
             Re_export (loc, name) )
         ; ( "select"
           , let+ select = Select.decode in
             Select select )
         ]
      <|> let+ loc, name = located Lib_name.decode in
          Direct (loc, name))
  in
  match t with
  | Re_export _ when not allow_re_export ->
    User_error.raise ~loc [ Pp.text "re_export is not allowed here" ]
  | _ -> t

let encode =
  let open Dune_lang.Encoder in
  function
  | Direct (_, name) -> Lib_name.encode name
  | Re_export (_, name) -> constr "re_export" Lib_name.encode name
  | Select select ->
    Code_error.raise "Lib_dep.encode: cannot encode select"
      [ ("select", Select.to_dyn select) ]

module L = struct
  let field_encode t ~name =
    let open Dune_lang.Encoder in
    field_l name encode t
end
