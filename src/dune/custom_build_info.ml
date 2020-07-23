open! Stdune

type t =
  { max_size : int
  ; action : Loc.t * Action_dune_lang.t
  }

let output_file name = Printf.sprintf ".%s_custom_build_info.txt-gen" name

let decode () =
  let open Dune_lang.Decoder in
  field_o "custom_build_info"
    ( Dune_lang.Syntax.since Stanza.syntax (2, 7)
    >>> fields
          (let+ max_size = field "max_size" int
           and+ action = field "action" (located Action_dune_lang.decode) in
           { max_size; action }) )

let to_dyn { max_size; action = _, action } =
  let open Dyn.Encoder in
  record
    [ ("max_size", int max_size); ("action", Action_dune_lang.to_dyn action) ]

let encode { max_size; action = _, action } =
  let open Dune_lang.Encoder in
  let fields =
    record_fields
      [ field "max_size" int max_size
      ; field "action" Action_dune_lang.encode action
      ]
  in
  Dune_lang.List (Dune_lang.atom "custom_build_info" :: fields)
