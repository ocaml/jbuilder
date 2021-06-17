(* This file is licensed under The MIT License *)
(* (c) MINES ParisTech 2018-2019               *)
(* (c) INRIA 2020                              *)
(* Written by: Emilio Jesús Gallego Arias *)

(* Legacy is used to signal that we are in a mode prior to Coq syntax 0.3 ,
   where mode was not supported, this allows us support older Coq compiler
   versions with coq-lang < 0.3 *)
type t =
  | Legacy
  | VoOnly
  | Native
  | Split of
      { package : string option
      ; profile : string list
      }

let default_profile = [ "release" ]

let default = Split { package = None; profile = default_profile }

let decode_v03 = Dune_lang.Decoder.(enum [ ("vo", VoOnly); ("native", Native) ])

let decode_v04 =
  let open Dune_lang.Decoder in
  let native =
    fields
      (let+ package = field_o "package" string
       and+ profile =
         field ~default:default_profile "profile" (repeat string)
       in
       Split { package; profile })
  in
  sum [ ("vo", return VoOnly); ("native", native) ]
