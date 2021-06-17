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

val default : t

(* We provide two different decoders depending on the Coq language syntax *)
val decode_v03 : t Dune_lang.Decoder.t

val decode_v04 : t Dune_lang.Decoder.t
