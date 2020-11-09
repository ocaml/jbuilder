(**************************************************************************)
(*                                                                        *)
(*    Copyright 2012-2018 OCamlPro                                        *)
(*    Copyright 2012 INRIA                                                *)
(*                                                                        *)
(*  All rights reserved. This file is distributed under the terms of the  *)
(*  GNU Lesser General Public License version 2.1, with the special       *)
(*  exception on linking described in the file LICENSE.                   *)
(*                                                                        *)
(**************************************************************************)

(** opam format lexer *)

open OpamParserTypes

exception Error of string
(** Raised on any lexing error with a description of the fault. Note that
    [Failure "lexing: empty token"] is never raised by the lexer. *)

val relop: string -> relop
(** Inverse of {!OpamPrinter.relop} *)

val logop: string -> logop
(** Inverse of {!OpamPrinter.logop} *)

val pfxop: string -> pfxop
(** Inverse of {!OpamPrinter.pfxop} *)

val env_update_op: string -> env_update_op
(** Inverse of {!OpamPrinter.env_update_op} *)


val token: Lexing.lexbuf -> OpamBaseParser.token
