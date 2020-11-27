(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*           Xavier Leroy and Damien Doligez, INRIA Rocquencourt          *)
(*                                                                        *)
(*   Copyright 1996 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(** transitioning copied from 4.11 *)

val null : string

val quote_command :
     string
  -> ?stdin:string
  -> ?stdout:string
  -> ?stderr:string
  -> string list
  -> string
