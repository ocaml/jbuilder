(**************************************************************************)
(*                                                                        *)
(*    Copyright 2012-2020 OCamlPro                                        *)
(*    Copyright 2012 INRIA                                                *)
(*                                                                        *)
(*  All rights reserved. This file is distributed under the terms of the  *)
(*  GNU Lesser General Public License version 2.1, with the special       *)
(*  exception on linking described in the file LICENSE.                   *)
(*                                                                        *)
(**************************************************************************)

(** Defines the types for the opam format lexer and parser *)

(** Relational operators *)
type relop = [ `Eq  (** [=] *)
             | `Neq (** [!=] *)
             | `Geq (** [>=] *)
             | `Gt  (** [>] *)
             | `Leq (** [<=] *)
             | `Lt  (** [<] *)
             ]

(** Logical operators *)
type logop = [ `And (** [&] *) | `Or (** [|] *) ]

(** Prefix operators *)
type pfxop = [ `Not (** [!] *) | `Defined (** [?] *) ]

type file_name = string

(** Source file positions: [(filename, line, column)] *)
type pos = file_name * int * int

(** Environment variable update operators *)
type env_update_op = Eq       (** [=] *)
                   | PlusEq   (** [+=] *)
                   | EqPlus   (** [=+] *)
                   | ColonEq  (** [:=] *)
                   | EqColon  (** [=:] *)
                   | EqPlusEq (** [=+=] *)

(** Base values *)
type value =
  | Bool of pos * bool
      (** [bool] atoms *)
  | Int of pos * int
      (** [int] atoms *)
  | String of pos * string
      (** [string] atoms *)
  | Relop of pos * relop * value * value
      (** Relational operators with two values (e.g. [os != "win32"]) *)
  | Prefix_relop of pos * relop * value
      (** Relational operators in prefix position (e.g. [< "4.07.0"]) *)
  | Logop of pos * logop * value * value
      (** Logical operators *)
  | Pfxop of pos * pfxop * value
      (** Prefix operators *)
  | Ident of pos * string
      (** Identifiers *)
  | List of pos * value list
      (** Lists of values ([[x1 x2 ... x3]]) *)
  | Group of pos * value list
      (** Groups of values ([(x1 x2 ... x3)]) *)
  | Option of pos * value * value list
      (** Value with optional list ([x1 {x2 x3 x4}]) *)
  | Env_binding of pos * value * env_update_op * value
      (** Environment variable binding ([FOO += "bar"]) *)

(** An opamfile section *)
type opamfile_section = {
  section_kind  : string;            (** Section kind (e.g. [extra-source]) *)
  section_name  : string option;     (** Section name (e.g. ["myfork.patch"]) *)
  section_items : opamfile_item list (** Content of the section *);
}

(** An opamfile is composed of sections and variable definitions *)
and opamfile_item =
  | Section of pos * opamfile_section (** e.g. [kind ["name"] { ... }] *)
  | Variable of pos * string * value  (** e.g. [opam-version: "2.0"] *)

(** A file is a list of items and the filename *)
type opamfile = {
  file_contents: opamfile_item list; (** Content of the file *)
  file_name    : file_name;          (** Name of the disk file this record was
                                         loaded from *)
}
