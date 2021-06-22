open! Dune_engine

(* This file is licensed under The MIT License *)
(* (c) MINES ParisTech 2018-2019               *)
(* (c) INRIA 2020                              *)
(* Written by: Emilio Jesús Gallego Arias *)

(* Build rules for Coq's .v -> .vo files *)

open! Stdune
open Coq_stanza

val setup_rules :
     sctx:Super_context.t
  -> dir:Path.Build.t
  -> dir_contents:Dir_contents.t
  -> Theory.t
  -> Action.t Action_builder.With_targets.t list Memo.Build.t

(* We allos to override the install package for some files, for example, those
   coming from the Coq native compiler *)
val install_rules :
     sctx:Super_context.t
  -> dir:Path.Build.t
  -> Theory.t
  -> (Package.t * (Loc.t option * Path.Build.t Install.Entry.t)) list
     Memo.Build.t

val coqpp_rules :
     sctx:Super_context.t
  -> dir:Path.Build.t
  -> Coqpp.t
  -> Action.t Action_builder.With_targets.t list Memo.Build.t

val extraction_rules :
     sctx:Super_context.t
  -> dir:Path.Build.t
  -> dir_contents:Dir_contents.t
  -> Extraction.t
  -> Action.t Action_builder.With_targets.t list Memo.Build.t
