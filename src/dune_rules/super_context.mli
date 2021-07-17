(** An augmented context *)
open! Dune_engine

(** A context augmented with: a lib-db, ... Super context are used for
    generating rules. *)

open! Stdune
open Import

type t

val all : t Context_name.Map.t Memo.Lazy.t

(** Find a super context by name. *)
val find : Context_name.t -> t option Memo.Build.t

val modules_of_lib :
  (* to avoid a cycle with [Dir_contents] *)
  (t -> dir:Path.Build.t -> name:Lib_name.t -> Modules.t Memo.Build.t) Fdecl.t

val to_dyn : t -> Dyn.t

val context : t -> Context.t

(** Context env with additional variables computed from packages *)
val context_env : t -> Env.t

val stanzas : t -> Dune_file.Stanzas.t Dir_with_dune.t list

val stanzas_in :
  t -> dir:Path.Build.t -> Dune_file.Stanzas.t Dir_with_dune.t option

val packages : t -> Package.t Package.Name.Map.t

val host : t -> t

val any_package :
  t -> Package.Name.t -> Expander.any_package option Memo.Build.t

val get_site_of_packages :
     t
  -> loc:Loc.t
  -> pkg:Package.Name.t
  -> site:Section.Site.t
  -> Section.t Memo.Build.t

module Lib_entry : sig
  type t =
    | Library of Lib.Local.t
    | Deprecated_library_name of Dune_file.Deprecated_library_name.t
end

val lib_entries_of_package : t -> Package.Name.t -> Lib_entry.t list

(** All public libraries of the workspace *)
val public_libs : t -> Lib.DB.t

(** Installed libraries that are not part of the workspace *)
val installed_libs : t -> Lib.DB.t

(** All non-public library names *)
val internal_lib_names : t -> Lib_name.Set.t

(** Compute the ocaml flags based on the directory environment and a buildable
    stanza *)
val ocaml_flags :
  t -> dir:Path.Build.t -> Ocaml_flags.Spec.t -> Ocaml_flags.t Memo.Build.t

val foreign_flags :
     t
  -> dir:Path.Build.t
  -> expander:Expander.t
  -> flags:Ordered_set_lang.Unexpanded.t
  -> language:Foreign_language.t
  -> string list Action_builder.t

val menhir_flags :
     t
  -> dir:Path.Build.t
  -> expander:Expander.t
  -> flags:Ordered_set_lang.Unexpanded.t
  -> string list Action_builder.t

(** Binaries that are symlinked in the associated .bin directory of [dir]. This
    associated directory is [Path.relative dir ".bin"] *)
val local_binaries :
  t -> dir:Path.Build.t -> File_binding.Expanded.t list Memo.Build.t

(** odoc config in the corresponding [(env)] stanza. *)
val odoc : t -> dir:Path.Build.t -> Env_node.Odoc.t Memo.Build.t

(** coq config in the corresponding [(env)] stanza. *)
val coq : t -> dir:Path.Build.t -> Env_node.Coq.t Action_builder.t Memo.Build.t

(** Formatting settings in the corresponding [(env)] stanza. *)
val format_config : t -> dir:Path.Build.t -> Format_config.t Memo.Build.t

(** Dump a directory environment in a readable form *)
val dump_env : t -> dir:Path.Build.t -> Dune_lang.t list Action_builder.t

val find_scope_by_dir : t -> Path.Build.t -> Scope.t

val find_scope_by_project : t -> Dune_project.t -> Scope.t

val find_project_by_key : t -> Dune_project.File_key.t -> Dune_project.t

val add_rule :
     t
  -> ?sandbox:Sandbox_config.t
  -> ?mode:Rule.Mode.t
  -> ?locks:Path.t list
  -> ?loc:Loc.t
  -> dir:Path.Build.t
  -> Action.t Action_builder.With_targets.t
  -> unit Memo.Build.t

val add_rule_get_targets :
     t
  -> ?sandbox:Sandbox_config.t
  -> ?mode:Rule.Mode.t
  -> ?locks:Path.t list
  -> ?loc:Loc.t
  -> dir:Path.Build.t
  -> Action.t Action_builder.With_targets.t
  -> Path.Build.Set.t Memo.Build.t

val add_rules :
     t
  -> ?sandbox:Sandbox_config.t
  -> dir:Path.Build.t
  -> Action.t Action_builder.With_targets.t list
  -> unit Memo.Build.t

val add_alias_action :
     t
  -> Alias.t
  -> dir:Path.Build.t
  -> loc:Loc.t option
  -> ?locks:Path.t list
  -> Action.t Action_builder.t
  -> unit Memo.Build.t

(** [resolve_program t ?hint name] resolves a program. [name] is looked up in
    the workspace, if it is not found in the tree is is looked up in the PATH.
    If it is not found at all, the resulting [Action.Prog.t] will either return
    the resolved path or a record with details about the error and possibly a
    hint.

    [hint] should tell the user what to install when the program is not found. *)
val resolve_program :
     t
  -> dir:Path.Build.t
  -> ?hint:string
  -> loc:Loc.t option
  -> string
  -> Action.Prog.t Memo.Build.t

val expander : t -> dir:Path.Build.t -> Expander.t Memo.Build.t

val dir_status_db : t -> Dir_status.DB.t

module As_memo_key : sig
  type nonrec t = t

  val to_dyn : t -> Dyn.t

  val equal : t -> t -> bool

  val hash : t -> int
end
