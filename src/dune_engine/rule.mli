(** Representation of rules *)

open! Stdune
open! Import
module Action_builder := Action_builder0

module Info : sig
  type t =
    | From_dune_file of Loc.t
    | Internal
    | Source_file_copy of Path.Source.t

  val of_loc_opt : Loc.t option -> t
end

module Promote : sig
  module Lifetime : sig
    type t =
      | Unlimited  (** The promoted file will be deleted by [dune clean] *)
      | Until_clean
  end

  module Into : sig
    type t =
      { loc : Loc.t
      ; dir : string
      }
  end

  type t =
    { lifetime : Lifetime.t
    ; into : Into.t option
    ; only : Predicate_lang.Glob.t option
    }
end

module Mode : sig
  type t =
    | Standard
    | Fallback  (** Only use this rule if the source files don't exist. *)
    | Promote of Promote.t
        (** Silently promote the targets to the source tree. *)
    | Ignore_source_files
        (** Just ignore the source files entirely. This is for cases where the
            targets are promoted only in a specific context, such as for
            .install files. *)
end

type t = private
  { context : Build_context.t option
  ; targets : Path.Build.Set.t
  ; action : Action.Full.t Action_builder.t
  ; mode : Mode.t
  ; info : Info.t
  ; loc : Loc.t
  ; (* Directory where all the targets are produced. *) dir : Path.Build.t
  }

val to_dyn : t -> Dyn.t

val make :
     ?sandbox:Sandbox_config.t
  -> ?mode:Mode.t
  -> context:Build_context.t option
  -> ?info:Info.t
  -> targets:Path.Build.Set.t
  -> Action.Full.t Action_builder.t
  -> t

val set_action : t -> Action.Full.t Action_builder.t -> t

val loc : t -> Loc.t

(** [find_source_dir rule] is the closest source directory corresponding to
    rule.dir. Eg. [src/dune] for a rule with dir
    [_build/default/src/dune/.dune.objs]. *)
val find_source_dir : t -> Source_tree.Dir.t Memo.Build.t

val head_target : t -> Path.Build.t

module Anonymous_action : sig
  (* jeremiedimino: this type correspond to a subset of [Rule.t]. We should
     eventually share the code. *)
  type t =
    { context : Build_context.t option
    ; action : Action.Full.t
    ; loc : Loc.t option
    ; dir : Path.Build.t
          (** Directory the action is attached to. This is the directory where
              the outcome of the action will be cached. *)
    ; alias : Alias.Name.t option  (** For better error messages *)
    }
end
