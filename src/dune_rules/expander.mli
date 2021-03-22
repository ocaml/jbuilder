(** An expander is able to expand any dune template. It has two modes of
    expansion:

    1. Static. In this mode it will only expand variables that do not introduce
    dependencies

    2. Dynamic. In this mode, the expander will record dependencies that are
    introduced by forms it has failed to expand. Later, these dependenceis can
    be filled for a full expansion.*)

open! Dune_engine
open Import

type t

val scope : t -> Scope.t

val dir : t -> Path.Build.t

val context : t -> Context.t

(** local or installed package *)
type any_package =
  | Local of Package.t
  | Installed of Dune_package.t

val make :
     scope:Scope.t
  -> scope_host:Scope.t
  -> context:Context.t
  -> lib_artifacts:Artifacts.Public_libs.t
  -> lib_artifacts_host:Artifacts.Public_libs.t
  -> bin_artifacts_host:Artifacts.Bin.t
  -> find_package:(Package.Name.t -> any_package option)
  -> t

val set_foreign_flags :
     t
  -> f:
       (   dir:Path.Build.t
        -> string list Action_builder.t Foreign_language.Dict.t Memo.Build.t)
  -> t

val set_local_env_var : t -> var:string -> value:string Action_builder.t -> t

val set_dir : t -> dir:Path.Build.t -> t

val set_scope : t -> scope:Scope.t -> t

val set_bin_artifacts : t -> bin_artifacts_host:Artifacts.Bin.t -> t

val set_artifacts_dynamic : t -> bool -> t

val set_lookup_ml_sources :
  t -> f:(dir:Path.Build.t -> Ml_sources.Artifacts.t Memo.Build.t) -> t

module Expanding_what : sig
  type t =
    | Nothing_special
    | Deps_like_field
    | User_action of Targets.Or_forbidden.t
end

(** Used to improve error messages and handing special cases, such as:
    [%{exe:fn}] maps [fn] to the host context except when expanding a deps-like
    field. *)
val set_expanding_what : t -> Expanding_what.t -> t

(** Expander needs to expand custom bindings sometimes. For example, the name of
    the library for the action that runs inline tests. This is the place to add
    such bindings. *)
val add_bindings : t -> bindings:Value.t list Pform.Map.t -> t

val add_bindings_full :
  t -> bindings:Value.t list Action_builder.t Pform.Map.t -> t

val extend_env : t -> env:Env.t -> t

val expand :
     t
  -> mode:'a String_with_vars.Mode.t
  -> String_with_vars.t
  -> 'a Action_builder.t

val expand_path : t -> String_with_vars.t -> Path.t Action_builder.t

val expand_str : t -> String_with_vars.t -> string Action_builder.t

val expand_pform : t -> Value.t list Action_builder.t String_with_vars.expander

module Static : sig
  val expand : t -> mode:'a String_with_vars.Mode.t -> String_with_vars.t -> 'a

  val expand_path : t -> String_with_vars.t -> Path.t

  val expand_str : t -> String_with_vars.t -> string

  val expand_pform : t -> Value.t list String_with_vars.expander

  module With_reduced_var_set : sig
    val expand_path :
      context:Context.t -> dir:Path.Build.t -> String_with_vars.t -> Path.t

    val expand_str :
      context:Context.t -> dir:Path.Build.t -> String_with_vars.t -> string

    val expand_str_partial :
         context:Context.t
      -> dir:Path.Build.t
      -> String_with_vars.t
      -> String_with_vars.t
  end

  module Or_exn : sig
    val expand_path : t -> String_with_vars.t -> Path.t Or_exn.t

    val expand_str : t -> String_with_vars.t -> string Or_exn.t
  end
end

(** Expand forms of the form (:standard \ foo bar). Expansion is only possible
    inside [Action_builder.t] because such forms may contain the form (:include
    ..) which needs files to be built. *)
val expand_and_eval_set :
     t
  -> Ordered_set_lang.Unexpanded.t
  -> standard:string list Action_builder.t
  -> string list Action_builder.t

val eval_blang : t -> Blang.t -> bool

val eval_blang_partial : t -> Blang.t -> bool Action_builder.t

val map_exe : t -> Path.t -> Path.t

val artifacts : t -> Artifacts.Bin.t

val find_package : t -> Package.Name.t -> any_package option
