(** META file parsing/printing *)

open! Stdune
open! Dune_engine
open! Dune_engine.Import

type t =
  { name : Lib_name.t option
  ; entries : entry list
  }

and entry =
  | Comment of string
  | Rule of rule
  | Package of t

and rule =
  { var : string
  ; predicates : predicate list
  ; action : action
  ; value : string
  }

and action =
  | Set
  | Add

and predicate =
  | Pos of string
  | Neg of string

val to_dyn : t -> Dyn.t

val filter_variable : t -> f:(string -> bool) -> t

val parse_entries : Lexing.lexbuf -> entry list

(** Add version fields to all package in [t] that don't have and have at least
    one rule. [get_version] is used to obtain the version. It receives as
    argument the package path. *)
val add_versions : t -> get_version:(Lib_name.t list -> string option) -> t

module Simplified : sig
  module Rules : sig
    type t =
      { set_rules : rule list
      ; add_rules : rule list
      }
  end

  type t =
    { name : Lib_name.t option
    ; vars : Rules.t String.Map.t
    ; subs : t list
    }

  val equal : t -> t -> bool

  val hash : t -> int

  val to_dyn : t -> Dyn.t
end

val complexify : Simplified.t -> t

val of_string : string -> name:Package.Name.t option -> Simplified.t

val load : Path.t -> name:Package.Name.t option -> Simplified.t Memo.Build.t

(** Builtin META files for libraries distributed with the compiler. For when
    ocamlfind is not installed. *)
val builtins :
     stdlib_dir:Path.t
  -> version:Ocaml_version.t
  -> Simplified.t Package.Name.Map.t

val pp : entry list -> unit Pp.t
