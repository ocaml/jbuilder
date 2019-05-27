open! Stdune
(** This module replaces arg_spec.mli in the post-arrow Dune. The main
    change is the constructor [Dyn] which now takes [static t Build.s]
    instead of ['a -> (unit, static) t], which allows us to get rid of
    the first type parameter ['a] in [('a, 'b) Arg_spec.t]. *)

(** Command line arguments specification *)

(** This module implements a small DSL to specify the command line
    argument of a program as well as the dependencies and targets of
    the program at the same time.

    For instance to represent the argument of [ocamlc -o src/foo.exe
    src/foo.ml], one might write:

    {[
      [ A "-o"
      ; Target (Path.relatie  dir "foo.exe")
      ; Dep    (Path.relative dir "foo.ml")
      ]
    ]}

    This DSL was inspired from the ocamlbuild API.  *)

open! Import

(** [A] stands for "atom", it is for command line arguments that are
    neither dependencies nor targets.

    [Path] is similar to [A] in the sense that it defines a command
    line argument that is neither a dependency or target. However, the
    difference between the two is that [A s] produces exactly the
    argument [s], while [Path p] produces a string that depends on
    where the command is executed. For instance [Path (Path.of_string
    "src/foo.ml")] will translate to "../src/foo.ml" if the command is
    started from the "test" directory.  *)

module Args : sig
  type static = Static
  type dynamic = Dynamic

  type _ t =
    | A        : string -> _ t
    | As       : string list -> _ t
    | S        : 'a t list -> 'a t
    | Concat   : string * 'a t list  -> 'a t
    | Dep      : Path.t -> _ t
    | Deps     : Path.t list -> _ t
    | Target   : Path.t -> dynamic t
    | Path     : Path.t -> _ t
    | Paths    : Path.t list -> _ t
    | Hidden_deps    : Dep.Set.t -> _ t
    | Hidden_targets : Path.t list -> dynamic t
    | Dyn      : static t Build.s -> dynamic t
    | Fail     : fail -> _ t

  (* Create a dynamic command line arguments. *)
  val dyn : string list Build.s -> dynamic t
end

(* TODO: Using list in [dynamic t list] complicates the API unnecessarily: we
can use the constructor [S] to concatenate lists instead. *)
val run
  :  dir:Path.t
  -> ?stdout_to:Path.t
  -> Action.Prog.t
  -> Args.dynamic Args.t list
  -> Action.t Build.s

(** [quote_args quote args] is [As \[quote; arg1; quote; arg2; ...\]] *)
val quote_args : string -> string list -> _ Args.t

val of_result : 'a Args.t Or_exn.t -> 'a Args.t
val of_result_map : 'a Or_exn.t -> f:('a -> 'b Args.t) -> 'b Args.t
val fail : exn -> 'a Args.t
