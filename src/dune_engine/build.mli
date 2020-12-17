(** The build description *)

open! Stdune
open! Import

(** Type of values allowed to be labeled *)
type label = ..

type 'a t

include Applicative_intf.S1 with type 'a t := 'a t

module With_targets : sig
  type 'a build

  type nonrec 'a t =
    { build : 'a t
    ; targets : Path.Build.Set.t
    }

  val map_build : 'a t -> f:('a build -> 'b build) -> 'b t

  val return : 'a -> 'a t

  val add : 'a t -> targets:Path.Build.t list -> 'a t

  val map : 'a t -> f:('a -> 'b) -> 'b t

  val map2 : 'a t -> 'b t -> f:('a -> 'b -> 'c) -> 'c t

  val write_file_dyn : Path.Build.t -> string t -> Action.t t

  val all : 'a t list -> 'a list t

  val of_result_map :
    'a Or_exn.t -> f:('a -> 'b t) -> targets:Path.Build.t list -> 'b t

  (** [memoize name t] is a build description that behaves like [t] except that
      its result is computed only once. *)
  val memoize : string -> 'a t -> 'a t

  module O : sig
    val ( >>> ) : unit t -> 'a t -> 'a t

    val ( let+ ) : 'a t -> ('a -> 'b) -> 'b t

    val ( and+ ) : 'a t -> 'b t -> ('a * 'b) t
  end
end
with type 'a build := 'a t

(** This function should be called before analysing build expressions using
    [static_deps], [lib_deps] or [exec], which all require some file system
    information. *)
val set_file_system_accessors : file_exists:(Path.t -> bool) -> unit

(** Add a set of targets to a build description, turning a target-less [Build.t]
    into [Build.With_targets.t]. *)
val with_targets : 'a t -> targets:Path.Build.t list -> 'a With_targets.t

(** Create a value of [With_targets.t] with the empty set of targets. *)
val with_no_targets : 'a t -> 'a With_targets.t

val return : 'a -> 'a t

val map : 'a t -> f:('a -> 'b) -> 'b t

val map2 : 'a t -> 'b t -> f:('a -> 'b -> 'c) -> 'c t

val ignore : 'a t -> unit t

val all : 'a t list -> 'a list t

val all_unit : unit t list -> unit t

(** Delay a static computation until the description is evaluated *)
val delayed : (unit -> 'a) -> 'a t

val or_exn : 'a Or_exn.t t -> 'a t

(** CR-someday diml: this API is not great, what about:

    {[
      module Action_with_deps : sig
        type t
        val add_file_dependency : t -> Path.t -> t
      end

      (** Same as
          [t >>> arr (fun x -> Action_with_deps.add_file_dependency x p)]
          but better as [p] is statically known *)

      val record_dependency
        :  Path.t
        -> ('a, Action_with_deps.t) t
        -> ('a, Action_with_deps.t) t
    ]} *)

(** [path p] records [p] as a file that is read by the action produced by the
    build description. *)
val path : Path.t -> unit t

val dep : Dep.t -> unit t

val deps : Dep.Set.t -> unit t

val dyn_deps : ('a * Dep.Set.t) t -> 'a t

val paths : Path.t list -> unit t

val path_set : Path.Set.t -> unit t

(** Evaluate a predicate against all targets and record all the matched files as
    dependencies of the action produced by the build description. *)
val paths_matching : loc:Loc.t -> File_selector.t -> Path.Set.t t

(** [paths_existing paths] will require as dependencies the files that actually
    exist. *)
val paths_existing : Path.t list -> unit t

(** [env_var v] records [v] as an environment variable that is read by the
    action produced by the build description. *)
val env_var : string -> unit t

val alias : Alias.t -> unit t

(** Compute the set of source of all files present in the sub-tree starting at
    [dir] and record them as dependencies. *)
val source_tree : dir:Path.t -> Path.Set.t t

(** Record dynamic dependencies *)
val dyn_paths : ('a * Path.t list) t -> 'a t

val dyn_paths_unit : Path.t list t -> unit t

val dyn_path_set : ('a * Path.Set.t) t -> 'a t

val dyn_path_set_reuse : Path.Set.t t -> Path.Set.t t

(** [catch t ~on_error] evaluates to [on_error exn] if exception [exn] is raised
    during the evaluation of [t]. *)
val catch : 'a t -> on_error:(exn -> 'a) -> 'a t

(** [contents path] returns a description that when run will return the contents
    of the file at [path]. Files are opened in binary mode by default. *)
val contents : ?binary:bool -> Path.t -> string t

(** [lines_of path] returns a description that when run will return the contents
    of the file at [path] as a list of lines. *)
val lines_of : Path.t -> string list t

(** [strings path] is like [lines_of path] except each line is unescaped using
    the OCaml conventions. *)
val strings : Path.t -> string list t

(** Load an S-expression from a file *)
val read_sexp : Path.t -> Dune_lang.Ast.t t

(** Evaluates to [true] if the file is present on the file system or is the
    target of a rule. It doesn't add the path as dependency *)
val file_exists : Path.t -> bool t

(** [if_file_exists p ~then ~else] is a description that behaves like [then_] if
    [file_exists p] evaluates to [true], and [else_] otherwise. *)
val if_file_exists : Path.t -> then_:'a t -> else_:'a t -> 'a t

(** [filter_existing_files p] is a build description which keep only the
    existing files. The files are not registered as dynamic dependencies. *)
val filter_existing_files : ('a * Path.Set.t) t -> ('a * Path.Set.t) t

(** Always fail when executed. We pass a function rather than an exception to
    get a proper backtrace *)
val fail : fail -> _ t

val of_result : 'a t Or_exn.t -> 'a t

val of_result_map : 'a Or_exn.t -> f:('a -> 'b t) -> 'b t

(** [memoize name t] is a build description that behaves like [t] except that
    its result is computed only once. *)
val memoize : string -> 'a t -> 'a t

(** Create a file with the given contents. Files are written in binary mode by
    default. *)
val write_file :
  ?binary:bool -> Path.Build.t -> string -> Action.t With_targets.t

val write_file_dyn :
  ?binary:bool t -> Path.Build.t -> string t -> Action.t With_targets.t

val copy : src:Path.t -> dst:Path.Build.t -> Action.t With_targets.t

val copy_and_add_line_directive :
  src:Path.t -> dst:Path.Build.t -> Action.t With_targets.t

val symlink : src:Path.t -> dst:Path.Build.t -> Action.t With_targets.t

val create_file : Path.Build.t -> Action.t With_targets.t

(** Merge a list of actions accumulating the sets of their targets. *)
val progn : Action.t With_targets.t list -> Action.t With_targets.t

val label : label -> unit t

(** {1 Analysis} *)

(** Compute static dependencies of a build description. *)
val static_deps : _ t -> Static_deps.t

(** Compute static library dependencies of a build description. *)
val fold_labeled : _ t -> init:'acc -> f:(label -> 'acc -> 'acc) -> 'acc

(** {1 Execution} *)

(** Execute a build description. Returns the result and the set of dynamic
    dependencies discovered during execution. *)
val exec : 'a t -> 'a * Dep.Set.t

(**/**)

val paths_for_rule : Path.Set.t -> unit t
