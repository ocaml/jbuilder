(** Rule aliases. *)

open Import


type t

val pp : t Fmt.t

val make : string -> dir:Path.t -> t

val of_path : Path.t -> t

(** The following always holds:

    {[
      make (name t) ~dir:(dir t) = t
    ]}
*)
val name : t -> string
val dir  : t -> Path.t

val fully_qualified_name : t -> Path.t

val default : dir:Path.t -> t
val runtest : dir:Path.t -> t
val install : dir:Path.t -> t
val doc     : dir:Path.t -> t
val lint    : dir:Path.t -> t
val bench   : dir:Path.t -> t

val dep : t -> ('a, 'a) Build.t

(** Implements [(alias_rec ...)] in dependency specification and
    [@alias] on the command line. *)
val dep_rec : loc:Loc.t -> file_tree:File_tree.t -> t -> (unit, unit) Build.t

(** File that represent the alias in the filesystem. It is a file under
    [_build/.aliases]. *)
val file : t -> Path.t

(** Same as [file t], except that it sets the digest suffix to [digest]. Files
    representing aliases ends with a hex-encoded md5sum of some data. It is usually filled
    with zeros except for files that represent the running of an action associated to an
    alias, it which case it is the md5 checksum of the action and its dependencies. *)
val file_with_digest_suffix : t -> digest:Digest.t -> Path.t

(** The following holds for any path [p]:

    {[
      match of_file p with
      | None -> true
      | Some t -> p = file t
    ]}
*)
val of_file : Path.t -> t option

(** Same as [Option.map (of_file p) ~f:name] but more efficient. *)
val name_of_file : Path.t -> string option

module Store : sig
  type t

  val pp : t Fmt.t

  val create : unit -> t

  val unlink : t -> string list -> unit
end

(** [add_build store alias deps] arrange things so that all [deps] are built as part of
    the build of alias [alias]. *)
val add_deps : Store.t -> t -> Path.t list -> unit

(** [add_build store alias ~stamp build] arrange things so that [build] is part of the
    build of alias [alias]. [stamp] is any S-expression that is unique and persistent
    S-expression.

    Return a rule that must be added with [Super_context.add_rule].
*)
val add_build
  :  Store.t
  -> t
  -> stamp:Sexp.t
  -> (unit, Action.t) Build.t
  -> (unit, Action.t) Build.t

(** Same as calling [add_build] in a loop but slightly more efficient. *)
val add_builds
  :  Store.t
  -> t
  -> (Sexp.t * (unit, Action.t) Build.t) list
  -> (unit, Action.t) Build.t list

val rules : Store.t -> Build_interpret.Rule.t list

