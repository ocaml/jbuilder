open! Dune_engine
open Stdune

(** A directory with a [dune] file *)
type 'data t =
  { src_dir : Path.Source.t
  ; ctx_dir : Path.Build.t  (** [_build/context-name/src_dir] *)
  ; data : 'data
  ; scope : Scope.t
  ; dune_version : Dune_lang.Syntax.Version.t
  }

val data : 'data t -> 'data

val map : 'a t -> f:('a -> 'b) -> 'b t

(** [deep_fold dwd ~init ~f] fold the most inner elements but still gives the
    corresponding directory information corresponding as first argument *)
val deep_fold :
  'a list t list -> init:'acc -> f:('a list t -> 'a -> 'acc -> 'acc) -> 'acc

module Memo : sig
  val deep_fold :
       'a list t list
    -> init:'acc
    -> f:('a list t -> 'a -> 'acc -> 'acc Memo.Build.t)
    -> 'acc Memo.Build.t
end
