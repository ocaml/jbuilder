open! Dune_engine
open! Stdune
open! Import

module type Info = Sub_system_info.S

module type S = sig
  module Info : Info

  (** Instantiated representation of the sub-system. I.e. with names resolved
      using a library database. *)
  type t

  (** Create an instance of the sub-system *)
  val instantiate :
       resolve:(Loc.t * Lib_name.t -> Lib.t Resolve.Build.t)
    -> get:(loc:Loc.t -> Lib.t -> t option Memo.Build.t)
    -> Lib.t
    -> Info.t
    -> t Memo.Build.t
end

(** A backend, for implementors of the sub-system *)
module type Backend = sig
  include S

  (** Description of a backend, such as "inline tests framework" or "ppx
      driver". *)
  val desc : plural:bool -> string

  (** "a" or "an" *)
  val desc_article : string

  (** Library the backend is attached to *)
  val lib : t -> Lib.t

  (** Return the processed information. This is what is serialised in
      [dune-package] files. Typically, it should be the original info with the
      private library names replaced by public ones. *)
  val public_info : t -> Info.t Resolve.Build.t
end

module type Registered_backend = sig
  type t

  val get : Lib.t -> t option Memo.Build.t

  (** Resolve a backend name *)
  val resolve : Lib.DB.t -> Loc.t * Lib_name.t -> t Resolve.Build.t

  module Selection_error : sig
    type nonrec t =
      | Too_many_backends of t list
      | No_backend_found

    val to_lib_resolve : ('a, t) result -> loc:Loc.t -> 'a Resolve.t
  end

  (** Choose a backend by either using the ones written by the user or by
      scanning the dependencies.

      The returned list is sorted by order of dependencies. It is not allowed to
      have two different backend that are completely independent, i.e. none of
      them is in the transitive closure of the other one. *)
  val select_extensible_backends :
       ?written_by_user:t list
    -> extends:(t -> t list Resolve.t)
    -> Lib.t list
    -> (t list, Selection_error.t) result Resolve.Build.t

  (** Choose a backend by either using the ones written by the user or by
      scanning the dependencies.

      A backend can replace other backends *)
  val select_replaceable_backend :
       ?written_by_user:t list
    -> replaces:(t -> t list Resolve.t)
    -> Lib.t list
    -> (t, Selection_error.t) result Resolve.t Memo.Build.t
end

(* This is probably what we'll give to plugins *)
module Library_compilation_context = struct
  type t =
    { super_context : Super_context.t
    ; dir : Path.Build.t
    ; stanza : Dune_file.Library.t
    ; scope : Scope.t
    ; source_modules : Module.t list
    ; compile_info : Lib.Compile.t
    }
end

(** An end-point, for users of the systems *)
module type End_point = sig
  module Backend : sig
    include Registered_backend

    (** Backends that this backends extends *)
    val extends : t -> t list Resolve.t
  end

  module Info : sig
    include Info

    (** Additional backends specified by the user at use-site *)
    val backends : t -> (Loc.t * Lib_name.t) list option
  end

  val gen_rules :
       Library_compilation_context.t
    -> info:Info.t
    -> backends:Backend.t list
    -> unit Memo.Build.t
end
