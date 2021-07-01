open! Dune_engine
open! Stdune
open! Import

module Bin : sig
  type t

  (** A named artifact that is looked up in the PATH if not found in the tree If
      the name is an absolute path, it is used as it. *)
  val binary :
       t
    -> ?hint:string
    -> loc:Loc.t option
    -> string
    -> Action.Prog.t Memo.Build.t

  val add_binaries : t -> dir:Path.Build.t -> File_binding.Expanded.t list -> t

  val create : context:Context.t -> local_bins:Path.Build.Set.t -> t
end

module Public_libs : sig
  type t =
    { context : Context.t
    ; public_libs : Lib.DB.t
    }

  (** [file_of_lib t ~from ~lib ~file] returns the path to a file in the
      directory of the given library. *)
  val file_of_lib :
    t -> loc:Loc.t -> lib:Lib_name.t -> file:string -> Path.t Resolve.Build.t
end

type t = private
  { public_libs : Public_libs.t
  ; bin : Bin.t
  }

val create :
  Context.t -> public_libs:Lib.DB.t -> local_bins:Path.Build.Set.t -> t
