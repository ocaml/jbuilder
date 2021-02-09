module Log = Dune_util.Log
open Stdune
module Key = Key
include Cache_intf

let promotion_to_string = function
  | Already_promoted { path; digest } ->
    Printf.sprintf "%s already promoted with digest %s"
      (Path.Local.to_string (Path.Build.local path))
      (Digest.to_string digest)
  | Promoted { path; digest } ->
    Printf.sprintf "%s promoted with digest %s"
      (Path.Local.to_string (Path.Build.local path))
      (Digest.to_string digest)

let make_caching (type t) (module Caching : Cache with type t = t) (cache : t) :
    (module Caching) =
  (module struct
    module Cache = Caching

    let cache = cache
  end)

let cachable = function
  | Unix.S_REG -> true
  | _ -> false

module Client = Client
module Local = Local
module Messages = Messages
