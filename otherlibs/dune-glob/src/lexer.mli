open Stdune

type t =
  | Literal of string
  | Re of Re.t

val parse_string : string -> (t, int * string) Result.result
