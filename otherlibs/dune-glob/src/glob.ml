open Stdune

type t =
  | Re of
      { re : Re.re
      ; repr : string
      }
  | Literal of string

let test t s =
  match t with
  | Literal t -> String.equal t s
  | Re { re; repr = _ } -> Re.execp re s

let empty = Re { re = Re.compile Re.empty; repr = "\000" }

let universal = Re { re = Re.compile (Re.rep Re.any); repr = "**" }

let of_string_result repr =
  Lexer.parse_string repr
  |> Result.map ~f:(function
       | Lexer.Literal s -> Literal s
       | Re re -> Re { re = Re.compile re; repr })

let of_string repr =
  match of_string_result repr with
  | Error (_, msg) -> invalid_arg (Printf.sprintf "invalid glob: :%s" msg)
  | Ok t -> t

let to_string t =
  match t with
  | Re { repr; re = _ } -> repr
  | Literal s -> s
