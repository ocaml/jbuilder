open Stdune

let int_of_string ?where s =
  match Int.of_string s with
  | Some s -> Ok s
  | None ->
    Result.Error
      (Printf.sprintf "invalid integer%s: %s"
         (match where with
         | Some l -> " in " ^ l
         | None -> "")
         s)

let int64_of_string ?where s =
  match Int64.of_string s with
  | res -> Ok res
  | exception _exn ->
    Result.Error
      (Printf.sprintf "invalid 64-bit integer%s: %s"
         (match where with
         | Some l -> " in " ^ l
         | None -> "")
         s)
