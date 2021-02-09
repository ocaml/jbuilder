open Import

module Vars = struct
  type t = string String.Map.t

  let of_lines lines =
    let rec loop acc = function
      | [] -> Ok acc
      | line :: lines -> (
        match String.index line ':' with
        | Some i ->
          let x =
            ( String.take line i
            , String.drop line (i + 2) (* skipping the space *) )
          in
          loop (x :: acc) lines
        | None -> Error (Printf.sprintf "Unrecognized line: %S" line))
    in
    match loop [] lines with
    | Error _ as e -> e
    | Ok s -> (
      match String.Map.of_list s with
      | Ok _ as s -> s
      | Error (var, _, _) -> Error (sprintf "Variable %S present twice." var))

  let of_list_exn = String.Map.of_list_exn

  let find t x = String.Map.find t x
end
