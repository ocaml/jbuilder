open StdLabels

module Sexp = struct
  type t =
    | Atom of string
    | List of t list
end

module Csexp = Csexp.Make (Sexp)

let atom = Sexp.Atom (String.make 128 'x')

let rec gen_sexp depth =
  if depth = 0 then
    atom
  else
    let x = gen_sexp (depth - 1) in
    List [ x; x ]

let s = Sys.opaque_identity (Csexp.to_string (gen_sexp 16))

let%bench "of_string" = ignore (Csexp.parse_string s : _ result)
