
type t = int
let equal (x : int) (y : int) = x = y
let compare (x : int) (y : int) = compare x y
let to_int x = x
let pp = Format.pp_print_int

let intersect x y = x land y <> 0
let (++) x y = x lor y

let dummy = -1
let inexistant = 1
let letter = 2
let not_letter = 4
let newline = 8
let lastnewline = 16
let search_boundary = 32

let from_char = function
  (* Should match [cword] definition *)
  | 'a'..'z' | 'A'..'Z' | '0'..'9' | '_' | '\170' | '\181' | '\186'
  | '\192'..'\214' | '\216'..'\246' | '\248'..'\255' ->
    letter
  | '\n' ->
    not_letter ++ newline
  | _ ->
    not_letter
