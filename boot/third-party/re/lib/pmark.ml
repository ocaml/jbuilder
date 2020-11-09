
module Pmark = struct
  type t = int
  let equal (x : int) (y : int) = x = y
  let compare (x : int) (y : int) = compare x y
  let r = ref 0
  let gen () = incr r ; !r

  let pp = Format.pp_print_int
end

include Pmark
module Set = Set.Make(Pmark)
