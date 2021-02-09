open Import

module T =
  Interned.Make
    (struct
      let initial_size = 16

      let resize_policy = Interned.Conservative

      let order = Interned.Natural
    end)
    ()

include T

let default = make "default"

let build_dir t = Path.Build.relative Path.Build.root (to_string t)

let is_default = equal default

include (
  Stringlike.Make (struct
    type t = T.t

    let description_of_valid_string = None

    let hint_valid = None

    let to_string = T.to_string

    let module_ = "Context_name"

    let description = "context name"

    let of_string_opt name =
      if
        name = ""
        || String.is_prefix name ~prefix:"."
        || name = "log" || name = "install" || String.contains name '/'
        || String.contains name '\\'
      then
        None
      else
        Some (make name)
  end) :
    Stringlike_intf.S with type t := t)

let target t ~toolchain =
  make (sprintf "%s.%s" (to_string t) (to_string toolchain))

module Infix = Comparator.Operators (T)
module Top_closure = Top_closure.Make (Set) (Monad.Id)
