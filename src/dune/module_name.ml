open Stdune

include Stringlike.Make (struct
  type t = string

  let to_string s = s

  let description = "module name"

  let module_ = "Module_name"

  let description_of_valid_string = None

  let of_string_opt s =
    if Lib_name.is_valid_module_name s then
      Some (String.capitalize s)
    else
      None
end)

let compare = String.compare

let add_suffix = ( ^ )

let uncapitalize = String.uncapitalize

let pp_quote fmt x = Format.fprintf fmt "%S" x

module Set = struct
  include String.Set

  let to_dyn t = Dyn.Set (List.map ~f:(fun s -> Dyn.String s) (to_list t))
end

module Map = String.Map
module Infix = Comparator.Operators (String)

let of_local_lib_name s = of_string (Lib_name.Local.to_string s)

let to_local_lib_name s = Lib_name.Local.of_string s

module Per_item = Per_item.Make (String)

let of_string_allow_invalid (_loc, s) =
  (* TODO add a warning here that is possible to disable *)
  String.capitalize s

module Unique = struct
  module T = struct
    type nonrec t = string

    let compare = compare
  end

  include T

  (* We make sure that obj's start with a lowercase letter to make it harder to
     confuse them with a proper module name *)
  let of_name_assuming_needs_no_mangling name = String.uncapitalize_ascii name

  let to_name t ~loc =
    match of_string_opt t with
    | Some t -> t
    | None ->
      User_error.raise ~loc
        [ Pp.textf "%s corresponds to an invalid module name" t ]

  let encode = Dune_lang.Encoder.string

  let of_string s = of_name_assuming_needs_no_mangling (of_string s)

  let decode =
    let open Dune_lang.Decoder in
    let+ s = Dune_lang.Decoder.string in
    of_string s

  let of_path_assuming_needs_no_mangling_allow_invalid path =
    let fn = Path.basename path in
    let loc = Loc.in_file path in
    let name =
      match String.index fn '.' with
      | None -> fn
      | Some i -> String.take fn i
    in
    of_name_assuming_needs_no_mangling (of_string_allow_invalid (loc, name))

  let to_dyn = to_dyn

  let artifact_filename (t : t) ~ext = t ^ ext

  module Map = Map
  module Set = Set
end

let wrap t ~with_ =
  sprintf "%s__%s" (Unique.of_name_assuming_needs_no_mangling with_) t
