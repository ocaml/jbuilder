module V1 = struct
  module Version = struct
    type t = string

    let to_string x = x
  end

  module Statically_linked_library = struct
    type t = string * string option

    let name = fst

    let version = snd
  end

  module Statically_linked_libraries = struct
    let to_list () = Build_info_data.statically_linked_libraries

    let find ~name =
      match List.assoc name (to_list ()) with
      | exception Not_found -> None
      | version -> Some (name, version)
  end

  let version () = Build_info_data.version
end

module V2 = struct
  include V1

  module Custom = struct
    type t = string

    let to_string x = x
  end

  let custom () = Build_info_data.custom

  let custom_lib lib = Build_info_data.custom_lib lib
end
