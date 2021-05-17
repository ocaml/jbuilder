module Meta_parser = Dune_meta_parser.Meta_parser.Make (struct
  module Loc = struct
    type t =
      { start : Lexing.position
      ; stop : Lexing.position
      }

    let of_lexbuf lexbuf : t =
      { start = Lexing.lexeme_start_p lexbuf
      ; stop = Lexing.lexeme_end_p lexbuf
      }
  end

  module Lib_name = struct
    type t = string

    let parse_string_exn (_, n) = n
  end

  module Pp = struct
    type 'tag t = string

    let text s = s
  end

  module User_message = struct
    module Style = struct
      type t = unit
    end
  end

  module User_error = struct
    module Annotations = struct
      type t = unit
    end

    let raise ?loc:_ ?hints:_ ?annots:_ texts =
      invalid_arg (String.concat " " texts)
  end
end)

include Meta_parser
