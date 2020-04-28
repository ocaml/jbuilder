open Stdune

let is_valid_module_name ?(with_hyphen=false) name =
  match name with
  | "" -> false
  | s -> (
      try
        ( match s.[0] with
          | 'A' .. 'Z'
          | 'a' .. 'z' ->
            ()
          | _ -> raise_notrace Exit );
        String.iter s ~f:(function
          | 'A' .. 'Z'
          | 'a' .. 'z'
          | '0' .. '9'
          | '\''
          | '_' ->
            ()
          | '-' when with_hyphen -> ()
          | _ -> raise_notrace Exit);
        true
      with Exit -> false )

module Local = struct
  type t = string

  let valid_format_doc =
    Pp.text
      "Library names must be non-empty and composed only of the following \
       characters: 'A'..'Z', 'a'..'z', '_' or '0'..'9'."

  include (
    Stringlike.Make (struct
      type t = string

      let to_string s = s

      let module_ = "Lib_name.Local"

      let description = "library name"

      let description_of_valid_string = Some valid_format_doc

      let valid_char = function
        | 'A' .. 'Z'
        | 'a' .. 'z'
        | '_'
        | '0' .. '9' ->
          true
        | _ -> false

      let of_string_opt (name : string) =
        match name with
        | "" -> None
        | (s : string) ->
          if s.[0] = '.' then
            None
          else
            let len = String.length s in
            let rec loop warn i =
              if i = len - 1 then
                if warn then
                  None
                else
                  Some s
              else
                let c = String.unsafe_get s i in
                if valid_char c then
                  loop warn (i + 1)
                else if c = '.' then
                  loop true (i + 1)
                else
                  None
            in
            loop false 0
    end) :
      Stringlike_intf.S with type t := t )
end

let split t =
  match String.split t ~on:'.' with
  | [] -> assert false
  | pkg :: rest -> (Package.Name.of_string pkg, rest)

let to_local = Local.of_string_opt

include Stringlike.Make (struct
  type nonrec t = string

  let description_of_valid_string = None

  let to_string s = s

  let module_ = "Lib_name"

  let description = "library name"

  let of_string_opt s =
    let l = String.split s ~on:'.' in
    Option.some_if
      (List.for_all l ~f:(is_valid_module_name ~with_hyphen:true))
      s
end)

let of_local (_loc, t) = t

let of_package_name p = Package.Name.to_string p

let hash = String.hash

let compare = String.compare

include (Comparator.Operators (String) : Comparator.OPS with type t := t)

module O = Comparable.Make (String)
module Map = O.Map

module Set = struct
  include O.Set

  let to_string_list = to_list
end

let package_name t =
  Package.Name.of_string
    ( match String.lsplit2 t ~on:'.' with
    | None -> t
    | Some (p, _) -> p )

let nest x y = sprintf "%s.%s" x y
