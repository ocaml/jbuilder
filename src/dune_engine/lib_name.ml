open Import

let private_key = "__private__"

module Local = struct
  type t = string

  let valid_format_doc =
    Pp.text
      "Library names must be non-empty and composed only of the following \
       characters: 'A'..'Z', 'a'..'z', '_' or '0'..'9'."

  include (
    Stringlike.Make (struct
      type t = string

      let valid_char = function
        | 'A' .. 'Z'
        | 'a' .. 'z'
        | '_'
        | '0' .. '9' ->
          true
        | _ -> false

      let to_string s = s

      let module_ = "Lib_name.Local"

      let description = "library name"

      let description_of_valid_string = Some valid_format_doc

      let hint_valid =
        Some
          (fun name ->
            String.filter_map name ~f:(fun c ->
                if valid_char c then
                  Some c
                else
                  match c with
                  | '.'
                  | '-' ->
                    Some '_'
                  | _ -> None))

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
      Stringlike_intf.S with type t := t)

  let mangled_path_under_package local_name =
    [ private_key; to_string local_name ]
end

let split t =
  match String.split t ~on:'.' with
  | [] -> assert false
  | pkg :: rest -> (Package.Name.of_string pkg, rest)

let to_local = Local.of_string_user_error

let to_local_exn t =
  match Local.of_string_opt t with
  | Some s -> s
  | None ->
    Code_error.raise "invalid Lib_name.t -> Lib_name.Local.t conversion"
      [ ("t", String t) ]

include Stringlike.Make (struct
  type nonrec t = string

  let description_of_valid_string = None

  let hint_valid = None

  let to_string s = s

  let module_ = "Lib_name"

  let description = "library name"

  let of_string_opt name =
    match name with
    | "" -> None
    | s -> Option.some_if (s.[0] <> '.') s
end)

type analyze =
  | Public of Package.Name.t * string list
  | Private of Package.Name.t * Local.t

let analyze t =
  let pkg, rest = split t in
  match rest with
  | [ pkey; name ] when pkey = private_key -> Private (pkg, Local.of_string name)
  | _ -> Public (pkg, rest)

let mangled pkg local_name =
  let under_pkg = Local.mangled_path_under_package local_name in
  Package.Name.to_string pkg :: under_pkg |> String.concat ~sep:"." |> of_string

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
    (match String.lsplit2 t ~on:'.' with
    | None -> t
    | Some (p, _) -> p)

let nest x y = sprintf "%s.%s" x y
