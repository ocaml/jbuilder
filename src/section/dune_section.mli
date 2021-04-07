type t =
  | Lib
  | Lib_root
  | Libexec
  | Libexec_root
  | Bin
  | Sbin
  | Toplevel
  | Share
  | Share_root
  | Etc
  | Etc_root
  | Doc
  | Stublibs
  | Man
  | Misc

val all : (t * string) list

val of_string : string -> t option

val to_string : t -> string
