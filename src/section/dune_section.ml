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

let all =
  [ (Lib, "lib")
  ; (Lib_root, "lib_root")
  ; (Libexec, "libexec")
  ; (Libexec_root, "libexec_root")
  ; (Bin, "bin")
  ; (Sbin, "sbin")
  ; (Toplevel, "toplevel")
  ; (Share, "share")
  ; (Share_root, "share_root")
  ; (Etc, "etc")
  ; (Etc_root, "etc_root")
  ; (Doc, "doc")
  ; (Stublibs, "stublibs")
  ; (Man, "man")
  ; (Misc, "misc")
  ]

let to_string t = List.assoc t all

let rec of_string x = function
  | [] -> None
  | (s, x') :: xs ->
    if x' = x then
      Some s
    else
      of_string x xs

let of_string x = of_string x all
