open! Stdune
open Dune_engine

type ccomp_type =
  | Gcc
  | Msvc
  | Clang
  | Other of string

let base_cxx_flags = function
  | Gcc -> [ "-x"; "c++"; "-lstdc++"; "-shared-libgcc" ]
  | Clang -> [ "-x"; "c++" ]
  | Msvc -> [ "/TP" ]
  | _ -> []

let preprocessed_filename = "ccomp"

let ccomp_type dir =
  let open Build.O in
  let filepath =
    Path.Build.(relative (relative dir ".dune") preprocessed_filename)
  in
  let+ ccomp = Build.contents (Path.build filepath) in
  match String.trim ccomp with
  | "clang" -> Clang
  | "gcc" -> Gcc
  | "msvc" -> Msvc
  | s -> Other s

let check_warn = function
  | Other s ->
    User_warning.emit
      [ Pp.textf
          "Dune was not able to automatically infer the C compiler in use: \
           \"%s\". Please open an issue on github to help us improve this \
           feature."
          s
      ]
  | _ -> ()

let get_flags dir =
  let open Build.O in
  let+ ccomp_type = ccomp_type dir in
  check_warn ccomp_type;
  (ccomp_type, base_cxx_flags ccomp_type)
