open! Stdune
open! Import

type t =
  { name : Context_name.t
  ; build_dir : Path.Build.t
  ; env : Env.t
  ; host : t option
  ; stdlib_dir : Path.t  (** todo try to remove it *)
  ; default_ocamlpath : Path.t list  (** todo try to remove it *)
  ; ocamlc : Path.t  (** todo try to remove it *)
  ; ocamlopt : Path.t option  (** todo try to remove it *)
  }

let create ~name ~build_dir ~env ~host ~stdlib_dir ~default_ocamlpath ~ocamlc
    ~ocamlopt =
  { name
  ; build_dir
  ; env
  ; host
  ; stdlib_dir
  ; default_ocamlpath
  ; ocamlc
  ; ocamlopt
  }
