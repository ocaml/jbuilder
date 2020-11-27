(** {2 Data for the plugin system} *)

(** At link time dune create the implementation of the module. *)

(** Findlib predicates set true by dune. It is used during the interpretation of
    the metafile *)
val findlib_predicates_set_by_dune : string -> bool

(** Library statically linked in the executable *)
val already_linked_libraries : string list

(** Information about builtin libraries, such as the one installed by the OCaml
    compiler. The META file are not always present, so version reduced to the
    needed information are included here. *)
val builtin_library : (string * Meta_parser.t) list

(** The path of the ocaml compilers. None when not available in the binary *)
val ocamlc : string option

val ocamlopt : string option
