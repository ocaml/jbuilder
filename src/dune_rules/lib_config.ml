open! Dune_engine
open! Stdune

type t =
  { has_native : bool
  ; ext_lib : string
  ; ext_obj : string
  ; os_type : Ocaml_config.Os_type.t
  ; architecture : string
  ; system : string
  ; model : string
  ; natdynlink_supported : Dynlink_supported.By_the_os.t
  ; ext_dll : string
  ; stdlib_dir : Path.t
  ; ccomp_type : Ocaml_config.Ccomp_type.t
  ; profile : Profile.t
  ; ocaml_version_string : string
  ; ocaml_version : Ocaml_version.t
  ; instrument_with : Lib_name.t list
  ; context_name : Context_name.t
  }

let allowed_in_enabled_if =
  [ ("architecture", (1, 0))
  ; ("system", (1, 0))
  ; ("model", (1, 0))
  ; ("os_type", (1, 0))
  ; ("ccomp_type", (2, 0))
  ; ("profile", (2, 5))
  ; ("ocaml_version", (2, 5))
  ; ("context_name", (2, 8))
  ]

let get_for_enabled_if t (pform : Pform.t) =
  match pform with
  | Var Architecture -> t.architecture
  | Var System -> t.system
  | Var Model -> t.model
  | Var Os_type -> Ocaml_config.Os_type.to_string t.os_type
  | Var Ccomp_type -> Ocaml_config.Ccomp_type.to_string t.ccomp_type
  | Var Profile -> Profile.to_string t.profile
  | Var Ocaml_version -> t.ocaml_version_string
  | Var Context_name -> Context_name.to_string t.context_name
  | _ ->
    Code_error.raise "Lib_config.get_for_enabled_if: var not allowed"
      [ ("var", Pform.to_dyn pform) ]

let linker_can_create_empty_archives t =
  match t.ccomp_type with
  | Msvc -> false
  | Other _ -> true

let hash = Poly.hash

let equal = Poly.equal
