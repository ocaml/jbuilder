type t = int * int * int

let make x = x

let of_ocaml_config ocfg = Ocaml_config.version ocfg

let supports_no_keep_locs version = version >= (4, 03, 0)

let supports_opaque_for_mli version = version >= (4, 03, 0)

let always_reads_alias_cmi version = version < (4, 03, 0)

let supports_color_in_ocamlparam version = version >= (4, 03, 0)

let supports_ocaml_color version = version >= (4, 05, 0)

let supports_response_file version = version >= (4, 05, 0)

let ocamlmklib_supports_response_file version = version >= (4, 08, 0)

let pervasives_includes_result version = version >= (4, 03, 0)

let stdlib_includes_uchar version = version >= (4, 03, 0)

let stdlib_includes_bigarray version = version >= (4, 07, 0)

let stdlib_includes_seq version = version >= (4, 07, 0)

let ooi_supports_no_approx version = version >= (4, 05, 0)

let ooi_supports_no_code version = version >= (4, 05, 0)

let supports_let_syntax version = version >= (4, 08, 0)

let supports_output_complete_exe version = version >= (4, 10, 0)

let supports_function_sections version = version > (4, 10, 0)

let supports_split_at_emit version = version >= (4, 11, 0)

let supports_library_linking version = version >= (4, 11, 0)

let custom_or_output_complete_exe version =
  if supports_output_complete_exe version then
    "-output-complete-exe"
  else
    "-custom"
