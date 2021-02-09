open! Dune_engine
open Stdune
open Action_builder.O

let gen_select_rules t ~dir compile_info =
  List.iter (Lib.Compile.resolved_selects compile_info) ~f:(fun rs ->
      let { Lib.Compile.Resolved_select.dst_fn; src_fn } = rs in
      let dst = Path.Build.relative dir dst_fn in
      Super_context.add_rule t ~dir
        (match src_fn with
        | Ok src_fn ->
          let src = Path.build (Path.Build.relative dir src_fn) in
          Action_builder.copy_and_add_line_directive ~src ~dst
        | Error e ->
          Action_builder.fail { fail = (fun () -> raise e) }
          |> Action_builder.with_targets ~targets:[ dst ]))

let with_lib_deps (t : Context.t) compile_info ~dir ~f =
  let prefix =
    Action_builder.label
      (Lib_deps_info.Label (Lib.Compile.lib_deps_info compile_info))
  in
  let prefix =
    if t.merlin then
      Merlin_ident.merlin_exists_path dir
        (Lib.Compile.merlin_ident compile_info)
      |> Path.build |> Action_builder.path >>> prefix
    else
      prefix
  in
  Build_system.prefix_rules prefix ~f
