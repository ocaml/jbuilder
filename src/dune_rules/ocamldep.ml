open! Dune_engine
open! Stdune
open Import

let parse_module_names ~(unit : Module.t) ~modules words =
  List.filter_map words ~f:(fun m ->
      let m = Module_name.of_string m in
      Modules.find_dep modules ~of_:unit m)

let parse_deps_exn ~file lines =
  let invalid () =
    User_error.raise
      [ Pp.textf "ocamldep returned unexpected output for %s:"
          (Path.to_string_maybe_quoted file)
      ; Pp.vbox
          (Pp.concat_map lines ~sep:Pp.cut ~f:(fun line ->
               Pp.seq (Pp.verbatim "> ") (Pp.verbatim line)))
      ]
  in
  match lines with
  | []
  | _ :: _ :: _ ->
    invalid ()
  | [ line ] -> (
    match String.lsplit2 line ~on:':' with
    | None -> invalid ()
    | Some (basename, deps) ->
      let basename = Filename.basename basename in
      if basename <> Path.basename file then invalid ();
      String.extract_blank_separated_words deps)

let read_deps_of ~obj_dir ~modules ~ml_kind unit =
  let all_deps_file = Obj_dir.Module.dep obj_dir (Transitive (unit, ml_kind)) in
  Action_builder.memoize
    (Path.Build.to_string all_deps_file)
    (Action_builder.map
       ~f:(parse_module_names ~unit ~modules)
       (Action_builder.lines_of (Path.build all_deps_file)))

let read_raw_deps_of ~obj_dir ~ml_kind unit =
  match Module.source unit ~ml_kind with
  | None -> None
  | Some source ->
    let dep = Path.build (Obj_dir.Module.dep obj_dir (Immediate source)) in
    Some
      (Action_builder.memoize (Path.to_string dep)
         (let open Action_builder.O in
         let+ lines = Action_builder.lines_of dep in
         List.map ~f:Module_name.of_string
           (parse_deps_exn ~file:(Module.File.path source) lines)))
