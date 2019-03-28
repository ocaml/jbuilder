open Stdune

let dev_files =
  let id = lazy (
    let open Sexp.Encoder in
    constr "dev_files" [string ".cmt"; string ".cmti"; string ".cmi"]
  ) in
  Predicate.create ~id ~f:(fun p ->
    match Filename.extension p with
    | ".cmt"
    | ".cmti"
    | ".cmi" -> true
    | _ -> false)

let add_obj_dir sctx ~obj_dir =
  if (Super_context.context sctx).merlin then
    let dir_glob =
      let dir = Obj_dir.byte_dir obj_dir in
      File_selector.create ~dir dev_files in
    let deps = Dep.Set.singleton (Dep.glob dir_glob) in
    Build_system.Alias.add_deps
      (Alias.check ~dir:(Obj_dir.dir obj_dir))
      deps
