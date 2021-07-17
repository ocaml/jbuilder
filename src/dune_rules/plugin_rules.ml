open! Stdune
open Dune_file.Plugin
open! Dune_engine
open Memo.Build.O

let meta_file ~dir { name; libraries = _; site = _, (pkg, site); _ } =
  Path.Build.L.relative dir
    [ ".site"
    ; Package.Name.to_string pkg
    ; Section.Site.to_string site
    ; Package.Name.to_string name
    ; Findlib.meta_fn
    ]

let resolve_libs ~sctx t =
  Resolve.Build.List.map t.libraries
    ~f:(Lib.DB.resolve (Super_context.public_libs sctx))

let setup_rules ~sctx ~dir t =
  let meta = meta_file ~dir t in
  Super_context.add_rule sctx ~dir
    (Action_builder.write_file_dyn meta
       (Resolve.Build.read
          (let open Resolve.Build.O in
          let+ requires = resolve_libs ~sctx t in
          let meta =
            { Meta.name = None
            ; entries =
                [ Gen_meta.requires
                    (Lib_name.Set.of_list_map ~f:Lib.name requires)
                ]
            }
          in
          Format.asprintf "%a" Pp.to_fmt
            (Pp.vbox (Pp.seq (Meta.pp meta.entries) Pp.cut)))))

let install_rules ~sctx ~dir ({ name; site = loc, (pkg, site); _ } as t) =
  let* skip_files =
    if t.optional then
      Resolve.Build.is_error (resolve_libs ~sctx t)
    else
      Memo.Build.return false
  in
  if skip_files then
    Memo.Build.return []
  else
    let meta = meta_file ~dir t in
    let+ entry =
      Install.Entry.make_with_site
        ~dst:(sprintf "%s/%s" (Package.Name.to_string name) Findlib.meta_fn)
        (Site { pkg; site; loc })
        (Super_context.get_site_of_packages sctx)
        meta
    in
    [ (Some loc, entry) ]
