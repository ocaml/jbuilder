let structure_item (mapper : Ast_mapper.mapper)
    (item : Parsetree.structure_item) : Parsetree.structure_item =
  match item.pstr_desc with
  | Pstr_extension (({ txt = "test"; _ }, _), _) ->
      Findlib.init ();
      Fl_dynload.load_packages ~debug:true ["pcre"];
      Dynlink.loadfile "plugin/plugin.cmxs";
      Ast_helper.Str.include_ (Ast_helper.Incl.mk (Ast_helper.Mod.structure []))
  | _ -> Ast_mapper.default_mapper.structure_item mapper item

let mapper : Ast_mapper.mapper =
  { Ast_mapper.default_mapper with structure_item }

let rewriter _config _cookies : Ast_mapper.mapper =
  mapper

let () =
  Migrate_parsetree.Driver.register ~name:"findlib_with_ppx"
    (module Migrate_parsetree.OCaml_current)
    rewriter
