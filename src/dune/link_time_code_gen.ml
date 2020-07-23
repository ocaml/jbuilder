open Import
module CC = Compilation_context
module SC = Super_context

type t =
  { to_link : Lib.Lib_and_module.t list
  ; force_linkall : bool
  }

let generate_and_compile_module cctx ~precompiled_cmi ~name:basename ~lib ~code
    ~requires =
  let sctx = CC.super_context cctx in
  let obj_dir = CC.obj_dir cctx in
  let dir = CC.dir cctx in
  let module_ =
    let name =
      let info = Lib.info lib in
      let loc = Lib_info.loc info in
      Module_name.of_string_allow_invalid (loc, basename)
    in
    let wrapped = Result.ok_exn (Lib.wrapped lib) in
    let src_dir = Path.build (Obj_dir.obj_dir obj_dir) in
    let gen_module = Module.generated ~src_dir name in
    match wrapped with
    | None -> gen_module
    | Some (Yes_with_transition _) -> assert false
    | Some (Simple false) -> gen_module
    | Some (Simple true) ->
      let main_module_name =
        Lib.main_module_name lib |> Result.ok_exn |> Option.value_exn
      in
      Module.with_wrapper gen_module ~main_module_name
  in
  SC.add_rule ~dir sctx
    (let ml =
       Module.file module_ ~ml_kind:Impl
       |> Option.value_exn |> Path.as_in_build_dir_exn
     in
     Build.write_file_dyn ml code);
  let cctx =
    Compilation_context.for_module_generated_at_link_time cctx ~requires
      ~module_
  in
  Module_compilation.build_module
    ~dep_graphs:(Dep_graph.Ml_kind.dummy module_)
    ~precompiled_cmi cctx module_;
  module_

let pr buf fmt = Printf.bprintf buf (fmt ^^ "\n")

let prlist buf name l ~f =
  match l with
  | [] -> pr buf "let %s = []" name
  | x :: l ->
    pr buf "let %s =" name;
    Printf.bprintf buf "  [ ";
    f x;
    List.iter l ~f:(fun x ->
        Printf.bprintf buf "  ; ";
        f x);
    pr buf "  ]"

let findlib_init_code ~preds ~libs =
  let public_libs =
    List.filter
      ~f:(fun lib ->
        let info = Lib.info lib in
        let status = Lib_info.status info in
        not (Lib_info.Status.is_private status))
      libs
  in
  let buf = Buffer.create 1024 in
  List.iter public_libs ~f:(fun lib ->
      pr buf "Findlib.record_package Findlib.Record_core %S;;"
        (Lib_name.to_string (Lib.name lib)));
  prlist buf "preds" (Variant.Set.to_list preds) ~f:(fun v ->
      pr buf "%S" (Variant.to_string v));
  pr buf "in";
  pr buf "let preds =";
  pr buf "  (if Dynlink.is_native then \"native\" else \"byte\") :: preds";
  pr buf "in";
  pr buf "Findlib.record_package_predicates preds;;";
  Buffer.contents buf

let gen_placeholder_var =
  let n = ref 0 in
  fun () ->
    let s = sprintf "p%d" !n in
    incr n;
    s

let fmt_eval ~cctx code =
  let context = CC.context cctx in
  let ocaml_version = Ocaml_version.of_ocaml_config context.ocaml_config in
  if Ocaml_version.has_sys_opaque_identity ocaml_version then
    Printf.sprintf "eval (Sys.opaque_identity %S)" code
  else
    Printf.sprintf "eval %S" code

let build_info_code_v1 ~cctx ~libs buf =
  (* [placeholders] is a mapping from source path to variable names. For each
     binding [(p, v)], we will generate the following code:

     {[ let v = Placeholder "%%DUNE_PLACEHOLDER:...:vcs-describe:...:p%%" ]} *)
  let placeholders = ref Path.Source.Map.empty in
  let placeholder p =
    match File_tree.nearest_vcs p with
    | None -> "None"
    | Some vcs -> (
      let p =
        Option.value
          (Path.as_in_source_tree vcs.root)
          (* The only VCS root that is potentially not in the source tree is the
             VCS at the root of the repo. For this VCS, it is enough to use the
             source tree root in the placeholder given that we take the nearest
             VCS when performing the actual substitution. *)
          ~default:Path.Source.root
      in
      match Path.Source.Map.find !placeholders p with
      | Some var -> var
      | None ->
        let var = gen_placeholder_var () in
        placeholders := Path.Source.Map.set !placeholders p var;
        var )
  in
  let version_of_package (p : Package.t) =
    match p.version with
    | Some v -> sprintf "Some %S" v
    | None -> placeholder p.path
  in
  let version =
    match Compilation_context.package cctx with
    | Some p -> version_of_package p
    | None ->
      let p = Path.Build.drop_build_context_exn (CC.dir cctx) in
      placeholder p
  in
  let libs =
    List.map libs ~f:(fun lib ->
        ( Lib.name lib
        , match Lib_info.version (Lib.info lib) with
          | Some v -> sprintf "Some %S" v
          | None -> (
            match Lib_info.status (Lib.info lib) with
            | Installed -> "None"
            | Public (_, p) -> version_of_package p
            | Private _ ->
              let p =
                Path.drop_build_context_exn (Obj_dir.dir (Lib.obj_dir lib))
              in
              placeholder p ) ))
  in
  Path.Source.Map.iteri !placeholders ~f:(fun path var ->
      pr buf "let %s = %s" var
        (fmt_eval ~cctx
           (Artifact_substitution.encode ~min_len:64 (Vcs_describe path))));
  if not (Path.Source.Map.is_empty !placeholders) then pr buf "";
  pr buf "let version = %s" version;
  pr buf "";
  prlist buf "statically_linked_libraries" libs ~f:(fun (name, v) ->
      pr buf "%S, %s" (Lib_name.to_string name) v);
  pr buf ""

let build_info_code_v2 ~cctx ~custom_build_info:(exe_cbi, lib_cbis) buf =
  let dir = CC.dir cctx in
  let encode min_len name =
    Artifact_substitution.(encode ~min_len (Custom (name, dir)))
  in
  let lib_cbi (name, { Custom_build_info.max_size; _ }) =
    let name = Lib_name.to_string name in
    pr buf "%S, %s" name (fmt_eval ~cctx (encode max_size name))
  in
  let exe_cbi =
    match exe_cbi with
    | Some { Custom_build_info.max_size; _ } ->
      fmt_eval ~cctx (encode max_size "exe")
    | None -> "None"
  in
  pr buf "let custom = %s" exe_cbi;
  pr buf "";
  prlist buf "lib_customs" lib_cbis ~f:lib_cbi;
  pr buf "";
  pr buf "let custom_lib name = List.assoc name lib_customs"

let build_info_code cctx ~libs ~api_version ~custom_build_info =
  let open Lib_info.Special_builtin_support in
  let buf = Buffer.create 1024 in
  let version_specific () =
    match api_version with
    | Build_info.V1 -> build_info_code_v1 ~cctx ~libs buf
    | Build_info.V2 ->
      build_info_code_v1 ~cctx ~libs buf;
      build_info_code_v2 ~cctx ~custom_build_info buf
  in
  (* Parse the replacement format described in [artifact_substitution.ml]. *)
  pr buf "let eval s =";
  pr buf "  let len = String.length s in";
  pr buf "  if s.[0] = '=' then";
  pr buf "    let colon_pos = String.index_from s 1 ':' in";
  pr buf "    let vlen = int_of_string (String.sub s 1 (colon_pos - 1)) in";
  pr buf "    (* This [min] is because the value might have been truncated";
  pr buf "       if it was too large *)";
  pr buf "    let vlen = min vlen (len - colon_pos - 1) in";
  pr buf "    Some (String.sub s (colon_pos + 1) vlen)";
  pr buf "  else";
  pr buf "    None";
  pr buf "[@@inline never]";
  pr buf "";
  version_specific ();
  pr buf "";
  Buffer.contents buf

let handle_special_libs ~custom_build_info cctx =
  let open Result.O in
  let+ all_libs = CC.requires_link cctx in
  let obj_dir = Compilation_context.obj_dir cctx |> Obj_dir.of_local in
  let sctx = CC.super_context cctx in
  let module LM = Lib.Lib_and_module in
  let cbis =
    Lib_info.gather_custom_build_info (List.map ~f:Lib.info all_libs)
  in
  let custom_build_info = (custom_build_info, cbis) in
  let rec process_libs ~to_link_rev ~force_linkall libs =
    match libs with
    | [] -> { to_link = List.rev to_link_rev; force_linkall }
    | lib :: libs -> (
      match Lib_info.special_builtin_support (Lib.info lib) with
      | None ->
        process_libs libs
          ~to_link_rev:(LM.Lib lib :: to_link_rev)
          ~force_linkall
      | Some special -> (
        match special with
        | Build_info { data_module; api_version } ->
          let module_ =
            let code =
              Build.return
                (build_info_code cctx ~libs:all_libs ~api_version
                   ~custom_build_info)
            in
            generate_and_compile_module cctx ~name:data_module ~lib ~code
              ~requires:(Ok [ lib ])
              ~precompiled_cmi:true
          in
          process_libs libs
            ~to_link_rev:(LM.Lib lib :: Module (obj_dir, module_) :: to_link_rev)
            ~force_linkall
        | Findlib_dynload ->
          (* If findlib.dynload is linked, we stores in the binary the packages
             linked by linking just after findlib.dynload a module containing
             the info *)
          let requires =
            (* This shouldn't fail since findlib.dynload depends on dynlink and
               findlib. That's why it's ok to use a dummy location. *)
            let+ dynlink =
              Lib.DB.resolve (SC.public_libs sctx)
                (Loc.none, Lib_name.of_string "dynlink")
            and+ findlib =
              Lib.DB.resolve (SC.public_libs sctx)
                (Loc.none, Lib_name.of_string "findlib")
            in
            [ dynlink; findlib ]
          in
          let module_ =
            generate_and_compile_module cctx ~lib ~name:"findlib_initl"
              ~code:
                (Build.return
                   (findlib_init_code
                      ~preds:Findlib.findlib_predicates_set_by_dune
                      ~libs:all_libs))
              ~requires ~precompiled_cmi:false
          in
          process_libs libs
            ~to_link_rev:(LM.Module (obj_dir, module_) :: Lib lib :: to_link_rev)
            ~force_linkall:true
        | Configurator _ ->
          process_libs libs
            ~to_link_rev:(LM.Lib lib :: to_link_rev)
            ~force_linkall ) )
  in
  process_libs all_libs ~to_link_rev:[] ~force_linkall:false
