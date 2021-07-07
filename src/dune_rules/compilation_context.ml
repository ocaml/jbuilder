open! Dune_engine
open! Stdune
open Import
module SC = Super_context

module Includes = struct
  type t =
    Module.t -> Command.Args.without_targets Command.Args.t Cm_kind.Dict.t

  let read_raw_deps_of ~obj_dir ~ml_kind m =
    (* If the module does not have an interface, use the dependencies of the
       implementation. *)
    let ml_kind =
      if ml_kind = Ml_kind.Intf && not (Module.has m ~ml_kind) then
        Ml_kind.Impl
      else
        ml_kind
    in
    match Module.kind m with
    | Module.Kind.Impl -> Ocamldep.read_raw_deps_of ~obj_dir ~ml_kind m
    | _ -> None

  let make ~project ~obj_dir ~opaque ~requires : _ -> _ Cm_kind.Dict.t =
    let open Action_builder.O in
    let iflags libs mode = Lib.L.include_flags ~project libs mode in
    let entry_modules =
      let* libs = Resolve.read requires in
      let+ entry_modules =
        Action_builder.List.map libs ~f:(fun lib ->
            let* entry_module_names =
              Action_builder.memo_build (Lib.entry_module_names lib)
            in
            Resolve.read entry_module_names)
      in
      List.fold_left2 libs entry_modules ~init:Module_name.Map.empty
        ~f:(fun accu lib entry_modules ->
          List.fold_left entry_modules ~init:accu ~f:(fun accu entry_module ->
              Module_name.Map.update accu entry_module ~f:(function
                | None -> Some (Lib.Set.singleton lib)
                | Some libs -> Some (Lib.Set.add libs lib))))
    in
    let cmi_includes libs =
      let+ requires = Resolve.read requires
      and+ libs = libs in
      Command.Args.S
        [ iflags requires Byte
        ; Hidden_deps (Lib_file_deps.deps libs ~groups:[ Cmi ])
        ]
    in
    let cmx_includes libs =
      let+ requires = Resolve.read requires
      and+ libs = libs in
      Command.Args.S
        [ iflags requires Native
        ; Hidden_deps
            (if opaque then
              List.map libs ~f:(fun lib ->
                  ( lib
                  , if Lib.is_local lib then
                      [ Lib_file_deps.Group.Cmi ]
                    else
                      [ Cmi; Cmx ] ))
              |> Lib_file_deps.deps_with_exts
            else
              Lib_file_deps.deps libs ~groups:[ Lib_file_deps.Group.Cmi; Cmx ])
        ]
    in
    fun m ->
      let libs ml_kind =
        match read_raw_deps_of ~obj_dir ~ml_kind m with
        | None -> Resolve.read requires
        | Some deps ->
          let+ deps = deps
          and+ entry_modules = entry_modules in
          List.fold_left deps ~init:Lib.Set.empty ~f:(fun accu dep ->
              match Module_name.Map.find entry_modules dep with
              | None -> accu
              | Some libs -> Lib.Set.union accu libs)
          |> Lib.Set.to_list
      in
      let cmi_includes = Command.Args.Dyn (cmi_includes (libs Intf)) in
      let cmx_includes = Command.Args.Dyn (cmx_includes (libs Impl)) in
      { Cm_kind.Dict.cmi = cmi_includes
      ; cmo = cmi_includes
      ; cmx = cmx_includes
      }

  let empty _ = Cm_kind.Dict.make_all Command.Args.empty
end

type opaque =
  | Explicit of bool
  | Inherit_from_settings

let eval_opaque (context : Context.t) = function
  | Explicit b -> b
  | Inherit_from_settings ->
    Profile.is_dev context.profile
    && Ocaml_version.supports_opaque_for_mli context.version

type t =
  { super_context : Super_context.t
  ; scope : Scope.t
  ; expander : Expander.t
  ; obj_dir : Path.Build.t Obj_dir.t
  ; modules : Modules.t
  ; flags : Ocaml_flags.t
  ; requires_compile : Lib.t list Resolve.t
  ; requires_link : Lib.t list Resolve.t Lazy.t
  ; includes : Includes.t
  ; preprocessing : Pp_spec.t
  ; opaque : bool
  ; stdlib : Ocaml_stdlib.t option
  ; js_of_ocaml : Dune_file.Js_of_ocaml.t option
  ; sandbox : Sandbox_config.t
  ; package : Package.t option
  ; vimpl : Vimpl.t option
  ; modes : Mode.Dict.Set.t
  ; bin_annot : bool
  }

let super_context t = t.super_context

let scope t = t.scope

let expander t = t.expander

let dir t = Obj_dir.dir t.obj_dir

let obj_dir t = t.obj_dir

let modules t = t.modules

let flags t = t.flags

let requires_compile t = t.requires_compile

let requires_link t = Lazy.force t.requires_link

let includes t = t.includes

let preprocessing t = t.preprocessing

let opaque t = t.opaque

let stdlib t = t.stdlib

let js_of_ocaml t = t.js_of_ocaml

let sandbox t = t.sandbox

let package t = t.package

let vimpl t = t.vimpl

let modes t = t.modes

let bin_annot t = t.bin_annot

let context t = Super_context.context t.super_context

let create ~super_context ~scope ~expander ~obj_dir ~modules ~flags
    ~requires_compile ~requires_link ?(preprocessing = Pp_spec.dummy) ~opaque
    ?stdlib ~js_of_ocaml ~package ?vimpl ?modes ?(bin_annot = true) () =
  let project = Scope.project scope in
  let requires_compile =
    if Dune_project.implicit_transitive_deps project then
      Lazy.force requires_link
    else
      requires_compile
  in
  let sandbox =
    (* With sandboxing, there are a few build errors in ocaml platform 1162238ae
       like: File "ocaml_modules/ocamlgraph/src/pack.ml", line 1: Error: The
       implementation ocaml_modules/ocamlgraph/src/pack.ml does not match the
       interface ocaml_modules/ocamlgraph/src/.graph.objs/byte/graph__Pack.cmi: *)
    Sandbox_config.no_sandboxing
  in
  let modes =
    let default =
      Mode.Dict.make_both (Some Dune_file.Mode_conf.Kind.Inherited)
    in
    Option.value ~default modes |> Mode.Dict.map ~f:Option.is_some
  in
  let opaque = eval_opaque (Super_context.context super_context) opaque in
  { super_context
  ; scope
  ; expander
  ; obj_dir
  ; modules
  ; flags
  ; requires_compile
  ; requires_link
  ; includes =
      Includes.make ~project ~obj_dir ~opaque ~requires:requires_compile
  ; preprocessing
  ; opaque
  ; stdlib
  ; js_of_ocaml
  ; sandbox
  ; package
  ; vimpl
  ; modes
  ; bin_annot
  }

let for_alias_module t =
  let flags =
    let project = Scope.project t.scope in
    let dune_version = Dune_project.dune_version project in
    let profile = (Super_context.context t.super_context).profile in
    Ocaml_flags.default ~dune_version ~profile
  in
  let sandbox =
    let ctx = Super_context.context t.super_context in
    (* If the compiler reads the cmi for module alias even with [-w -49
       -no-alias-deps], we must sandbox the build of the alias module since the
       modules it references are built after. *)
    if Ocaml_version.always_reads_alias_cmi ctx.version then
      Sandbox_config.needs_sandboxing
    else
      Sandbox_config.no_special_requirements
  in
  { t with
    flags =
      Ocaml_flags.append_common flags
        [ "-w"; "-49"; "-nopervasives"; "-nostdlib" ]
  ; includes = Includes.empty
  ; stdlib = None
  ; sandbox
  }

let for_root_module t =
  let flags =
    let project = Scope.project t.scope in
    let dune_version = Dune_project.dune_version project in
    let profile = (Super_context.context t.super_context).profile in
    Ocaml_flags.default ~profile ~dune_version
  in
  { t with
    flags =
      Ocaml_flags.append_common flags
        [ "-w"; "-49"; "-nopervasives"; "-nostdlib" ]
  ; stdlib = None
  }

let for_module_generated_at_link_time cctx ~requires ~module_ =
  let opaque =
    (* Cmi's of link time generated modules are compiled with -opaque, hence
       their implementation must also be compiled with -opaque *)
    let ctx = Super_context.context cctx.super_context in
    Ocaml_version.supports_opaque_for_mli ctx.version
  in
  (* [modules] adds the wrong prefix "dune__exe__" but it's not used anyway *)
  let modules = Modules.singleton_exe module_ in
  { cctx with
    opaque
  ; js_of_ocaml = None
  ; flags = Ocaml_flags.empty
  ; requires_link = lazy requires
  ; requires_compile = requires
  ; modules
  }

let for_wrapped_compat t = { t with includes = Includes.empty; stdlib = None }

let for_plugin_executable t ~embed_in_plugin_libraries =
  let libs = Scope.libs t.scope in
  let requires_link =
    lazy (Resolve.List.map ~f:(Lib.DB.resolve libs) embed_in_plugin_libraries)
  in
  { t with requires_link }

let without_bin_annot t = { t with bin_annot = false }

let root_module_entries t =
  let open Action_builder.O in
  let* requires = Resolve.read t.requires_compile in
  let* l =
    Action_builder.List.map requires ~f:(fun lib ->
        Action_builder.memo_build (Lib.entry_module_names lib) >>= Resolve.read)
  in
  Action_builder.return (List.concat l)
