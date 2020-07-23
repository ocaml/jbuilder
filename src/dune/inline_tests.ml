open! Stdune
open Import
open! No_io
module SC = Super_context

module Backend = struct
  module M = struct
    module Info = struct
      let name = Sub_system_name.make "inline_tests.backend"

      type t =
        { loc : Loc.t
        ; runner_libraries : (Loc.t * Lib_name.t) list
        ; flags : Ordered_set_lang.Unexpanded.t
        ; generate_runner : (Loc.t * Action_unexpanded.t) option
        ; extends : (Loc.t * Lib_name.t) list
        }

      type Sub_system_info.t += T of t

      let loc t = t.loc

      (* The syntax of the driver sub-system is part of the main dune syntax, so
         we simply don't create a new one.

         If we wanted to make the ppx system an extension, then we would create
         a new one. *)
      let syntax = Stanza.syntax

      open Dune_lang.Decoder

      let decode =
        fields
          (let+ loc = loc
           and+ runner_libraries =
             field "runner_libraries"
               (repeat (located Lib_name.decode))
               ~default:[]
           and+ flags = Ordered_set_lang.Unexpanded.field "flags"
           and+ generate_runner =
             field_o "generate_runner" (located Action_dune_lang.decode)
           and+ extends =
             field "extends" (repeat (located Lib_name.decode)) ~default:[]
           in
           { loc; runner_libraries; flags; generate_runner; extends })

      let encode t =
        let open Dune_lang.Encoder in
        let lib (_loc, x) = Lib_name.encode x in
        ( (1, 0)
        , record_fields
          @@ [ field_l "runner_libraries" lib t.runner_libraries
             ; field_i "flags" Ordered_set_lang.Unexpanded.encode t.flags
             ; field_o "generate_runner" Action_dune_lang.encode
                 (Option.map t.generate_runner ~f:snd)
             ; field_l "extends" lib t.extends
             ] )
    end

    type t =
      { info : Info.t
      ; lib : Lib.t
      ; runner_libraries : Lib.t list Or_exn.t
      ; extends : t list Or_exn.t
      }

    let desc ~plural =
      "inline tests backend"
      ^
      if plural then
        "s"
      else
        ""

    let desc_article = "an"

    let lib t = t.lib

    let extends t = t.extends

    let instantiate ~resolve ~get lib (info : Info.t) =
      { info
      ; lib
      ; runner_libraries = Result.List.map info.runner_libraries ~f:resolve
      ; extends =
          (let open Result.O in
          Result.List.map info.extends ~f:(fun ((loc, name) as x) ->
              let* lib = resolve x in
              match get ~loc lib with
              | None ->
                Error
                  (User_error.E
                     (User_error.make ~loc
                        [ Pp.textf "%S is not an %s" (Lib_name.to_string name)
                            (desc ~plural:false)
                        ]))
              | Some t -> Ok t))
      }

    let public_info t =
      let open Result.O in
      let+ runner_libraries = t.runner_libraries
      and+ extends = t.extends in
      { Info.loc = t.info.loc
      ; flags = t.info.flags
      ; generate_runner = t.info.generate_runner
      ; runner_libraries =
          List.map2 t.info.runner_libraries runner_libraries
            ~f:(fun (loc, _) lib -> (loc, Lib.name lib))
      ; extends =
          List.map2 t.info.extends extends ~f:(fun (loc, _) t ->
              (loc, Lib.name t.lib))
      }
  end

  include M
  include Sub_system.Register_backend (M)
end

include Sub_system.Register_end_point (struct
  module Backend = Backend

  module Mode_conf = struct
    module T = struct
      type t =
        | Byte
        | Javascript
        | Native
        | Best

      let compare (a : t) b = Poly.compare a b

      let to_dyn _ = Dyn.opaque
    end

    include T
    open Dune_lang.Decoder

    let decode =
      enum
        [ ("byte", Byte)
        ; ("js", Javascript)
        ; ("native", Native)
        ; ("best", Best)
        ]

    module O = Comparable.Make (T)

    module Set = struct
      include O.Set

      let decode = repeat decode >>| of_list

      let default = of_list [ Best ]
    end
  end

  module Info = struct
    let name = Sub_system_name.make "inline_tests"

    type t =
      { loc : Loc.t
      ; deps : Dep_conf.t list
      ; modes : Mode_conf.Set.t
      ; flags : Ordered_set_lang.Unexpanded.t
      ; backend : (Loc.t * Lib_name.t) option
      ; libraries : (Loc.t * Lib_name.t) list
      }

    type Sub_system_info.t += T of t

    let loc t = t.loc

    let backends t = Option.map t.backend ~f:(fun x -> [ x ])

    let syntax = Stanza.syntax

    open Dune_lang.Decoder

    let decode =
      fields
        (let+ loc = loc
         and+ deps = field "deps" (repeat Dep_conf.decode) ~default:[]
         and+ flags = Ordered_set_lang.Unexpanded.field "flags"
         and+ backend = field_o "backend" (located Lib_name.decode)
         and+ libraries =
           field "libraries" (repeat (located Lib_name.decode)) ~default:[]
         and+ modes =
           field "modes"
             (Dune_lang.Syntax.since syntax (1, 11) >>> Mode_conf.Set.decode)
             ~default:Mode_conf.Set.default
         in
         { loc; deps; flags; backend; libraries; modes })

    (* We don't use this at the moment, but we could implement it for debugging
       purposes *)
    let encode _t = assert false
  end

  let gen_rules c ~(info : Info.t) ~backends =
    let { Sub_system.Library_compilation_context.super_context = sctx
        ; dir
        ; stanza = lib
        ; scope
        ; source_modules
        ; compile_info = _
        } =
      c
    in
    let loc = lib.buildable.loc in
    let lib_name = snd lib.name in
    let inline_test_name =
      sprintf "%s.inline-tests" (Lib_name.Local.to_string lib_name)
    in
    let inline_test_dir = Path.Build.relative dir ("." ^ inline_test_name) in
    let obj_dir =
      Obj_dir.make_exe ~dir:inline_test_dir ~name:inline_test_name
    in
    let name =
      sprintf "inline_test_runner_%s" (Lib_name.Local.to_string (snd lib.name))
    in
    let main_module =
      let name = Module_name.of_string name in
      let src_dir = Path.build inline_test_dir in
      Module.generated ~src_dir name
    in
    let modules = Modules.singleton_exe main_module in
    let bindings =
      Pform.Map.singleton "library-name"
        (Values [ String (Lib_name.Local.to_string (snd lib.name)) ])
    in
    let expander = Super_context.expander sctx ~dir in
    let runner_libs =
      let open Result.O in
      let* libs =
        Result.List.concat_map backends ~f:(fun (backend : Backend.t) ->
            backend.runner_libraries)
      in
      let* lib =
        Lib.DB.resolve (Scope.libs scope) (loc, Dune_file.Library.best_name lib)
      in
      let* more_libs =
        Result.List.map info.libraries ~f:(Lib.DB.resolve (Scope.libs scope))
      in
      Lib.closure ~linking:true ((lib :: libs) @ more_libs)
    in
    (* Generate the runner file *)
    SC.add_rule sctx ~dir ~loc
      (let target =
         Module.file main_module ~ml_kind:Impl
         |> Option.value_exn |> Path.as_in_build_dir_exn
       in
       let files ml_kind =
         Pform.Var.Values
           (Value.L.paths
              (List.filter_map source_modules ~f:(Module.file ~ml_kind)))
       in
       let bindings =
         Pform.Map.of_list_exn
           [ ("impl-files", files Impl); ("intf-files", files Intf) ]
       in
       let expander = Expander.add_bindings expander ~bindings in
       let action =
         let open Build.With_targets.O in
         let+ actions =
           Build.With_targets.all
             (List.filter_map backends ~f:(fun (backend : Backend.t) ->
                  Option.map backend.info.generate_runner
                    ~f:(fun (loc, action) ->
                      Action_unexpanded.expand action ~loc ~expander
                        ~dep_kind:Required
                        ~targets:(Forbidden "inline test generators")
                        ~targets_dir:dir
                        (Build.return Bindings.empty))))
         in
         Action.with_stdout_to target (Action.progn actions)
       in
       Build.With_targets.add ~targets:[ target ] action);
    let cctx =
      Compilation_context.create () ~super_context:sctx ~expander ~scope
        ~obj_dir ~modules ~opaque:(Explicit false) ~requires_compile:runner_libs
        ~requires_link:(lazy runner_libs)
        ~flags:(Ocaml_flags.of_list [ "-w"; "-24"; "-g" ])
        ~js_of_ocaml:(Some lib.buildable.js_of_ocaml) ~dynlink:false
        ~package:(Option.map lib.public ~f:Dune_file.Public_lib.package)
    in
    let linkages =
      let modes =
        if Mode_conf.Set.mem info.modes Javascript then
          Mode_conf.Set.add info.modes Byte
        else
          info.modes
      in
      List.map (Mode_conf.Set.to_list modes) ~f:(fun (mode : Mode_conf.t) ->
          match mode with
          | Native -> Exe.Linkage.native
          | Best -> Exe.Linkage.native_or_custom (Super_context.context sctx)
          | Byte -> Exe.Linkage.byte
          | Javascript -> Exe.Linkage.js)
    in
    Exe.build_and_link cctx
      ~program:{ name; main_module_name = Module.name main_module; loc }
      ~linkages
      ~link_args:(Build.return (Command.Args.A "-linkall"))
      ~custom_build_info:None ~promote:None;
    let flags =
      let flags =
        List.map backends ~f:(fun backend -> backend.Backend.info.flags)
        @ [ info.flags ]
      in
      let expander = Expander.add_bindings expander ~bindings in
      let open Build.O in
      let+ l =
        List.map flags
          ~f:(Expander.expand_and_eval_set expander ~standard:(Build.return []))
        |> Build.all
      in
      Command.Args.As (List.concat l)
    in
    let source_files = List.concat_map source_modules ~f:Module.sources in
    Mode_conf.Set.iter info.modes ~f:(fun (mode : Mode_conf.t) ->
        let ext =
          match mode with
          | Native
          | Best ->
            ".exe"
          | Javascript -> ".bc.js"
          | Byte -> ".bc"
        in
        let custom_runner =
          match mode with
          | Native
          | Best
          | Byte ->
            None
          | Javascript -> Some "node"
        in
        SC.add_alias_action sctx ~dir ~loc:(Some info.loc) (Alias.runtest ~dir)
          ~stamp:("ppx-runner", name, mode)
          (let exe =
             Path.build (Path.Build.relative inline_test_dir (name ^ ext))
           in
           let exe, runner_args =
             match custom_runner with
             | None -> (Ok exe, Command.Args.empty)
             | Some runner ->
               ( Super_context.resolve_program ~dir sctx ~loc:(Some loc) runner
               , Dep exe )
           in
           let open Build.With_targets.O in
           Build.with_no_targets (Dep_conf_eval.unnamed info.deps ~expander)
           >>> Build.with_no_targets (Build.paths source_files)
           >>> Build.progn
                 ( Command.run exe ~dir:(Path.build dir)
                     [ runner_args; Dyn flags ]
                 :: List.map source_files ~f:(fun fn ->
                        Build.With_targets.return
                          (Action.diff ~optional:true fn
                             (Path.Build.extend_basename
                                (Path.as_in_build_dir_exn fn)
                                ~suffix:".corrected"))) )))
end)

let linkme = ()
