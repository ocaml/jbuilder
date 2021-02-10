open! Dune_engine
open Import
open! No_io
open! Action_builder.O
module SC = Super_context

(* This module interprets [(menhir ...)] stanzas -- that is, it provides build
   rules for Menhir parsers. *)

(* We assume that Menhir supports the commands [--infer-write-query] and
   [--infer-read-reply]. Although we could test at runtime whether this is the
   case, it is simpler to just require it. Dune is shipped with a constraint
   that Menhir (if present) is at version 20180523 or newer. *)

(* In order to perform OCaml type inference, we first let Menhir create a mock
   [.ml] file that contains just the semantic actions. We then use the OCaml
   compiler to perform type inference and create an inferred [.mli] file.
   Finally, we let Menhir read this [.mli] file. *)

(* This three-step process described is skipped in some cases where we know that
   OCaml type inference is not required, e.g., when Menhir is used in certain
   special modes, such as [--only-tokens]. In those cases, we use a
   straightforward one-step process. *)

(* -------------------------------------------------------------------------- *)

(* This signature describes the input of the functor [Run], which follows. *)

type stanza = Dune_file.Menhir.t

module type PARAMS = sig
  (* [cctx] is the compilation context. *)

  val cctx : Compilation_context.t

  (* [dir] is the directory inside [_build/<context>/...] where the build
     happens. If the [(menhir ...)] stanza appears in [src/dune], then [dir] is
     of the form [_build/<context>/src], e.g., [_build/default/src]. *)
  val dir : Path.Build.t

  (* [stanza] is the [(menhir ...)] stanza, as found in the [dune] file. *)

  val stanza : stanza
end

(* -------------------------------------------------------------------------- *)

(* This functor is where [(menhir ...)] stanzas are desugared. *)

module Run (P : PARAMS) : sig end = struct
  open P

  (* [sctx] is the "super context", while [context] is the "context". Both store
     information about the current build context. *)

  let sctx = Compilation_context.super_context cctx

  (* [build_dir] is the base directory of the context; we run menhir from this
     directory to we get correct error paths. *)
  let build_dir = (Super_context.context sctx).build_dir

  let expander = Compilation_context.expander cctx

  (* ------------------------------------------------------------------------ *)

  (* [where_is_flag stanza flag] tests whether the flag [flag] is present
     among the flags in [stanza]. If so, the flag's location is returned. *)

  (* TODO This code ignores every element that has polarity [Neg]. This is
     likely to be incorrect. *)

  (* TODO This code seems to be the only call site of [fold_strings] in dune.
     This is weird. There is probably a better way of testing for the presence
     of a flag. Help is welcome! *)

  let where_is_flag (stanza : stanza) (flag : string) : Loc.t option =
    Ordered_set_lang.Unexpanded.fold_strings stanza.flags ~init:(None)
      ~f:(fun pos sw accu ->
        match accu, pos, String_with_vars.text_only sw with
        | _, Neg, _ ->
            accu
        | Some _, Pos, _ ->
            accu
        | None, Pos, Some flag' when flag = flag' ->
            Some (String_with_vars.loc sw)
        | None, Pos, _ ->
            accu
      )

  (* [has_flag stanza flag] tests whether the flag [flag] is present among the
     flags in [stanza]. A Boolean result is returned. *)

  let has_flag (stanza : stanza) (flag : string) : bool =
    Option.is_some (where_is_flag stanza flag)

  (* ------------------------------------------------------------------------ *)

  (* Naming conventions. *)

  (* If [m] is a (short) module name, such as "myparser", then [source m] is the
     corresponding source file, and [targets m] is the list of targets that
     Menhir must build. *)

  let source m = Path.relative (Path.build dir) (m ^ ".mly")

  let cons_if b x xs =
    if b then x :: xs else xs

  let targets m stanza =
    let basenames =
      [ m ^ ".ml"; m ^ ".mli" ]
      |> cons_if (has_flag stanza "--cmly") (m ^ ".cmly")
      |> cons_if (has_flag stanza "--dump") (m ^ ".automaton")
      |> cons_if (has_flag stanza "--dump-resolved") (m ^ ".automaton.resolved")
      |> cons_if (has_flag stanza "--explain") (m ^ ".conflicts")
      |> cons_if (has_flag stanza "--automaton-graph") (m ^ ".dot")
      |> cons_if (has_flag stanza "--reference-graph") (m ^ ".dot")
    in
    List.map basenames ~f:(Path.Build.relative dir)

  let sources ms = List.map ~f:source ms

  (* The following definitions control where the mock [.ml] file and the
     inferred [.mli] file are created and how they are named. *)

  (* We change the module's base name, and use dummy extensions, so as to
     minimize the risk of confusing the build system (and the user). *)

  let mock m = m ^ "__mock"

  let mock_ml m : Path.Build.t = Path.Build.relative dir (mock m ^ ".ml.mock")

  let inferred_mli m : Path.Build.t =
    Path.Build.relative dir (mock m ^ ".mli.inferred")

  (* ------------------------------------------------------------------------ *)

  (* Rule generation. *)

  let menhir_binary =
    SC.resolve_program sctx ~dir "menhir" ~loc:None ~hint:"opam install menhir"

  (* Reminder (from command.mli):

     [Deps] is for command line arguments that are dependencies. [As] is for
     command line arguments that are neither dependencies nor targets.
     [Hidden_targets] is for targets that are *not* command line arguments. *)

  type 'a args = 'a Command.Args.t list

  (* [menhir args] generates a Menhir command line (a build action). *)

  let menhir (args : 'a args) : Action.t Action_builder.With_targets.t =
    Command.run ~dir:(Path.build build_dir) menhir_binary args

  let rule ?(mode = stanza.mode) :
      Action.t Action_builder.With_targets.t -> unit =
    SC.add_rule sctx ~dir ~mode ~loc:stanza.loc

  let expand_flags flags = Super_context.menhir_flags sctx ~dir ~expander ~flags

  (* ------------------------------------------------------------------------ *)

  (* If there is no [base] clause, then a stanza that mentions several modules
     is equivalent to a list of stanzas, each of which mentions one module, so
     Menhir must be invoked once per module, separately. If there is a [base]
     clause, then the stanza describes a multi-module parser, so Menhir must be
     invoked once. In either case, we are able to reformulate the input in the
     form of a list of stanzas, each of which has a [base] clause. *)

  (* The current concrete name for [base] clauses is [merge_into], but I would
     like to change it in the future. *)

  let stanzas : stanza list =
    match stanza.merge_into with
    | None ->
      List.map
        ~f:(fun m -> { stanza with modules = [ m ]; merge_into = Some m })
        stanza.modules
    | Some _ -> [ stanza ]

  (* ------------------------------------------------------------------------ *)

  (* The [--infer-*] commands should not be passed by the user; we take care of
     using these commands appropriately. Fail if they are present. *)

  let forbidden_flags =
    [ "--depend"
    ; "--raw-depend"
    ; "--infer"
    ; "--infer-write-query"
    ; "--infer-read-reply"
    ]

  let () =
    List.iter stanzas ~f:(fun (stanza : stanza) ->
      List.iter forbidden_flags ~f:(fun flag ->
        Option.iter (where_is_flag stanza flag) ~f:(fun loc ->
          User_error.raise ~loc
            [ Pp.textf "The flag %s must not be used in a menhir stanza." flag
            ])))

  (* ------------------------------------------------------------------------ *)

  (* [process3 stanza] converts a Menhir stanza into a set of build rules. This
     is the three-step process where Menhir is invoked twice and OCaml type
     inference is performed in between. *)

  let process3 base (stanza : stanza) : unit =
    let expanded_flags = expand_flags stanza.flags in
    (* 1. A first invocation of Menhir creates a mock [.ml] file. *)
    rule ~mode:Standard
      (menhir
         [ Command.Args.dyn expanded_flags
         ; Deps (sources stanza.modules)
         ; A "--base"
         ; Path (Path.relative (Path.build dir) base)
         ; A "--infer-write-query"
         ; Target (mock_ml base)
         ]);
    (* 2. The OCaml compiler performs type inference. *)
    let name = Module_name.of_string_allow_invalid (stanza.loc, mock base) in
    let mock_module : Module.t =
      let source =
        let impl = Module.File.make Dialect.ocaml (Path.build (mock_ml base)) in
        Module.Source.make ~impl name
      in
      Module.of_source ~visibility:Public ~kind:Impl source
    in
    let mock_module =
      Pp_spec.pp_module_as
        (Compilation_context.preprocessing cctx)
        name mock_module ~lint:false
    in
    let cctx = Compilation_context.without_bin_annot cctx in
    let deps = Dep_rules.for_module cctx mock_module in
    Module_compilation.ocamlc_i ~deps cctx mock_module
      ~output:(inferred_mli base);
    (* 3. A second invocation of Menhir reads the inferred [.mli] file. *)
    rule
      (menhir
         [ Command.Args.dyn expanded_flags
         ; Deps (sources stanza.modules)
         ; A "--base"
         ; Path (Path.relative (Path.build dir) base)
         ; A "--infer-read-reply"
         ; Dep (Path.build (inferred_mli base))
         ; Hidden_targets (targets base stanza)
         ])

  (* ------------------------------------------------------------------------ *)

  (* [process3 stanza] converts a Menhir stanza into a set of build rules. This
     is a simpler one-step process where Menhir is invoked directly. *)

  let process1 base (stanza : stanza) : unit =
    let expanded_flags = expand_flags stanza.flags in
    rule
      (menhir
         [ Command.Args.dyn expanded_flags
         ; Deps (sources stanza.modules)
         ; A "--base"
         ; Path (Path.relative (Path.build dir) base)
         ; Hidden_targets (targets base stanza)
         ])

  (* ------------------------------------------------------------------------ *)

  (* [process stanza] converts a Menhir stanza into a set of build rules, using
     either [process3] or [process1], as appropriate. *)

  (* Because Menhir processes [--only-tokens] before the [--infer-*] commands,
     when [--only-tokens] is present, no [--infer-*] command should be used. *)

  let process (stanza : stanza) : unit =
    let base = Option.value_exn stanza.merge_into in
    let ocaml_type_inference_disabled = has_flag stanza "--only-tokens" in
    if ocaml_type_inference_disabled || not stanza.infer then
      process1 base stanza
    else
      process3 base stanza

  (* ------------------------------------------------------------------------ *)

  (* The main side effect. *)

  let () = List.iter ~f:process stanzas
end

(* -------------------------------------------------------------------------- *)

(* The final glue. *)

let modules (stanza : Dune_file.Menhir.t) : string list =
  match stanza.merge_into with
  | Some m -> [ m ]
  | None -> stanza.modules

let targets (stanza : Dune_file.Menhir.t) : string list =
  let f m = [ m ^ ".ml"; m ^ ".mli" ] in
  List.concat_map (modules stanza) ~f

let module_names (stanza : Dune_file.Menhir.t) : Module_name.t list =
  List.map (modules stanza) ~f:(fun s ->
      (* TODO the loc can improved here *)
      Module_name.of_string_allow_invalid (stanza.loc, s))

let gen_rules ~dir cctx stanza =
  let module R = Run (struct
    let cctx = cctx

    let dir = dir

    let stanza = stanza
  end) in
  ()
