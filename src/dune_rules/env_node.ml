open! Dune_engine
open Import

module Odoc = struct
  type warnings = Dune_env.Stanza.Odoc.warnings =
    | Fatal
    | Nonfatal

  type t = { warnings : warnings }
end

module Coq = struct
  type t = Ordered_set_lang.Unexpanded.t
end

type t =
  { scope : Scope.t
  ; local_binaries : File_binding.Expanded.t list Memo.Lazy.t
  ; ocaml_flags : Ocaml_flags.t Memo.Lazy.t
  ; foreign_flags :
      string list Action_builder.t Foreign_language.Dict.t Memo.Lazy.t
  ; external_env : Env.t Memo.Lazy.t
  ; bin_artifacts : Artifacts.Bin.t Memo.Lazy.t
  ; inline_tests : Dune_env.Stanza.Inline_tests.t Memo.Lazy.t
  ; menhir_flags : string list Action_builder.t Memo.Lazy.t
  ; odoc : Odoc.t Memo.Lazy.t
  ; coq : Coq.t Memo.Lazy.t
  ; format_config : Format_config.t Memo.Lazy.t
  }

let scope t = t.scope

let local_binaries t = Memo.Lazy.force t.local_binaries

let ocaml_flags t = Memo.Lazy.force t.ocaml_flags

let foreign_flags t = Memo.Lazy.force t.foreign_flags

let external_env t = Memo.Lazy.force t.external_env

let bin_artifacts t = Memo.Lazy.force t.bin_artifacts

let inline_tests t = Memo.Lazy.force t.inline_tests

let menhir_flags t = Memo.Lazy.force t.menhir_flags

let format_config t = Memo.Lazy.force t.format_config

let set_format_config t format_config =
  { t with format_config = Memo.Lazy.of_val format_config }

let odoc t = Memo.Lazy.force t.odoc

let coq t = Memo.Lazy.force t.coq

let make ~dir ~inherit_from ~scope ~config_stanza ~profile ~expander
    ~expander_for_artifacts ~default_context_flags ~default_env
    ~default_bin_artifacts =
  let config = Dune_env.Stanza.find config_stanza ~profile in
  let inherited ~field ~root extend =
    Memo.lazy_ (fun () ->
        extend
          (match inherit_from with
          | None -> root
          | Some t -> field (Memo.Lazy.force t)))
  in
  let inherited_if_absent ~field ~root f_absent =
    Memo.lazy_ (fun () ->
        match root with
        | None ->
          f_absent
            (match inherit_from with
            | None -> None
            | Some t -> Some (field (Memo.Lazy.force t)))
        | Some x -> x)
  in
  let local_binaries =
    inherited ~field:local_binaries ~root:[] (fun binaries ->
        binaries
        @ List.map config.binaries
            ~f:
              (File_binding.Unexpanded.expand ~dir ~f:(fun template ->
                   Expander.expand
                     (Memo.Lazy.force expander_for_artifacts)
                     ~mode:Single ~template
                   |> Value.to_string ~dir:(Path.build dir))))
  in
  let external_env =
    inherited ~field:external_env ~root:default_env (fun env ->
        let env, have_binaries =
          (Env.extend_env env config.env_vars, List.is_non_empty config.binaries)
        in
        if have_binaries then
          let dir = Utils.local_bin dir |> Path.build in
          Env.cons_path env ~dir
        else
          env)
  in
  let bin_artifacts =
    inherited ~field:bin_artifacts ~root:default_bin_artifacts (fun binaries ->
        Artifacts.Bin.add_binaries binaries ~dir
          (Memo.Lazy.force local_binaries))
  in
  let ocaml_flags =
    let default_ocaml_flags =
      let project = Scope.project scope in
      let dune_version = Dune_project.dune_version project in
      Ocaml_flags.default ~profile ~dune_version
    in
    inherited ~field:ocaml_flags ~root:default_ocaml_flags (fun flags ->
        let expander = Expander.set_dir (Memo.Lazy.force expander) ~dir in
        Ocaml_flags.make ~spec:config.flags ~default:flags
          ~eval:(Expander.expand_and_eval_set expander))
  in
  let inline_tests =
    match config with
    | { inline_tests = Some s; _ } -> Memo.Lazy.of_val s
    | { inline_tests = None; _ } ->
      inherited ~field:inline_tests Fun.id
        ~root:
          (if Profile.is_inline_test profile then
            Enabled
          else
            Disabled)
  in
  let foreign_flags =
    inherited ~field:foreign_flags ~root:default_context_flags (fun flags ->
        let expander = Expander.set_dir (Memo.Lazy.force expander) ~dir in
        Foreign_language.Dict.mapi config.foreign_flags ~f:(fun ~language f ->
            let standard = Foreign_language.Dict.get flags language in
            Expander.expand_and_eval_set expander f ~standard))
  in
  let menhir_flags =
    inherited ~field:menhir_flags ~root:(Action_builder.return []) (fun flags ->
        let expander = Expander.set_dir (Memo.Lazy.force expander) ~dir in
        Expander.expand_and_eval_set expander config.menhir_flags
          ~standard:flags)
  in
  let odoc =
    let open Odoc in
    let root =
      (* DUNE3: Enable for dev profile in the future *)
      { warnings = Nonfatal }
    in
    inherited ~field:odoc ~root (fun { warnings } ->
        { warnings = Option.value config.odoc.warnings ~default:warnings })
  in
  let coq = inherited ~field:coq ~root:config.coq (fun x -> x) in
  let format_config =
    inherited_if_absent ~field:format_config ~root:config.format_config
      (function
      | None ->
        Code_error.raise
          "format config should always have a default value taken from the \
           project root"
          []
      | Some x -> x)
  in
  { scope
  ; ocaml_flags
  ; foreign_flags
  ; external_env
  ; bin_artifacts
  ; local_binaries
  ; inline_tests
  ; menhir_flags
  ; odoc
  ; coq
  ; format_config
  }
