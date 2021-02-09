open! Dune_engine
open! Stdune
open Dune_lang.Decoder

(* workspace files use the same version numbers as dune-project files for
   simplicity *)
let syntax = Stanza.syntax

let env_field =
  field "env" ~default:Dune_env.Stanza.empty
    (Dune_lang.Syntax.since syntax (1, 1) >>> Dune_env.Stanza.decode)

module Context = struct
  module Target = struct
    type t =
      | Native
      | Named of Context_name.t

    let equal x y =
      match (x, y) with
      | Native, Native -> true
      | Named x, Named y -> Context_name.equal x y
      | _, _ -> false

    let t =
      let+ context_name = Context_name.decode in
      match Context_name.to_string context_name with
      | "native" -> Native
      | _ -> Named context_name

    let add ts x =
      match x with
      | None -> ts
      | Some t ->
        if List.mem t ~set:ts then
          ts
        else
          ts @ [ t ]
  end

  module Common = struct
    type t =
      { loc : Loc.t
      ; profile : Profile.t
      ; targets : Target.t list
      ; env : Dune_env.Stanza.t
      ; toolchain : Context_name.t option
      ; name : Context_name.t
      ; host_context : Context_name.t option
      ; paths : (string * Ordered_set_lang.t) list
      ; fdo_target_exe : Path.t option
      ; dynamically_linked_foreign_archives : bool
      ; instrument_with : Lib_name.t list
      }

    let to_dyn = Dyn.Encoder.opaque

    let equal
        { loc = _
        ; profile
        ; targets
        ; env
        ; toolchain
        ; name
        ; host_context
        ; paths
        ; fdo_target_exe
        ; dynamically_linked_foreign_archives
        ; instrument_with
        } t =
      Profile.equal profile t.profile
      && List.equal Target.equal targets t.targets
      && Dune_env.Stanza.equal env t.env
      && Option.equal Context_name.equal toolchain t.toolchain
      && Context_name.equal name t.name
      && Option.equal Context_name.equal host_context t.host_context
      && List.equal
           (Tuple.T2.equal String.equal Ordered_set_lang.equal)
           paths t.paths
      && Option.equal Path.equal fdo_target_exe t.fdo_target_exe
      && Bool.equal dynamically_linked_foreign_archives
           t.dynamically_linked_foreign_archives
      && List.equal Lib_name.equal instrument_with t.instrument_with

    let fdo_suffix t =
      match t.fdo_target_exe with
      | None -> ""
      | Some file ->
        let name, _ = Path.split_extension file in
        "-fdo-" ^ Path.basename name

    let t ~profile ~instrument_with =
      let+ env = env_field
      and+ targets =
        field "targets" (repeat Target.t) ~default:[ Target.Native ]
      and+ profile = field "profile" Profile.decode ~default:profile
      and+ host_context =
        field_o "host"
          (Dune_lang.Syntax.since syntax (1, 10) >>> Context_name.decode)
      and+ toolchain =
        field_o "toolchain"
          (Dune_lang.Syntax.since syntax (1, 5) >>> Context_name.decode)
      and+ dynamically_linked_foreign_archives =
        let+ disable =
          field ~default:false "disable_dynamically_linked_foreign_archives"
            (Dune_lang.Syntax.since syntax (2, 0) >>> bool)
        in
        not disable
      and+ fdo_target_exe =
        let f file =
          let ext = Filename.extension file in
          if ext = ".exe" then
            Path.(relative root file)
          else
            User_error.raise
              [ Pp.textf
                  "`fdo %s` expects executable filename ending with .exe \
                   extension, not %s. \n\
                   Please specify the name of the executable to optimize, \
                   including path from <root>."
                  file ext
              ]
        in
        field_o "fdo" (Dune_lang.Syntax.since syntax (2, 0) >>> map string ~f)
      and+ paths =
        let f l =
          match
            Env.Map.of_list (List.map ~f:(fun ((loc, s), _) -> (s, loc)) l)
          with
          | Ok _ -> List.map ~f:(fun ((_, s), x) -> (s, x)) l
          | Error (var, _, loc) ->
            User_error.raise ~loc
              [ Pp.textf
                  "the variable %S can appear at most once in this stanza." var
              ]
        in
        field "paths" ~default:[]
          (Dune_lang.Syntax.since Stanza.syntax (1, 12)
          >>> map ~f (repeat (pair (located string) Ordered_set_lang.decode)))
      and+ instrument_with =
        field ~default:instrument_with "instrument_with"
          (Dune_lang.Syntax.since syntax (2, 7) >>> repeat Lib_name.decode)
      and+ loc = loc in
      Option.iter host_context ~f:(fun _ ->
          match targets with
          | [ Target.Native ] -> ()
          | _ ->
            User_error.raise ~loc
              [ Pp.text
                  "`targets` and `host` options cannot be used in the same \
                   context."
              ]);
      { targets
      ; profile
      ; loc
      ; env
      ; name = Context_name.default
      ; host_context
      ; toolchain
      ; paths
      ; fdo_target_exe
      ; dynamically_linked_foreign_archives
      ; instrument_with
      }
  end

  module Opam = struct
    type t =
      { base : Common.t
      ; switch : string
      ; root : string option
      ; merlin : bool
      }

    let to_dyn { base; switch; root; merlin } =
      let open Dyn.Encoder in
      record
        [ ("base", Common.to_dyn base)
        ; ("switch", string switch)
        ; ("root", option string root)
        ; ("merlin", bool merlin)
        ]

    let equal { base; switch; root; merlin } t =
      Common.equal base t.base
      && String.equal switch t.switch
      && Option.equal String.equal root t.root
      && Bool.equal merlin t.merlin

    let t ~profile ~instrument_with ~x =
      let+ loc_switch, switch = field "switch" (located string)
      and+ name = field_o "name" Context_name.decode
      and+ root = field_o "root" string
      and+ merlin = field_b "merlin"
      and+ base = Common.t ~profile ~instrument_with in
      let name =
        match name with
        | Some s -> s
        | None -> (
          let name = switch ^ Common.fdo_suffix base in
          match Context_name.of_string_opt name with
          | Some s -> s
          | None ->
            User_error.raise ~loc:loc_switch
              [ Pp.textf "Generated context name %S is invalid" name
              ; Pp.text
                  "Please specify a context name manually with the (name ..) \
                   field"
              ])
      in
      let base = { base with targets = Target.add base.targets x; name } in
      { base; switch; root; merlin }
  end

  module Default = struct
    type t = Common.t

    let to_dyn = Common.to_dyn

    let t ~profile ~instrument_with ~x =
      let+ common = Common.t ~profile ~instrument_with
      and+ name =
        field_o "name"
          ( Dune_lang.Syntax.since syntax (1, 10) >>= fun () ->
            Context_name.decode )
      in
      let default =
        (* TODO proper error handling with locs *)
        let name =
          Context_name.to_string common.name ^ Common.fdo_suffix common
        in
        Context_name.parse_string_exn (Loc.none, name)
      in
      let name = Option.value ~default name in
      { common with targets = Target.add common.targets x; name }

    let equal = Common.equal
  end

  type t =
    | Default of Default.t
    | Opam of Opam.t

  let hash = Hashtbl.hash

  let to_dyn =
    let open Dyn.Encoder in
    function
    | Default d -> constr "Default" [ Default.to_dyn d ]
    | Opam o -> constr "Opam" [ Opam.to_dyn o ]

  let equal x y =
    match (x, y) with
    | Default x, Default y -> Default.equal x y
    | Opam x, Opam y -> Opam.equal x y
    | _, _ -> false

  let loc = function
    | Default x -> x.loc
    | Opam x -> x.base.loc

  let host_context = function
    | Default { host_context; _ }
    | Opam { base = { host_context; _ }; _ } ->
      host_context

  let t ~profile ~instrument_with ~x =
    sum
      [ ( "default"
        , fields (Default.t ~profile ~instrument_with ~x) >>| fun x -> Default x
        )
      ; ( "opam"
        , fields (Opam.t ~profile ~instrument_with ~x) >>| fun x -> Opam x )
      ]

  let env = function
    | Default d -> d.env
    | Opam o -> o.base.env

  let name = function
    | Default d -> d.name
    | Opam o -> o.base.name

  let targets = function
    | Default x -> x.targets
    | Opam x -> x.base.targets

  let all_names t =
    let n = name t in
    n
    ::
    List.filter_map (targets t) ~f:(function
      | Native -> None
      | Named s -> Some (Context_name.target n ~toolchain:s))

  let default ?x ?profile ?instrument_with () =
    Default
      { loc = Loc.of_pos __POS__
      ; targets = [ Option.value x ~default:Target.Native ]
      ; profile = Option.value profile ~default:Profile.default
      ; name = Context_name.default
      ; host_context = None
      ; env = Dune_env.Stanza.empty
      ; toolchain = None
      ; paths = []
      ; fdo_target_exe = None
      ; dynamically_linked_foreign_archives = true
      ; instrument_with = Option.value instrument_with ~default:[]
      }
end

module T = struct
  type t =
    { merlin_context : Context_name.t option
    ; contexts : Context.t list
    ; env : Dune_env.Stanza.t
    }

  let to_dyn { merlin_context; contexts; env } =
    let open Dyn.Encoder in
    record
      [ ("merlin_context", option Context_name.to_dyn merlin_context)
      ; ("contexts", list Context.to_dyn contexts)
      ; ("env", Dune_env.Stanza.to_dyn env)
      ]

  let equal { merlin_context; contexts; env } w =
    Option.equal Context_name.equal merlin_context w.merlin_context
    && List.equal Context.equal contexts w.contexts
    && Dune_env.Stanza.equal env w.env
end

include T

let hash { merlin_context; contexts; env } =
  Tuple.T3.hash
    (Option.hash Context_name.hash)
    (List.hash Context.hash) Dune_env.Stanza.hash
    (merlin_context, contexts, env)

include Dune_lang.Versioned_file.Make (struct
  type t = unit
end)

let () = Lang.register syntax ()

let bad_configuration_check map =
  let find_exn loc name host =
    match Context_name.Map.find map host with
    | Some host_ctx -> host_ctx
    | None ->
      User_error.raise ~loc
        [ Pp.textf "Undefined host context '%s' for '%s'."
            (Context_name.to_string host)
            (Context_name.to_string name)
        ]
  in
  let check elt =
    Context.host_context elt
    |> Option.iter ~f:(fun host ->
           let name = Context.name elt in
           let loc = Context.loc elt in
           let host_elt = find_exn loc name host in
           Context.host_context host_elt
           |> Option.iter ~f:(fun host_of_host ->
                  User_error.raise ~loc:(Context.loc host_elt)
                    [ Pp.textf
                        "Context '%s' is both a host (for '%s') and a target \
                         (for '%s')."
                        (Context_name.to_string host)
                        (Context_name.to_string name)
                        (Context_name.to_string host_of_host)
                    ]))
  in
  Context_name.Map.iter map ~f:check

let top_sort contexts =
  let key = Context.name in
  let map =
    Context_name.Map.of_list_map_exn contexts ~f:(fun x -> (key x, x))
  in
  let deps def =
    match Context.host_context def with
    | None -> []
    | Some ctx -> [ Context_name.Map.find_exn map ctx ]
  in
  bad_configuration_check map;
  match Context_name.Top_closure.top_closure ~key ~deps contexts with
  | Ok topo_contexts -> topo_contexts
  | Error _ -> assert false

let t ?x ?profile:cmdline_profile ?instrument_with:cmdline_instrument_with () =
  let* () = Dune_lang.Versioned_file.no_more_lang in
  let* env = env_field in
  let* profile = field "profile" Profile.decode ~default:Profile.default in
  let profile = Option.value cmdline_profile ~default:profile in
  let* instrument_with =
    field "instrument_with"
      (Dune_lang.Syntax.since Stanza.syntax (2, 7) >>> repeat Lib_name.decode)
      ~default:[]
  in
  let instrument_with =
    Option.value cmdline_instrument_with ~default:instrument_with
  in
  let+ contexts =
    multi_field "context" (Context.t ~profile ~instrument_with ~x)
  in
  let defined_names = ref Context_name.Set.empty in
  let merlin_context =
    List.fold_left contexts ~init:None ~f:(fun acc ctx ->
        let name = Context.name ctx in
        if Context_name.Set.mem !defined_names name then
          User_error.raise ~loc:(Context.loc ctx)
            [ Pp.textf "second definition of build context %S"
                (Context_name.to_string name)
            ];
        defined_names :=
          Context_name.Set.union !defined_names
            (Context_name.Set.of_list (Context.all_names ctx));
        match (ctx, acc) with
        | Opam { merlin = true; _ }, Some _ ->
          User_error.raise ~loc:(Context.loc ctx)
            [ Pp.text "you can only have one context for merlin" ]
        | Opam { merlin = true; _ }, None -> Some name
        | _ -> acc)
  in
  let contexts =
    match contexts with
    | [] -> [ Context.default ?x ~profile ~instrument_with () ]
    | _ -> contexts
  in
  let merlin_context =
    match merlin_context with
    | Some _ -> merlin_context
    | None ->
      if
        List.exists contexts ~f:(function
          | Context.Default _ -> true
          | _ -> false)
      then
        Some Context_name.default
      else
        None
  in
  { merlin_context; contexts = top_sort (List.rev contexts); env }

let t ?x ?profile ?instrument_with () =
  fields (t ?x ?profile ?instrument_with ())

let default ?x ?profile ?instrument_with () =
  { merlin_context = Some Context_name.default
  ; contexts = [ Context.default ?x ?profile ?instrument_with () ]
  ; env = Dune_env.Stanza.empty
  }

let load ?x ?profile ?instrument_with p =
  let x = Option.map x ~f:(fun s -> Context.Target.Named s) in
  Io.with_lexbuf_from_file p ~f:(fun lb ->
      if Dune_lexer.eof_reached lb then
        default ?x ?profile ?instrument_with ()
      else
        parse_contents lb ~f:(fun lang ->
            String_with_vars.set_decoding_env
              (Pform.Env.initial lang.version)
              (t ?x ?profile ?instrument_with ())))

let default ?x ?profile ?instrument_with () =
  let x = Option.map x ~f:(fun s -> Context.Target.Named s) in
  default ?x ?profile ?instrument_with ()

let filename = "dune-workspace"

module DB = struct
  module Settings = struct
    type t =
      { x : Context_name.t option
      ; profile : Profile.t option
      ; instrument_with : Lib_name.t list option
      ; path : Path.t option
      }

    let to_dyn { x; profile; instrument_with; path } =
      let open Dyn.Encoder in
      record
        [ ("x", option Context_name.to_dyn x)
        ; ("profile", option Profile.to_dyn profile)
        ; ("instrument_with", option (list Lib_name.to_dyn) instrument_with)
        ; ("path", option Path.to_dyn path)
        ]

    let t = Memo.Run.Fdecl.create to_dyn
  end
end

let init ?x ?profile ?instrument_with ?workspace_file () =
  let path : Path.t option =
    match workspace_file with
    | None ->
      let p = Path.of_string filename in
      Option.some_if (Path.exists p) p
    | Some p ->
      if not (Path.exists p) then
        User_error.raise
          [ Pp.textf "Workspace file %s does not exist"
              (Path.to_string_maybe_quoted p)
          ];
      Some p
  in
  Memo.Run.Fdecl.set DB.Settings.t
    { DB.Settings.x; profile; instrument_with; path }

let workspace =
  let f () =
    let (_ : Memo.Run.t) = Memo.current_run () in
    let { DB.Settings.path; profile; instrument_with; x } =
      Memo.Run.Fdecl.get DB.Settings.t
    in
    match path with
    | None -> default ?x ?profile ?instrument_with ()
    | Some p -> load ?x ?profile ?instrument_with p
  in
  let memo =
    Memo.create "workspaces-db" ~doc:"get all workspaces" ~visibility:Hidden
      ~input:(module Unit)
      ~output:(Allow_cutoff (module T))
      Sync f
  in
  Memo.exec memo
