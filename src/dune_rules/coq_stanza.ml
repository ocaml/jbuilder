open! Dune_engine
open Import
open Dune_lang.Decoder

module Coqpp = struct
  type t =
    { modules : string list
    ; loc : Loc.t
    }

  let decode =
    fields
      (let+ modules = field "modules" (repeat string)
       and+ loc = loc in
       { modules; loc })

  type Stanza.t += T of t

  let p = ("coq.pp", decode >>| fun x -> [ T x ])
end

let coq_syntax =
  Dune_lang.Syntax.create ~name:"coq" ~desc:"the coq extension (experimental)"
    [ ((0, 1), `Since (1, 9))
    ; ((0, 2), `Since (2, 5))
    ; ((0, 3), `Since (2, 8))
    ]

module Buildable = struct
  type t =
    { flags : Ordered_set_lang.Unexpanded.t
    ; mode : Loc.t * Coq_mode.t
    ; libraries : (Loc.t * Lib_name.t) list  (** ocaml libraries *)
    ; theories : (Loc.t * Coq_lib_name.t) list  (** coq libraries *)
    ; loc : Loc.t
    }

  let decode =
    let+ loc = loc
    and+ flags = Ordered_set_lang.Unexpanded.field "flags"
    and+ mode =
      located
        (field "mode" ~default:Coq_mode.VoOnly
           (Dune_lang.Syntax.since coq_syntax (0, 3) >>> Coq_mode.decode))
    and+ libraries =
      field "libraries" (repeat (located Lib_name.decode)) ~default:[]
    and+ theories =
      field "theories"
        (Dune_lang.Syntax.since coq_syntax (0, 2) >>> repeat Coq_lib_name.decode)
        ~default:[]
    in
    { flags; mode; libraries; theories; loc }
end

module Extraction = struct
  type t =
    { (* not a list of modules because we want to preserve whatever case coq
         uses *)
      extracted_modules : string list
    ; prelude : Loc.t * Coq_module.Name.t
    ; buildable : Buildable.t
    }

  let ml_target_fnames t =
    List.concat_map t.extracted_modules ~f:(fun m -> [ m ^ ".ml"; m ^ ".mli" ])

  let decode =
    fields
      (let+ extracted_modules = field "extracted_modules" (repeat string)
       and+ prelude =
         field "prelude" (located (string >>| Coq_module.Name.make))
       and+ buildable = Buildable.decode in
       { prelude; extracted_modules; buildable })

  type Stanza.t += T of t

  let p = ("coq.extraction", decode >>| fun x -> [ T x ])
end

module Theory = struct
  type t =
    { name : Loc.t * Coq_lib_name.t
    ; package : Package.t option
    ; project : Dune_project.t
    ; synopsis : string option
    ; modules : Ordered_set_lang.t
    ; ffi_modules : string list
    ; boot : bool
    ; enabled_if : Blang.t
    ; buildable : Buildable.t
    }

  let coq_public_decode =
    map_validate
      (let+ project = Dune_project.get_exn ()
       and+ loc_name =
         field_o "public_name"
           (Dune_lang.Decoder.plain_string (fun ~loc s -> (loc, s)))
       in
       (project, loc_name))
      ~f:(fun (project, loc_name) ->
        match loc_name with
        | None -> Ok None
        | Some (loc, name) ->
          let pkg =
            match String.lsplit2 ~on:'.' name with
            | None -> Package.Name.of_string name
            | Some (pkg, _) -> Package.Name.of_string pkg
          in
          Stanza_common.Pkg.resolve project pkg
          |> Result.map ~f:(fun pkg -> Some (loc, pkg)))

  let select_deprecation ~package ~public =
    match (package, public) with
    | p, None -> p
    | None, Some (loc, pkg) ->
      User_warning.emit ~loc
        [ Pp.text
            "(public_name ...) is deprecated and will be removed in the Coq \
             language version 1.0, please use (package ...) instead"
        ];
      Some pkg
    | Some _, Some (loc, _) ->
      User_error.raise ~loc
        [ Pp.text
            "Cannot both use (package ...) and (public_name ...), please \
             remove the latter as it is deprecated and will be removed in the \
             1.0 version of the Coq language"
        ]

  let decode =
    fields
      (let+ name = field "name" Coq_lib_name.decode
       and+ package = field_o "package" Stanza_common.Pkg.decode
       and+ project = Dune_project.get_exn ()
       and+ public = coq_public_decode
       and+ synopsis = field_o "synopsis" string
       and+ boot =
         field_b "boot" ~check:(Dune_lang.Syntax.since coq_syntax (0, 2))
       and+ modules = Stanza_common.modules_field "modules"
       and+ ffi_modules = field "ffi_modules" (repeat string)
       and+ enabled_if = Enabled_if.decode ~allowed_vars:Any ~since:None ()
       and+ buildable = Buildable.decode in
       let package = select_deprecation ~package ~public in
       { name
       ; package
       ; project
       ; synopsis
       ; modules
       ; ffi_modules
       ; boot
       ; buildable
       ; enabled_if
       })

  type Stanza.t += T of t

  let coqlib_warn x =
    User_warning.emit ~loc:x.buildable.loc
      [ Pp.text
          "(coqlib ...) is deprecated and will be removed in the Coq language \
           version 1.0, please use (coq.theory ...) instead"
      ];
    x

  let coqlib_p = ("coqlib", decode >>| fun x -> [ T (coqlib_warn x) ])

  let p = ("coq.theory", decode >>| fun x -> [ T x ])

  (* todo add location to the string *)
  let ffi_parse_name (s : string) =
    match Stdune.String.rsplit2 ~on:'.' s with
    | None ->
       User_error.raise
         [ Pp.textf "invalid coqffi module name" ; Pp.verbatim s]
    | Some (lib_name, module_name) ->
       (lib_name, module_name)

  let ffi_target_fnames t =
    List.map t.ffi_modules ~f:(fun m -> ffi_parse_name m |> snd |> (fun m -> m ^ ".v"))

end

let unit_stanzas =
  let+ r = return [ Theory.coqlib_p; Theory.p; Coqpp.p; Extraction.p ] in
  ((), r)

let key = Dune_project.Extension.register coq_syntax unit_stanzas Unit.to_dyn
