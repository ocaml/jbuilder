open Stdune

module Id : sig
  type t =
    { unique_id : int
    ; path : Path.t
    ; name : Lib_name.t
    }

  val compare : t -> t -> Ordering.t

  include Comparator.OPS with type t := t

  val make : path:Path.t -> name:Lib_name.t -> t

  module Set : Set.S with type elt = t

  module Map : Map.S with type key = t

  module Top_closure :
    Top_closure.S with type key := t and type 'a monad := 'a Monad.Id.t
end = struct
  module T = struct
    type t =
      { unique_id : int
      ; path : Path.t
      ; name : Lib_name.t
      }

    let compare t1 t2 = Int.compare t1.unique_id t2.unique_id

    let to_dyn _ = Dyn.opaque
  end

  include T

  include (Comparator.Operators (T) : Comparator.OPS with type t := T.t)

  let gen_unique_id =
    let next = ref 0 in
    fun () ->
      let n = !next in
      next := n + 1;
      n

  let make ~path ~name = { unique_id = gen_unique_id (); path; name }

  include Comparable.Make (T)
  module Top_closure = Top_closure.Make (Set) (Monad.Id)
end

module Info = struct
  module T = struct
    type t =
      { info : Lib_info.external_
      ; id : Id.t
      }

    let compare { id; info = _ } t = Id.compare id t.id

    let to_dyn t = Lib_info.to_dyn Path.to_dyn t.info
  end

  include T

  let make info =
    let path = Lib_info.src_dir info in
    let name = Lib_info.name info in
    let id = Id.make ~path ~name in
    { info; id }

  let info (t : t) = t.info

  include (Comparator.Operators (T) : Comparator.OPS with type t := T.t)

  include Comparable.Make (T)
end

type t =
  { parent : t option
  ; resolve : Lib_name.t -> Lib_info.external_ resolve_result
  ; table : (Lib_name.t, status) Table.t
  ; all : Lib_name.t list Lazy.t
  ; stdlib_dir : Path.t
  }

and 'a resolve_result =
  | Not_found
  | Found of 'a
  | Hidden of
      { info : 'a
      ; reason : string
      }
  | Redirect of t option * (Loc.t * Lib_name.t)

and status =
  | St_found of Info.t
  | St_not_found
  | St_hidden of Info.t * string

let create ?parent ~stdlib_dir ~resolve ~all () =
  { parent
  ; resolve
  ; table = Table.create (module Lib_name) 1024
  ; all = Lazy.from_fun all
  ; stdlib_dir
  }

module Library_related_stanza = struct
  type t =
    | Library of Path.Build.t * Dune_file.Library.t
    | External_variant of Dune_file.External_variant.t
    | Deprecated_library_name of Dune_file.Deprecated_library_name.t
end

module Resolve_result = struct
  type nonrec 'a t = 'a resolve_result =
    | Not_found
    | Found of 'a
    | Hidden of
        { info : 'a
        ; reason : string
        }
    | Redirect of t option * (Loc.t * Lib_name.t)

  let to_dyn f (x : _ t) =
    let open Dyn.Encoder in
    match x with
    | Not_found -> constr "Not_found" []
    | Found lib -> constr "Found" [ f lib ]
    | Hidden { info = lib; reason } -> constr "Hidden" [ f lib; string reason ]
    | Redirect (_, (_, name)) -> constr "Redirect" [ Lib_name.to_dyn name ]
end

let error_two_impl_for_variant name variant (loc1, impl1) (loc2, impl2) =
  User_error.raise
    [ Pp.textf "Two implementations of %s have the same variant %S:"
        (Lib_name.Local.to_string name)
        (Variant.to_string variant)
    ; Pp.textf "- %s (%s)" (Lib_name.to_string impl1)
        (Loc.to_file_colon_line loc1)
    ; Pp.textf "- %s (%s)" (Lib_name.to_string impl2)
        (Loc.to_file_colon_line loc2)
    ]

let check_valid_external_variants libmap stanzas =
  List.iter stanzas ~f:(fun (stanza : Library_related_stanza.t) ->
      match stanza with
      | Library _
      | Deprecated_library_name _ ->
        ()
      | External_variant ev -> (
        let loc, virtual_lib = ev.virtual_lib in
        match
          Option.map (Lib_name.Map.find libmap virtual_lib) ~f:(fun res ->
              (* [res] is created by the code in [create_from_library_stanzas]
                 bellow. We know that it is either [Found] or [Redirect (_,
                 name)] where [name] is in [libmap] for sure and maps to [Found
                 _]. *)
              match res with
              | Not_found
              | Hidden _ ->
                assert false
              | Found x -> x
              | Redirect (_, (_, name')) -> (
                match Lib_name.Map.find libmap name' with
                | Some (Found x) -> x
                | _ -> assert false ))
        with
        | None ->
          User_error.raise ~loc
            [ Pp.textf "Virtual library %s hasn't been found in the project."
                (Lib_name.to_string virtual_lib)
            ]
        | Some info -> (
          match Lib_info.virtual_ info with
          | Some _ -> ()
          | None ->
            User_error.raise ~loc
              [ Pp.textf "Library %s isn't a virtual library."
                  (Lib_name.to_string virtual_lib)
              ] ) ))

let create_from_stanzas ?parent ~lib_config stanzas =
  (* Construct a mapping from virtual library name to a list of [(variant,
     implementation_for_this_variant)]. We check a bit later that there is
     duplicate in the inner lists. *)
  let variant_map = Lib_name.Map.empty in
  let variant_map =
    List.fold_left stanzas ~init:variant_map ~f:(fun acc stanza ->
        match (stanza : Library_related_stanza.t) with
        | Library
            ( _
            , ( { implements = Some (_, vlib)
                ; variant = Some variant
                ; buildable = { loc; _ }
                ; _
                } as lib ) ) ->
          Lib_name.Map.Multi.cons acc vlib
            (variant, (loc, Dune_file.Library.best_name lib))
        | External_variant ev ->
          Lib_name.Map.Multi.cons acc (snd ev.virtual_lib)
            (ev.variant, ev.implementation)
        | _ -> acc)
  in
  let map =
    List.concat_map stanzas ~f:(fun stanza ->
        match (stanza : Library_related_stanza.t) with
        | External_variant _ -> []
        | Deprecated_library_name
            { old_public_name = { public = old_public_name; _ }
            ; new_public_name
            ; _
            } ->
          [ ( Dune_file.Public_lib.name old_public_name
            , Redirect (None, new_public_name) )
          ]
        | Library (dir, (conf : Dune_file.Library.t)) -> (
          (* In the [implements] field of library stanzas, the user might use
             either public or private library names. As a result, we have to
             lookup for implementations via both the public and private names. *)
          let variants_private =
            Lib_name.Map.find variant_map (Lib_name.of_local conf.name)
            |> Option.value ~default:[]
          in
          let variants =
            match conf.public with
            | None -> variants_private
            | Some { name = _loc, name; _ } -> (
              if Lib_name.equal name (Lib_name.of_local conf.name) then
                variants_private
              else
                match Lib_name.Map.find variant_map name with
                | None -> variants_private
                | Some variants_public ->
                  List.rev_append variants_private variants_public )
          in
          let known_implementations =
            match Variant.Map.of_list variants with
            | Ok x -> x
            | Error (variant, x, y) ->
              error_two_impl_for_variant (snd conf.name) variant x y
          in
          let info =
            Dune_file.Library.to_lib_info conf ~dir ~lib_config
              ~known_implementations
            |> Lib_info.of_local
          in
          match conf.public with
          | None -> [ (Dune_file.Library.best_name conf, Found info) ]
          | Some p ->
            let name = Dune_file.Public_lib.name p in
            if Lib_name.equal name (Lib_name.of_local conf.name) then
              [ (name, Found info) ]
            else
              [ (name, Found info)
              ; (Lib_name.of_local conf.name, Redirect (None, p.name))
              ] ))
    |> Lib_name.Map.of_list_reducei ~f:(fun name v1 v2 ->
           let res =
             match (v1, v2) with
             | Found info1, Found info2 ->
               Error (Lib_info.loc info1, Lib_info.loc info2)
             | Found info, Redirect (None, (loc, _))
             | Redirect (None, (loc, _)), Found info ->
               Error (loc, Lib_info.loc info)
             | Redirect (None, (loc1, lib1)), Redirect (None, (loc2, lib2)) ->
               if Lib_name.equal lib1 lib2 then
                 Ok v1
               else
                 Error (loc1, loc2)
             | _ ->
               Code_error.raise "create_from_stanzas produced unexpected result"
                 [ ("v1", Resolve_result.to_dyn Dyn.Encoder.opaque v1)
                 ; ("v2", Resolve_result.to_dyn Dyn.Encoder.opaque v2)
                 ]
           in
           match res with
           | Ok x -> x
           | Error (loc1, loc2) ->
             User_error.raise
               [ Pp.textf "Library %s is defined twice:"
                   (Lib_name.to_string name)
               ; Pp.textf "- %s" (Loc.to_file_colon_line loc1)
               ; Pp.textf "- %s" (Loc.to_file_colon_line loc2)
               ])
  in
  (* We need to check that [external_variant] stanzas are correct, i.e. contain
     valid [virtual_library] fields now since this is the last time we analyse
     them. *)
  check_valid_external_variants map stanzas;
  create () ?parent ~stdlib_dir:lib_config.stdlib_dir
    ~resolve:(fun name ->
      Lib_name.Map.find map name |> Option.value ~default:Not_found)
    ~all:(fun () -> Lib_name.Map.keys map)

let create_from_findlib ?(external_lib_deps_mode = false) ~stdlib_dir findlib =
  create () ~stdlib_dir
    ~resolve:(fun name ->
      match Findlib.find findlib name with
      | Ok (Library pkg) -> Found (Dune_package.Lib.info pkg)
      | Ok (Deprecated_library_name d) ->
        Redirect (None, (Loc.none, d.new_public_name))
      | Error e -> (
        match e with
        | Not_found ->
          if external_lib_deps_mode then
            let pkg = Findlib.dummy_package findlib ~name in
            Found (Dune_package.Lib.info pkg)
          else
            Not_found
        | Hidden pkg ->
          Hidden
            { info = Dune_package.Lib.info pkg
            ; reason = "unsatisfied 'exist_if'"
            } ))
    ~all:(fun () ->
      Findlib.all_packages findlib |> List.map ~f:Dune_package.Entry.name)

let instantiate info ~hidden =
  let hidden =
    match hidden with
    | Some _ -> hidden
    | None -> (
      let enabled = Lib_info.enabled info in
      match enabled with
      | Normal
      | Optional ->
        None
      | Disabled_because_of_enabled_if -> Some "unsatisfied 'enabled_if'" )
  in
  let info = Info.make info in
  match hidden with
  | None -> St_found info
  | Some reason -> St_hidden (info, reason)

let rec find_internal db (name : Lib_name.t) : status =
  match Table.find db.table name with
  | Some x -> x
  | None -> resolve_name db name

and resolve_name db name =
  match db.resolve name with
  | Redirect (db', (_, name')) -> (
    let db' = Option.value db' ~default:db in
    match find_internal db' name' with
    | x ->
      Table.add_exn db.table name x;
      x )
  | Found info -> instantiate info ~hidden:None
  | Not_found ->
    let res =
      match db.parent with
      | None -> St_not_found
      | Some db -> find_internal db name
    in
    Table.add_exn db.table name res;
    res
  | Hidden { info; reason = hidden } -> (
    match
      match db.parent with
      | None -> St_not_found
      | Some db -> find_internal db name
    with
    | St_found _ as x ->
      Table.add_exn db.table name x;
      x
    | _ -> instantiate info ~hidden:(Some hidden) )

module Error = struct
  let make ?loc ?hints paragraphs =
    Error (User_error.E (User_error.make ?loc ?hints paragraphs))

  let external_lib_deps_hint () =
    match !Clflags.external_lib_deps_hint with
    | [] -> (* during bootstrap *) []
    | l ->
      [ Pp.box ~indent:2
          (Pp.concat ~sep:Pp.space
             ( Pp.text "try:"
             :: List.map l ~f:(fun x -> Pp.verbatim (String.quote_for_shell x))
             ))
      ]

  let not_found ~loc ~name =
    make ~loc
      [ Pp.textf "Library %S not found." (Lib_name.to_string name) ]
      ~hints:(external_lib_deps_hint ())

  let hidden ~loc ~name ~dir ~reason =
    make ~loc
      [ Pp.textf "Library %S in %s is hidden (%s)." (Lib_name.to_string name)
          (Path.to_string_maybe_quoted dir)
          reason
      ]
      ~hints:(external_lib_deps_hint ())
end

let resolve t (loc, name) =
  match find_internal t name with
  | St_found t -> Ok t
  | St_not_found -> Error.not_found ~loc ~name
  | St_hidden (info, reason) ->
    let dir = Lib_info.src_dir info.info in
    Error.hidden ~loc ~name ~dir ~reason

let find t name =
  match find_internal t name with
  | St_found t -> Some t
  | St_not_found
  | St_hidden _ ->
    None

let rec all ?(recursive = false) (t : t) =
  let l =
    List.fold_left (Lazy.force t.all)
      ~f:(fun libs (name : Lib_name.t) ->
        match find t name with
        | Some x -> Info.Set.add libs x
        | None -> libs)
      ~init:Info.Set.empty
  in
  match (recursive, t.parent) with
  | true, Some t -> Info.Set.union (all ~recursive t) l
  | _ -> l
