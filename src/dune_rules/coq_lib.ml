open! Dune_engine

(* This file is licensed under The MIT License *)
(* (c) MINES ParisTech 2018-2019               *)
(* (c) INRIA 2020                              *)
(* Written by: Emilio Jesús Gallego Arias *)

open! Stdune

(* At some point we may want to reify this into resolved status, for example:

   ; libraries : Lib.t list Or_exn.t

   etc.. *)
type t =
  { name : Loc.t * Coq_lib_name.t
  ; wrapper : string
  ; implicit : bool (* Only useful for the stdlib *)
  ; src_root : Path.Build.t
  ; obj_root : Path.Build.t
  ; theories : (Loc.t * Coq_lib_name.t) list
  ; libraries : (Loc.t * Lib_name.t) list
  ; package : Package.t option
  }

let name l = snd l.name

let implicit l = l.implicit

let location l = fst l.name

let wrapper l = l.wrapper

let src_root l = l.src_root

let obj_root l = l.obj_root

let libraries l = l.libraries

let package l = l.package

module Error = struct
  let make ?loc ?hints paragraphs =
    Error
      (User_error.E
         (User_error.make ?loc ?hints paragraphs, User_error.Annotations.none))

  let duplicate_theory_name theory =
    let loc, name = theory.Coq_stanza.Theory.name in
    let name = Coq_lib_name.to_string name in
    make ~loc [ Pp.textf "Duplicate theory name: %s" name ]

  let theory_not_found ~loc name =
    let name = Coq_lib_name.to_string name in
    make ~loc [ Pp.textf "Theory %s not found" name ]

  let private_deps_not_allowed ~loc private_dep =
    let name = Coq_lib_name.to_string (name private_dep) in
    make ~loc
      [ Pp.textf
          "Theory %S is private, it cannot be a dependency of a public theory. \
           You need to associate %S to a package."
          name name
      ]

  let duplicate_boot_lib ~loc boot_theory =
    let name =
      Coq_lib_name.to_string (snd boot_theory.Coq_stanza.Theory.name)
    in
    make ~loc [ Pp.textf "Cannot have more than one boot library: %s)" name ]

  let cycle_found ~loc cycle =
    make ~loc
      [ Pp.textf "Cycle found"
      ; Pp.enumerate cycle ~f:(fun t ->
            Pp.text (Coq_lib_name.to_string (snd t.name)))
      ]
end

module DB = struct
  type lib = t

  type nonrec t =
    { boot : (Loc.t * t) option
    ; libs : t Coq_lib_name.Map.t
    }

  let boot_library { boot; _ } = boot

  let create_from_stanza ((dir, s) : Path.Build.t * Coq_stanza.Theory.t) =
    let name = snd s.name in
    ( name
    , { name = s.name
      ; wrapper = Coq_lib_name.wrapper name
      ; implicit = s.boot
      ; obj_root = dir
      ; src_root = dir
      ; theories = s.buildable.theories
      ; libraries = s.buildable.libraries
      ; package = s.package
      } )

  (* Should we register errors and printers, or raise is OK? *)
  let create_from_coqlib_stanzas sl =
    let libs =
      match Coq_lib_name.Map.of_list_map ~f:create_from_stanza sl with
      | Ok m -> m
      | Error (_name, _w1, (_, w2)) ->
        Result.ok_exn (Error.duplicate_theory_name w2)
    in
    let boot =
      match List.find_all ~f:(fun (_, s) -> s.Coq_stanza.Theory.boot) sl with
      | [] -> None
      | [ l ] -> Some ((snd l).buildable.loc, snd (create_from_stanza l))
      | _ :: (_, s2) :: _ ->
        Result.ok_exn (Error.duplicate_boot_lib ~loc:s2.buildable.loc s2)
    in
    { boot; libs }

  let resolve ?(allow_private_deps = true) db (loc, name) =
    match Coq_lib_name.Map.find db.libs name with
    | Some s ->
      if (not allow_private_deps) && Option.is_none s.package then
        Error.private_deps_not_allowed ~loc s
      else
        Ok s
    | None -> Error.theory_not_found ~loc name

  let find_many t ~loc = Result.List.map ~f:(fun name -> resolve t (loc, name))

  module Coq_lib_closure = Top_closure.Make (String.Set) (Or_exn)

  let add_boot db theories =
    match db.boot with
    | None -> theories
    (* XXX: Note that this means that we will prefix Coq with -Q, not sure we
       want to do that (yet), but seems like good practice. *)
    | Some (loc, stdlib) -> (loc, snd stdlib.name) :: theories

  let top_closure db theories ~allow_private_deps ~loc =
    let open Result.O in
    let* theories =
      add_boot db theories
      |> Result.List.map ~f:(resolve ~allow_private_deps db)
    in
    let key (t : lib) = Coq_lib_name.to_string (snd t.name) in
    let deps (t : lib) =
      Result.List.map ~f:(resolve ~allow_private_deps db) t.theories
    in
    match Coq_lib_closure.top_closure theories ~key ~deps with
    | Ok (Ok s) -> Ok s
    | Ok (Error cycle) -> Error.cycle_found ~loc cycle
    | Error exn -> Error exn

  let requires db (t : lib) : lib list Or_exn.t =
    let allow_private_deps = Option.is_none t.package in
    let loc = location t in
    top_closure db t.theories ~loc ~allow_private_deps

  let requires_for_user_written db = function
    | [] -> Ok []
    | start :: xs as theories ->
      let loc =
        let stop = Option.value (List.last xs) ~default:start in
        Loc.span (fst start) (fst stop)
      in
      top_closure db theories ~loc ~allow_private_deps:true

  let resolve db l = resolve db l
end
