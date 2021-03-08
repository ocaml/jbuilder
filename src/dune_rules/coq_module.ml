open! Dune_engine

(* This file is licensed under The MIT License *)
(* (c) MINES ParisTech 2018-2019               *)
(* (c) INRIA 2020                              *)
(* Written by: Emilio Jesús Gallego Arias *)

open! Stdune

module Name = struct
  type t = string

  let make x = x

  let compare = String.compare

  let equal = String.equal

  let to_dyn s = Dyn.String s

  let to_string s = s
end

(* We keep prefix and name separated as the handling of `From Foo Require Bar.`
   may benefit from it. *)
type t =
  { source : Path.Build.t
  ; prefix : string list
  ; name : Name.t
  }

let make ~source ~prefix ~name = { source; prefix; name }

let source x = x.source

let prefix x = x.prefix

let name x = x.name

let build_vo_dir ~obj_dir x =
  List.fold_left x.prefix ~init:obj_dir ~f:Path.Build.relative

let cmxs_of_mod ~wrapper_name x =
  let native_base =
    "N" ^ String.concat ~sep:"_" ((wrapper_name :: x.prefix) @ [ x.name ])
  in
  [ native_base ^ ".cmi"; native_base ^ ".cmxs" ]

let dep_file x ~obj_dir =
  let vo_dir = build_vo_dir ~obj_dir x in
  Path.Build.relative vo_dir (x.name ^ ".v.d")

type target =
  | Vo
  | Vos

type obj_files_mode =
  | Build of target
  | Install

(* XXX: Remove the install .coq-native hack once rules can output targets in
   multiple subdirs *)
let obj_files x ~wrapper_name ~mode ~obj_dir ~obj_files_mode =
  let vo_dir = build_vo_dir ~obj_dir x in
  let install_vo_dir = String.concat ~sep:"/" x.prefix in
  let native_objs =
    match mode with
    | Coq_mode.Native ->
      let cmxs_obj = cmxs_of_mod ~wrapper_name x in
      List.map
        ~f:(fun x ->
          ( Path.Build.relative vo_dir x
          , Filename.(concat (concat install_vo_dir ".coq-native") x) ))
        cmxs_obj
    | VoOnly
    | Legacy ->
      []
  in
  let obj_files =
    match obj_files_mode with
    | Build Vo -> [ x.name ^ ".vo"; "." ^ x.name ^ ".aux"; x.name ^ ".glob" ]
    | Build Vos -> [ x.name ^ ".vos" ]
    | Install -> [ x.name ^ ".vo"; x.name ^ ".vos" ]
  in
  List.map obj_files ~f:(fun fname ->
      (Path.Build.relative vo_dir fname, Filename.concat install_vo_dir fname))
  @ native_objs

let to_dyn { source; prefix; name } =
  let open Dyn.Encoder in
  record
    [ ("source", Path.Build.to_dyn source)
    ; ("prefix", list string prefix)
    ; ("name", Name.to_dyn name)
    ]

let parse ~dir ~loc s =
  let clist = List.rev @@ String.split s ~on:'.' in
  match clist with
  | [] -> User_error.raise ~loc [ Pp.text "Invalid coq module" ]
  | name :: prefix ->
    let prefix = List.rev prefix in
    let source = List.fold_left prefix ~init:dir ~f:Path.Build.relative in
    let source = Path.Build.relative source (name ^ ".v") in
    make ~name ~source ~prefix

let eval =
  let key x = String.concat ~sep:"." (x.prefix @ [ x.name ]) in
  let eq_key x y = String.equal (key x) (key y) in
  fun ~dir ~standard osl ->
    Ordered_set_lang.eval ~parse:(parse ~dir) ~standard ~eq:eq_key osl
