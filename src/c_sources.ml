open Stdune
open Dune_file

module Library = Dune_file.Library

type t =
  { libraries : C.Sources.t Lib_name.Map.t
  ; executables : C.Sources.t String.Map.t
  }

let for_lib t ~dir ~name =
  match Lib_name.Map.find t.libraries name with
  | Some m -> m
  | None ->
    Exn.code_error "C_sources.for_lib"
      [ "name", Lib_name.to_sexp name
      ; "dir", Path.to_sexp dir
      ; "available", Sexp.Encoder.(list Lib_name.to_sexp)
                       (Lib_name.Map.keys t.libraries)
      ]

let for_exes t ~first_exe =
  match String.Map.find t.executables first_exe with
  | Some m -> m
  | None ->
    Exn.code_error "C_sources.for_exes"
      [ "first_exe", Sexp.Encoder.string first_exe
      ; "available", Sexp.Encoder.(list string) (String.Map.keys t.executables)
      ]

let empty =
  { libraries = Lib_name.Map.empty
  ; executables = String.Map.empty
  }

let c_name, cxx_name =
  let make what ~loc s =
    if match s with
      | "" | "." | ".."  -> true
      | _ -> false then
      Errors.fail loc "%S is not a valid %s name." s what
    else
      s
  in
  ( make "C"
  , make "C++"
  )

module Eval = struct
  module Value = struct
    type t = string
    type key = string
    let key s = s
  end

  include Ordered_set_lang.Make_loc(String)(Value)
end

let load_sources ~dune_version ~dir ~files =
  let init = C.Kind.Dict.make String.Map.empty in
  String.Set.fold files ~init ~f:(fun fn acc ->
    match C.Kind.split_extension fn ~dune_version with
    | Unrecognized -> acc
    | Not_allowed_until version ->
      let loc = Loc.in_dir dir in
      Errors.warn loc
        "Source file %s with extension %s is not allowed before version %a"
        fn (Filename.extension fn) Syntax.Version.pp version;
      acc
    | Recognized (obj, kind) ->
      let path = Path.relative dir fn in
      C.Kind.Dict.update acc kind ~f:(fun v ->
        String.Map.add v obj (C.Source.make ~kind ~path)
      ))

let make (d : _ Dir_with_dune.t)
      ~(c_sources : C.Source.t String.Map.t C.Kind.Dict.t) =
  let libs, exes =
    List.filter_partition_map d.data ~f:(fun stanza ->
      match (stanza : Stanza.t) with
      | C_executables exes ->
        let eval (kind : C.Kind.t) (c_sources : C.Source.t String.Map.t)
              validate osl =
          Eval.eval_unordered osl
            ~parse:(fun ~loc s ->
              let s = validate ~loc s in
              let s' = Filename.basename s in
              if s' <> s then begin
                Errors.fail loc "relative part of stub are no longer \
                                 necessary and are ignored."
              end;
              s'
            )
            ~standard:String.Map.empty
          |> String.Map.map ~f:(fun (loc, s) ->
            match String.Map.find c_sources s with
            | Some source -> (loc, source)
            | None ->
              Errors.fail loc "%s does not exist as a C source. \
                               %s must be present"
                s (String.enumerate_one_of (C.Kind.possible_fns kind s))
          )
        in
        let names =
          Option.value ~default:Ordered_set_lang.standard in
        let c = eval C.Kind.C c_sources.c c_name (names exes.c_names) in
        let cxx =
          eval C.Kind.Cxx c_sources.cxx cxx_name (names exes.cxx_names) in
        let all = String.Map.union c cxx ~f:(fun _ (_loc1, c) (loc2, cxx) ->
          Errors.fail loc2 "%a and %a have conflicting names. \
                            You must rename one of them."
            Path.pp_in_source (C.Source.path cxx)
            Path.pp_in_source (C.Source.path c)
        ) in
        Right (exes, all)
      | Library lib ->
        let eval (kind : C.Kind.t) (c_sources : C.Source.t String.Map.t)
              validate osl =
          Eval.eval_unordered osl
            ~parse:(fun ~loc s ->
              let s = validate ~loc s in
              let s' = Filename.basename s in
              if s' <> s then begin
                Errors.warn loc "relative part of stub are no longer \
                                 necessary and are ignored."
              end;
              s'
            )
            ~standard:String.Map.empty
          |> String.Map.map ~f:(fun (loc, s) ->
            match String.Map.find c_sources s with
            | Some source -> (loc, source)
            | None ->
              let dune_version = d.dune_version in
              Errors.fail loc "%s does not exist as a C source. \
                               %s must be present"
                s (String.enumerate_one_of
                     (C.Kind.possible_fns kind s ~dune_version))
          )
        in
        let names =
          Option.value ~default:Ordered_set_lang.standard in
        let c = eval C.Kind.C c_sources.c c_name (names lib.c_names) in
        let cxx = eval C.Kind.Cxx c_sources.cxx cxx_name (names lib.cxx_names) in
        let all = String.Map.union c cxx ~f:(fun _ (_loc1, c) (loc2, cxx) ->
          Errors.fail loc2 "%a and %a have conflicting names. \
                            You must rename one of them."
            Path.pp_in_source (C.Source.path cxx)
            Path.pp_in_source (C.Source.path c)
        ) in
        Left (lib, all)
      | _ -> Skip
    )
  in
  let libraries =
    match
      Lib_name.Map.of_list_map libs ~f:(fun (lib, m) ->
        Library.best_name lib, m)
    with
    | Ok x -> x
    | Error (name, _, (lib2, _)) ->
      Errors.fail lib2.buildable.loc
        "Library %a appears for the second time \
         in this directory"
        Lib_name.pp_quoted name
  in
  let executables =
    match
      String.Map.of_list_map exes
        ~f:(fun (exes, m) -> snd (List.hd exes.names), m)
    with
    | Ok x -> x
    | Error (name, _, (exes2, _)) ->
      let loc =
        List.find_map exes2.names ~f:(fun (loc, s) ->
          if s = name then
            Some loc
          else
            None)
        |> Option.value_exn
      in
      Errors.fail loc
        "Executable %S appears for the second time \
         in this directory"
        name
  in
  let () =
    let rev_map =
      List.concat_map libs ~f:(fun (_, c_sources) ->
        String.Map.values c_sources
        |> List.map ~f:(fun (loc, source) ->
          (C.Source.path source, loc)))
      |> Path.Map.of_list
    in
    match rev_map with
    | Ok _ -> ()
    | Error (_, loc1, loc2) ->
      Errors.fail loc2
        "This c stub is already used in another stanza:@\n\
         @[<v>%a@]@\n"
        (Fmt.prefix (Fmt.string "- ") Loc.pp_file_colon_line)
        loc1
  in
  { libraries
  ; executables
  }
