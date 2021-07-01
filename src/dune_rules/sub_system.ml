open! Dune_engine
open! Stdune
open! Import
include Sub_system_intf
open Memo.Build.O

module Register_backend (M : Backend) = struct
  include Sub_system_info.Register (M.Info)

  include Lib.Sub_system.Register (struct
    include M

    type Lib.Sub_system.t += T of t

    let public_info = Some public_info
  end)

  let top_closure l ~deps =
    let open Resolve.Build.O in
    Lib.L.top_closure l ~key:M.lib ~deps >>| function
    | Ok x -> x
    | Error _ ->
      (* Lib.t values can't be cyclic, so we can't have cycles here *)
      assert false

  include Comparable.Make (struct
    type t = M.t

    let to_dyn _ = Dyn.opaque

    let compare a b =
      Lib.Id.compare (Lib.unique_id (M.lib a)) (Lib.unique_id (M.lib b))
  end)

  let resolve db (loc, name) =
    let open Memo.Build.O in
    Resolve.Build.bind
      (Lib.DB.resolve db (loc, name))
      ~f:(fun lib ->
        get lib >>| function
        | None ->
          Resolve.fail
            (User_error.make ~loc
               [ Pp.textf "%s is not %s %s" (Lib_name.to_string name)
                   M.desc_article (M.desc ~plural:false)
               ])
        | Some t -> Resolve.return t)

  module Selection_error = struct
    type t =
      | Too_many_backends of M.t list
      | No_backend_found

    let to_lib_resolve t ~loc =
      match t with
      | Ok x -> Resolve.return x
      | Error err -> (
        match err with
        | Too_many_backends backends ->
          Resolve.fail
            (User_error.make ~loc
               [ Pp.textf "Too many independent %s found:" (M.desc ~plural:true)
               ; Pp.enumerate backends ~f:(fun t ->
                     let lib = M.lib t in
                     let info = Lib.info lib in
                     let src_dir = Lib_info.src_dir info in
                     Pp.textf "%S in %s"
                       (Lib_name.to_string (Lib.name lib))
                       (Path.to_string_maybe_quoted src_dir))
               ])
        | No_backend_found ->
          Resolve.fail
            (User_error.make ~loc
               [ Pp.textf "No %s found." (M.desc ~plural:false) ]))
  end

  open Selection_error

  let written_by_user_or_scan ~written_by_user ~to_scan =
    let open Memo.Build.O in
    (match written_by_user with
    | Some l -> Memo.Build.return l
    | None ->
      Memo.Build.parallel_map to_scan ~f:get >>| List.filter_map ~f:Fun.id)
    >>| function
    | [] -> Error No_backend_found
    | l -> Ok l

  let select_extensible_backends ?written_by_user ~extends to_scan =
    Memo.Build.bind (written_by_user_or_scan ~written_by_user ~to_scan)
      ~f:(function
      | Error _ as err -> Resolve.Build.return err
      | Ok backends ->
        let open Resolve.Build.O in
        let* backends =
          top_closure backends ~deps:(fun s -> Memo.Build.return (extends s))
        in
        let+ roots =
          let all = Set.of_list backends in
          let rec loop acc backends =
            match backends with
            | [] -> Resolve.Build.return (Set.to_list acc)
            | t :: rest ->
              let* x = Memo.Build.return (extends t) in
              let acc = Set.diff acc (Set.of_list x) in
              loop acc rest
          in
          loop all backends
        in
        if List.length roots = 1 then
          Ok backends
        else
          Error (Too_many_backends roots))

  let select_replaceable_backend ?written_by_user ~replaces to_scan =
    Memo.Build.map (written_by_user_or_scan ~written_by_user ~to_scan)
      ~f:(function
      | Error _ as err -> Resolve.return err
      | Ok backends -> (
        let open Resolve.O in
        let+ replaced_backends = Resolve.List.concat_map backends ~f:replaces in
        match
          Set.diff (Set.of_list backends) (Set.of_list replaced_backends)
          |> Set.to_list
        with
        | [ b ] -> Ok b
        | l -> Error (Too_many_backends l)))
end

type Lib.Sub_system.t +=
  | Gen of (Library_compilation_context.t -> unit Memo.Build.t)

module Register_end_point (M : End_point) = struct
  include Sub_system_info.Register (M.Info)

  let gen info (c : Library_compilation_context.t) =
    let open Memo.Build.O in
    let* backends =
      let ( let& ) t f = Resolve.Build.bind t ~f in
      let& deps = Lib.Compile.direct_requires c.compile_info in
      let& pps = Lib.Compile.pps c.compile_info in
      let& written_by_user =
        match M.Info.backends info with
        | None -> Memo.Build.return (Resolve.return None)
        | Some l ->
          let+ l =
            Memo.Build.parallel_map l
              ~f:(M.Backend.resolve (Scope.libs c.scope))
          in
          Resolve.map (Resolve.List.map l ~f:Fun.id) ~f:Option.some
      in
      let& backends =
        M.Backend.select_extensible_backends ?written_by_user
          ~extends:M.Backend.extends (deps @ pps)
      in
      Memo.Build.return
        (M.Backend.Selection_error.to_lib_resolve ~loc:(M.Info.loc info)
           backends)
    in
    match Resolve.peek backends with
    | Ok backends -> M.gen_rules c ~info ~backends
    | Error () ->
      let fail = Action_builder.ignore (Resolve.read backends) in
      Rules.prefix_rules fail ~f:(fun () -> M.gen_rules c ~info ~backends:[])

  include Lib.Sub_system.Register (struct
    module Info = M.Info

    type t = Library_compilation_context.t -> unit Memo.Build.t

    type Lib.Sub_system.t += T = Gen

    let instantiate ~resolve:_ ~get:_ _id info = Memo.Build.return (gen info)

    let public_info = None
  end)
end

let gen_rules (c : Library_compilation_context.t) =
  let* l = Lib.Compile.sub_systems c.compile_info in
  Memo.Build.parallel_iter l ~f:(function
    | Gen gen -> gen c
    | _ -> Memo.Build.return ())
