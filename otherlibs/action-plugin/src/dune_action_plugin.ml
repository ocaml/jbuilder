module V1 = struct
  module Path = Path
  module Glob = Dune_glob.V1
  open Protocol

  module Execution_error = struct
    exception E of string

    let raise string = raise (E string)

    let raise_on_fs_error = function
      | Error message -> raise message
      | Ok result -> result
  end

  module Fs : sig
    val read_directory : string -> (string list, string) result

    val read_file : string -> (string, string) result

    val write_file : string -> string -> (unit, string) result
  end = struct
    let catch_system_exceptions f ~name =
      try Ok (f ()) with
      | Unix.Unix_error (e, _, _) -> Error (name ^ ": " ^ Unix.error_message e)
      | Sys_error error -> Error (name ^ ": " ^ error)

    let read_directory =
      let rec loop dh acc =
        match Unix.readdir dh with
        | "."
        | ".." ->
          loop dh acc
        | s -> loop dh (s :: acc)
        | exception End_of_file -> acc
      in
      fun path ->
        catch_system_exceptions ~name:"read_directory" (fun () ->
            let dh = Unix.opendir path in
            Stdune.Exn.protect
              ~f:(fun () -> loop dh [] |> List.sort String.compare)
              ~finally:(fun () -> Unix.closedir dh))

    let read_file path =
      catch_system_exceptions ~name:"read_file" (fun () ->
          Stdune.Io.String_path.read_file path)

    let write_file path data =
      catch_system_exceptions ~name:"write_file" (fun () ->
          Stdune.Io.String_path.write_file path data)
  end

  module Stage = struct
    type 'a t =
      { action : unit -> 'a
      ; dependencies : Dependency.Set.t
      ; targets : Stdune.String.Set.t
      }

    let map (t : 'a t) ~f = { t with action = (fun () -> f (t.action ())) }

    let both (t1 : 'a t) (t2 : 'b t) =
      { action = (fun () -> (t1.action (), t2.action ()))
      ; dependencies = Dependency.Set.union t1.dependencies t2.dependencies
      ; targets = Stdune.String.Set.union t1.targets t2.targets
      }
  end

  (* Construction inspired by free monad. *)
  type 'a t =
    | Pure of 'a
    | Stage of 'a t Stage.t

  let lift_stage stage = Stage (Stage.map stage ~f:(fun a -> Pure a))

  let rec map (t : 'a t) ~f =
    match t with
    | Pure a -> Pure (f a)
    | Stage at -> Stage (Stage.map ~f:(map ~f) at)

  let rec stage (t : 'a t) ~f =
    match t with
    | Pure a -> f a
    | Stage at -> Stage (Stage.map ~f:(stage ~f) at)

  let return a = Pure a

  let rec both (t1 : 'a t) (t2 : 'b t) =
    match (t1, t2) with
    | Pure a1, _ -> map ~f:(fun a2 -> (a1, a2)) t2
    | _, Pure a2 -> map ~f:(fun a1 -> (a1, a2)) t1
    | Stage at1, Stage at2 ->
      Stage (Stage.both at1 at2 |> Stage.map ~f:(fun (am1, am2) -> both am1 am2))

  let read_file ~path =
    let path = Path.to_string path in
    let action () = Fs.read_file path |> Execution_error.raise_on_fs_error in
    lift_stage
      { action
      ; dependencies = Dependency.Set.singleton (File path)
      ; targets = Stdune.String.Set.empty
      }

  let write_file ~path ~data =
    let path = Path.to_string path in
    let action () =
      Fs.write_file path data |> Execution_error.raise_on_fs_error
    in
    lift_stage
      { action
      ; dependencies = Dependency.Set.empty
      ; targets = Stdune.String.Set.singleton path
      }

  (* TODO jstaron: If program tries to read empty directory, dune does not copy
     it to `_build` so we get a "No such file or directory" error. *)
  let read_directory_with_glob ~path ~glob =
    let path = Path.to_string path in
    let action () =
      Fs.read_directory path |> Execution_error.raise_on_fs_error
      |> List.filter (Glob.test glob)
    in
    lift_stage
      { action
      ; dependencies =
          Dependency.Set.singleton (Glob { path; glob = Glob.to_string glob })
      ; targets = Stdune.String.Set.empty
      }

  let rec run_by_dune t context =
    match t with
    | Pure () -> Context.respond context Done
    | Stage at ->
      let allowed_targets = Context.targets context in
      let disallowed_targets =
        Stdune.String.Set.diff at.targets allowed_targets
      in
      (match Stdune.String.Set.to_list disallowed_targets with
      | [] -> ()
      | [ t ] ->
        Execution_error.raise
          (Printf.sprintf
             "%s is written despite not being declared as a target in dune \
              file. To fix, add it to target list in dune file."
             t)
      | ts ->
        Execution_error.raise
          (Printf.sprintf
             "Following files were written despite not being declared as \
              targets in dune file:\n\
              %sTo fix, add them to target list in dune file."
             (ts |> String.concat "\n")));
      let prepared_dependencies = Context.prepared_dependencies context in
      let required_dependencies =
        Dependency.Set.diff at.dependencies prepared_dependencies
      in
      if Dependency.Set.is_empty required_dependencies then
        run_by_dune (at.action ()) context
      else
        Context.respond context (Need_more_deps required_dependencies)

  (* If executable is not run by dune, assume that all dependencies are already
     prepared and no target checking is done. *)
  let rec run_outside_of_dune t =
    match t with
    | Pure () -> ()
    | Stage at -> run_outside_of_dune (at.action ())

  let do_run t =
    match Protocol.Context.create () with
    | Run_outside_of_dune -> run_outside_of_dune t
    | Error message ->
      Execution_error.raise
        (Printf.sprintf
           "Error during communication with dune. %s Did you use different \
            dune version to compile the executable?"
           message)
    | Ok context -> run_by_dune t context

  let run t =
    try
      do_run t;
      exit 0
    with
    | Execution_error.E message ->
      prerr_endline message;
      exit 1

  module O = struct
    let ( let+ ) at f = map at ~f

    let ( and+ ) = both
  end

  module Private = struct
    module Protocol = Protocol

    let do_run = do_run

    module Execution_error = Execution_error
  end
end

module Private = V1.Private
