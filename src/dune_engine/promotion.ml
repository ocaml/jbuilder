open Import

let staging_area = Path.Build.relative Path.Build.root ".promotion-staging"

module File = struct
  type t =
    { src : Path.Build.t
    ; staging : Path.Build.t option
    ; dst : Path.Source.t
    }

  let in_staging_area source = Path.Build.append_source staging_area source

  let to_dyn { src; staging; dst } =
    let open Dyn.Encoder in
    record
      [ ("src", Path.Build.to_dyn src)
      ; ("staging", option Path.Build.to_dyn staging)
      ; ("dst", Path.Source.to_dyn dst)
      ]

  let db : t list ref = ref []

  let register_dep ~source_file ~correction_file =
    db :=
      { src = snd (Path.Build.split_sandbox_root correction_file)
      ; staging = None
      ; dst = source_file
      }
      :: !db

  let register_intermediate ~source_file ~correction_file =
    let staging = in_staging_area source_file in
    Path.mkdir_p (Path.build (Option.value_exn (Path.Build.parent staging)));
    Unix.rename
      (Path.Build.to_string correction_file)
      (Path.Build.to_string staging);
    let src = snd (Path.Build.split_sandbox_root correction_file) in
    db := { src; staging = Some staging; dst = source_file } :: !db

  let promote { src; staging; dst } =
    let correction_file = Option.value staging ~default:src in
    let correction_exists = Path.exists (Path.build correction_file) in
    Console.print
      [ Pp.box ~indent:2
          (if correction_exists then
            Pp.textf "Promoting %s to %s."
              (Path.to_string_maybe_quoted (Path.build src))
              (Path.Source.to_string_maybe_quoted dst)
          else
            Pp.textf "Skipping promotion of %s to %s as the %s is missing."
              (Path.to_string_maybe_quoted (Path.build src))
              (Path.Source.to_string_maybe_quoted dst)
              (match staging with
              | None -> "file"
              | Some staging ->
                Format.sprintf "staging file (%s)"
                  (Path.to_string_maybe_quoted (Path.build staging))))
      ];
    if correction_exists then
      let chmod perms = perms lor 0o200 in
      Io.copy_file ~chmod
        ~src:(Path.build correction_file)
        ~dst:(Path.source dst) ()
end

let clear_cache () = File.db := []

let () = Hooks.End_of_build.always clear_cache

module P = Persistent.Make (struct
  type t = File.t list

  let name = "TO-PROMOTE"

  let version = 2
end)

let db_file = Path.relative Path.build_dir ".to-promote"

let dump_db db =
  if Path.build_dir_exists () then
    match db with
    | [] -> if Path.exists db_file then Path.unlink_no_err db_file
    | l -> P.dump db_file l

let load_db () = Option.value ~default:[] (P.load db_file)

let group_by_targets db =
  List.map db ~f:(fun { File.src; staging; dst } -> (dst, (src, staging)))
  |> Path.Source.Map.of_list_multi
  (* Sort the list of possible sources for deterministic behavior *)
  |> Path.Source.Map.map
       ~f:(List.sort ~compare:(fun (x, _) (y, _) -> Path.Build.compare x y))

type files_to_promote =
  | All
  | These of Path.Source.t list * (Path.Source.t -> unit)

let do_promote db files_to_promote =
  let by_targets = group_by_targets db in
  let potential_build_contexts =
    match Path.readdir_unsorted_with_kinds Path.build_dir with
    | exception _ -> []
    | Error _ -> []
    | Ok files ->
      List.filter_map files ~f:(function
        | fn, Unix.S_DIR ->
          if fn = "" || fn.[0] = '.' || fn = "install" then
            None
          else
            Some (Path.(relative build_dir) fn)
        | _ -> None)
  in
  let dirs_to_clear_from_cache = Path.root :: potential_build_contexts in
  let promote_one dst srcs =
    match srcs with
    | [] -> assert false
    | (src, staging) :: others ->
      (* We remove the files from the digest cache to force a rehash on the next
         run. We do this because on OSX [mtime] is not precise enough and if a
         file is modified and promoted quickly, it will look like it hasn't
         changed even though it might have.

         aalekseyev: this is probably unnecessary now, depending on when
         [do_promote] runs (before or after [invalidate_cached_timestamps]) *)
      List.iter dirs_to_clear_from_cache ~f:(fun dir ->
          Cached_digest.remove (Path.append_source dir dst));
      File.promote { src; staging; dst };
      List.iter others ~f:(fun (path, _staging) ->
          Format.eprintf " -> ignored %s.@."
            (Path.to_string_maybe_quoted (Path.build path)))
  in
  match files_to_promote with
  | All ->
    Path.Source.Map.iteri by_targets ~f:promote_one;
    []
  | These (files, on_missing) ->
    let files = Path.Source.Set.of_list files |> Path.Source.Set.to_list in
    let by_targets =
      List.fold_left files ~init:by_targets ~f:(fun map fn ->
          match Path.Source.Map.find by_targets fn with
          | None ->
            on_missing fn;
            map
          | Some srcs ->
            promote_one fn srcs;
            Path.Source.Map.remove by_targets fn)
    in
    Path.Source.Map.to_list by_targets
    |> List.concat_map ~f:(fun (dst, srcs) ->
           List.map srcs ~f:(fun (src, staging) -> { File.src; staging; dst }))

let finalize () =
  let db =
    match !Clflags.promote with
    | Some Automatically -> do_promote !File.db All
    | Some Never
    | None ->
      !File.db
  in
  dump_db db

let promote_files_registered_in_last_run files_to_promote =
  let db = load_db () in
  let db = do_promote db files_to_promote in
  dump_db db
