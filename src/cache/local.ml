open Stdune
open Result.O
open Cache_intf

type t =
  { root : Path.t
  ; build_root : Path.t option
  ; info : User_message.Style.t Pp.t list -> unit
  ; warn : User_message.Style.t Pp.t list -> unit
  ; repositories : repository list
  ; command_handler : command -> unit
  ; duplication_mode : Duplication_mode.t
  ; temp_dir : Path.t
  }

module Trimming_result = struct
  type t = { trimmed_bytes : int64 }

  let empty = { trimmed_bytes = 0L }

  (* CR-someday amokhov: Right now Dune doesn't support large (>1Gb) files on
     32-bit platforms due to the pervasive use of [int] for representing
     individual file sizes. It's not fundamentally difficult to switch to
     [int64], so we should do it if it becomes a real issue. *)
  let add t ~(bytes : int) =
    { trimmed_bytes = Int64.add t.trimmed_bytes (Int64.of_int bytes) }
end

let default_root () =
  Path.L.relative
    (Path.of_filename_relative_to_initial_cwd Xdg.cache_dir)
    [ "dune"; "db" ]

let file_store_root cache = Path.L.relative cache.root [ "files"; "v4" ]

let metadata_store_root cache = Path.L.relative cache.root [ "meta"; "v4" ]

(* A file storage scheme. *)
module type FSScheme = sig
  (* Given a cache root and a file digest, determine the location of the file in
     the cache. *)
  val path : root:Path.t -> Digest.t -> Path.t

  (* Given a cache root, list all files stored in the cache. *)
  val list : root:Path.t -> Path.t list
end

(* A file storage scheme where a file with a digest [d] is stored in a
   subdirectory whose name is made of the first two characters of [d], that is:

   [<root>/<first-two-characters-of-d>/<d>]

   CR-someday amokhov: We currently do not provide support for collisions where
   two or more files with different content have the same content digest [d]. If
   this ever becomes a real (i.e. not hypothetical) problem, a good way forward
   would be to switch from MD5 to SHA1 or higher, making the chance of this
   happening even lower, and at the same time making it more difficult to create
   deliberate collisions. *)
module FirstTwoCharsSubdir : FSScheme = struct
  let path ~root digest =
    let digest = Digest.to_string digest in
    let first_two_chars = String.sub digest ~pos:0 ~len:2 in
    Path.L.relative root [ first_two_chars; digest ]

  (* List all entries in a given cache root. Returns the empty list of the root
     doesn't exist. *)
  let list ~root =
    let open Result.O in
    let f dir =
      let root = Path.L.relative root [ dir ] in
      if String.for_all ~f:Char.is_lowercase_hex dir then
        let+ paths = Path.readdir_unsorted root in
        List.map ~f:(Path.relative root) paths
      else
        Ok []
    in
    match Path.readdir_unsorted root >>= Result.List.concat_map ~f with
    | Ok res -> res
    | Error ENOENT -> []
    | Error e -> User_error.raise [ Pp.text (Unix.error_message e) ]
end

module FSSchemeImpl = FirstTwoCharsSubdir

let metadata_path cache key =
  FSSchemeImpl.path ~root:(metadata_store_root cache) key

let file_path cache key = FSSchemeImpl.path ~root:(file_store_root cache) key

(* Support for an older version of the cache. *)
module V3 = struct
  let file_store_root cache = Path.L.relative cache.root [ "files"; "v3" ]

  let metadata_store_root cache = Path.L.relative cache.root [ "meta"; "v3" ]

  let file_path cache key = FSSchemeImpl.path ~root:(file_store_root cache) key
end

module Metadata_file = struct
  type t =
    { metadata : Sexp.t list
    ; files : File.t list
    }

  let to_sexp { metadata; files } =
    let open Sexp in
    let f ({ path; digest } : File.t) =
      Sexp.List
        [ Sexp.Atom (Path.Local.to_string (Path.Build.local path))
        ; Sexp.Atom (Digest.to_string digest)
        ]
    in
    List
      [ List (Atom "metadata" :: metadata)
      ; List (Atom "files" :: List.map ~f files)
      ]

  let of_sexp = function
    | Sexp.List
        [ List (Atom "metadata" :: metadata); List (Atom "files" :: produced) ]
      ->
      let+ files =
        Result.List.map produced ~f:(function
          | List [ Atom path; Atom digest ] ->
            let path = Path.Build.of_string path in
            let+ digest =
              match Digest.from_hex digest with
              | Some digest -> Ok digest
              | None -> Error "Invalid digest in cache metadata"
            in
            { File.path; digest }
          | _ -> Error "Invalid list of produced files in cache metadata")
      in
      { metadata; files }
    | _ -> Error "Invalid cache metadata"

  let of_string s =
    match Csexp.parse_string s with
    | Ok sexp -> of_sexp sexp
    | Error (_, msg) -> Error msg

  let to_string f = to_sexp f |> Csexp.to_string

  let parse path = Io.with_file_in path ~f:Csexp.input >>= of_sexp
end

let tmp_path cache name =
  let res = Path.relative cache.temp_dir name in
  Path.mkdir_p res;
  res

let make_path cache path =
  match cache.build_root with
  | Some p -> Result.ok (Path.append_local p path)
  | None ->
    Result.Error
      (sprintf "relative path %s while no build root was set"
         (Path.Local.to_string_maybe_quoted path))

let with_repositories cache repositories = Result.Ok { cache with repositories }

let duplicate ?(duplication = None) cache ~src ~dst =
  match Option.value ~default:cache.duplication_mode duplication with
  | Copy -> Io.copy_file ~src ~dst ()
  | Hardlink -> Path.link src dst

let retrieve cache (file : File.t) =
  let src = file_path cache file.digest in
  let dst = Path.build file.path in
  cache.info
    [ Pp.textf "retrieve %s from cache" (Path.to_string_maybe_quoted dst) ];
  duplicate cache ~src ~dst;
  dst

let deduplicate cache (file : File.t) =
  match cache.duplication_mode with
  | Copy -> ()
  | Hardlink -> (
    let path = Path.build file.path in
    let path_in_cache = file_path cache file.digest in
    let tmpname = Path.build (Path.Build.of_string ".dedup") in
    cache.info
      [ Pp.textf "deduplicate %s from %s" (Path.to_string path)
          (Path.to_string path_in_cache)
      ];
    try
      Path.unlink_no_err tmpname;
      Path.link path_in_cache tmpname;
      Path.rename tmpname path
    with Unix.Unix_error (e, syscall, _) ->
      Path.unlink_no_err tmpname;
      cache.warn
        [ Pp.textf "error handling dune-cache command: %s: %s" syscall
            (Unix.error_message e)
        ] )

let apply ~f o v =
  match o with
  | Some o -> f v o
  | None -> v

let promote_sync cache paths key metadata ~repository ~duplication =
  let open Result.O in
  let* repo =
    match repository with
    | None -> Result.Ok None
    | Some idx -> (
      match List.nth cache.repositories idx with
      | None -> Result.Error (Printf.sprintf "repository out of range: %i" idx)
      | repo -> Result.Ok repo )
  in
  let metadata =
    apply
      ~f:(fun metadata repository ->
        metadata
        @ [ Sexp.List [ Sexp.Atom "repo"; Sexp.Atom repository.remote ]
          ; Sexp.List [ Sexp.Atom "commit_id"; Sexp.Atom repository.commit ]
          ])
      repo metadata
  in
  let promote (path, expected_digest) =
    let* abs_path = make_path cache (Path.Build.local path) in
    cache.info [ Pp.textf "promote %s" (Path.to_string abs_path) ];
    let stat = Path.lstat abs_path in
    let* stat =
      if stat.st_kind = S_REG then
        Result.Ok stat
      else
        Result.Error
          (Format.sprintf "invalid file type: %s"
             (Path.string_of_file_kind stat.st_kind))
    in
    (* Create a duplicate (either a [Copy] or a [Hardlink] depending on the
       [duplication] setting) of the promoted file in a temporary directory to
       correctly handle the situation when the file is modified or deleted
       during the promotion process. *)
    Temp.with_temp_path ~dir:cache.temp_dir ~prefix:"temp" ~suffix:"data"
      ~f:(function
      | Error exn ->
        let message =
          sprintf "Failed to create a temp file: %s"
            (Exn.to_dyn exn |> Dyn.to_string)
        in
        cache.info [ Pp.text message ];
        Result.Error message
      | Ok tmp -> (
        duplicate ~duplication cache ~src:abs_path ~dst:tmp;
        let effective_digest = Digest.file_with_stats tmp (Path.stat tmp) in
        if Digest.compare effective_digest expected_digest != Ordering.Eq then (
          let message =
            Printf.sprintf "digest mismatch: %s != %s"
              (Digest.to_string effective_digest)
              (Digest.to_string expected_digest)
          in
          cache.info [ Pp.text message ];
          Result.Error message
        ) else
          let in_the_cache = file_path cache effective_digest in
          (* CR-someday: we assume that if the file with [effective_digest]
             exists in the file storage, then its content matches the digest,
             i.e. the user never modifies it. In principle, we could add a
             consistency check but this would have a non-negligible performance
             cost. A good compromise seems to be to add a "paranoid" mode to
             Dune cache where we always check file contents for consistency with
             the expected digest, so one could enable it when needed. In the
             paranoid mode, we could furthermore check for a digest collision
             via [Io.compare_files in_the_cache tmp]. *)
          match Path.exists in_the_cache with
          | true ->
            (* Update the timestamp of the existing cache entry, moving it to
               the back of the trimming queue. *)
            Path.touch in_the_cache;
            Result.Ok (Already_promoted { path; digest = effective_digest })
          | false ->
            Path.mkdir_p (Path.parent_exn in_the_cache);
            (* Move the temporary file to the cache. *)
            Path.rename tmp in_the_cache;
            (* Remove write permissions, making the cache entry immutable. We
               assume that users do not modify the files in the cache. *)
            Path.chmod in_the_cache ~mode:(stat.st_perm land 0o555);
            Result.Ok (Promoted { path; digest = effective_digest }) ))
  in
  let+ promoted = Result.List.map ~f:promote paths in
  let metadata_path = metadata_path cache key
  and metadata_tmp_path = Path.relative cache.temp_dir "metadata"
  and files =
    List.map promoted ~f:(function
        | Already_promoted f
        | Promoted f
        -> f)
  in
  let metadata_file : Metadata_file.t = { metadata; files } in
  let metadata = Csexp.to_string (Metadata_file.to_sexp metadata_file) in
  Io.write_file metadata_tmp_path metadata;
  let () =
    match Io.read_file metadata_path with
    | contents ->
      if contents <> metadata then
        User_warning.emit
          [ Pp.textf "non reproductible collision on rule %s"
              (Digest.to_string key)
          ]
    | exception Sys_error _ -> Path.mkdir_p (Path.parent_exn metadata_path)
  in
  Path.rename metadata_tmp_path metadata_path;
  (* The files that have already been present in the cache can be deduplicated,
     i.e. replaced with hardlinks to their cached copies. *)
  ( match cache.duplication_mode with
  | Copy -> ()
  | Hardlink ->
    List.iter promoted ~f:(function
      | Already_promoted file -> cache.command_handler (Dedup file)
      | _ -> ()) );
  (metadata_file, promoted)

let promote cache paths key metadata ~repository ~duplication =
  Result.map ~f:ignore
    (promote_sync cache paths key metadata ~repository ~duplication)

let search cache key =
  let path = metadata_path cache key in
  let* sexp =
    try Io.with_file_in path ~f:Csexp.input
    with Sys_error _ -> Error "no cached file"
  in
  let+ metadata = Metadata_file.of_sexp sexp in
  (* Touch cache files so they are removed last by LRU trimming. *)
  let () =
    let f (file : File.t) =
      (* There is no point in trying to trim out files that are missing : dune
         will have to check when hardlinking anyway since they could disappear
         inbetween. *)
      try Path.touch ~create:false (file_path cache file.digest)
      with Unix.(Unix_error (ENOENT, _, _)) -> ()
    in
    List.iter ~f metadata.files
  in
  (metadata.metadata, metadata.files)

let set_build_dir cache p = Result.Ok { cache with build_root = Some p }

let teardown cache = Temp.destroy Dir cache.temp_dir

let hint _ _ = Result.Ok ()

let detect_duplication_mode root =
  let () = Path.mkdir_p root in
  let beacon = Path.relative root "beacon"
  and target = Path.relative Path.build_dir ".cache-beacon" in
  let () = Path.touch beacon in
  let rec test () =
    match Path.link beacon target with
    | exception Unix.Unix_error (Unix.EEXIST, _, _) ->
      Path.unlink_no_err target;
      test ()
    | exception Unix.Unix_error _ -> Duplication_mode.Copy
    | () -> Duplication_mode.Hardlink
  in
  test ()

let make ?(root = default_root ())
    ?(duplication_mode = detect_duplication_mode root)
    ?(log = Dune_util.Log.info) ?(warn = fun pp -> User_warning.emit pp)
    ~command_handler () =
  let res =
    { root
    ; build_root = None
    ; info = log
    ; warn
    ; repositories = []
    ; command_handler
    ; duplication_mode
    ; temp_dir =
        (* CR-someday amokhov: Introduce [val getpid : unit -> t] in [pid.ml] so
           that we don't use the untyped version of pid anywhere. *)
        Temp.temp_in_dir ~perms:0o700 Temp.Dir ~dir:root ~prefix:"promoting."
          ~suffix:("." ^ string_of_int (Unix.getpid ()))
    }
  in
  match
    Path.mkdir_p @@ file_store_root res;
    Path.mkdir_p @@ metadata_store_root res
  with
  | () -> Ok res
  | exception exn ->
    Error
      ("Unable to set up the cache root directory: " ^ Printexc.to_string exn)

let duplication_mode cache = cache.duplication_mode

let trim_bad_metadata_files ~metadata_files ~trimmed_so_far ~file_path cache =
  List.fold_left metadata_files ~init:trimmed_so_far
    ~f:(fun trimmed_so_far path ->
      let should_be_removed =
        match Metadata_file.parse path with
        | Result.Error msg ->
          cache.warn
            [ Pp.textf "remove invalid metadata file %s: %s"
                (Path.to_string_maybe_quoted path)
                msg
            ];
          true
        | Result.Ok { Metadata_file.files; _ } ->
          let is_broken =
            List.exists files ~f:(fun { File.digest; _ } ->
                let reference = file_path cache digest in
                not (Path.exists reference))
          in
          if is_broken then
            cache.info
              [ Pp.textf "remove metadata file %s as it refers to missing files"
                  (Path.to_string_maybe_quoted path)
              ];
          is_broken
      in
      match should_be_removed with
      | true ->
        let bytes = (Path.stat path).st_size in
        Path.unlink_no_err path;
        Trimming_result.add trimmed_so_far ~bytes
      | false -> trimmed_so_far)

let garbage_collect_impl ~trimmed_so_far cache =
  let metadata_files = FSSchemeImpl.list ~root:(V3.metadata_store_root cache) in
  let trimmed_so_far =
    trim_bad_metadata_files ~metadata_files ~trimmed_so_far
      ~file_path:V3.file_path cache
  in
  let metadata_files = FSSchemeImpl.list ~root:(metadata_store_root cache) in
  trim_bad_metadata_files ~metadata_files ~trimmed_so_far ~file_path cache

let garbage_collect = garbage_collect_impl ~trimmed_so_far:Trimming_result.empty

(* We call a cached file "unused" if there are currently no hard links to it
   from build directories. Note that [st_nlink] can return 0 if the file has
   been removed since we scanned the tree -- in this case we do not want to
   claim that its removal is the result of cache trimming and we, therefore,
   skip it while trimming. *)
let file_exists_and_is_unused ~stats = stats.Unix.st_nlink = 1

let files_in_cache_for_all_supported_versions cache =
  let files = FSSchemeImpl.list ~root:(file_store_root cache) in
  let files_v3 = FSSchemeImpl.list ~root:(V3.file_store_root cache) in
  files @ files_v3

let trim cache ~goal =
  let files = files_in_cache_for_all_supported_versions cache in
  let f path =
    let stats = Path.stat path in
    if file_exists_and_is_unused ~stats then
      Some (path, stats.st_size, stats.st_ctime)
    else
      None
  and compare (_, _, t1) (_, _, t2) = Ordering.of_int (Stdlib.compare t1 t2) in
  let files = List.sort ~compare (List.filter_map ~f files)
  and delete (trimmed_so_far : Trimming_result.t) (path, bytes, _) =
    if trimmed_so_far.trimmed_bytes >= goal then
      trimmed_so_far
    else (
      Path.unlink path;
      (* CR-someday amokhov: We should really be using block_size * #blocks
         because that's how much we save actually. *)
      Trimming_result.add trimmed_so_far ~bytes
    )
  in
  let trimmed_so_far =
    List.fold_left ~init:Trimming_result.empty ~f:delete files
  in
  garbage_collect_impl cache ~trimmed_so_far

let overhead_size cache =
  let files = files_in_cache_for_all_supported_versions cache in
  let stats =
    let f p =
      try
        let stats = Path.stat p in
        if file_exists_and_is_unused ~stats then
          Int64.of_int stats.st_size
        else
          0L
      with Unix.Unix_error (Unix.ENOENT, _, _) -> 0L
    in
    List.map ~f files
  in
  List.fold_left ~f:Int64.add ~init:0L stats
