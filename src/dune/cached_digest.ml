open Stdune

type file =
  { mutable digest : Digest.t
  ; mutable timestamp : float
  ; mutable size : int
  ; mutable permissions : Unix.file_perm
  ; mutable stats_checked : int
  }

type t =
  { mutable checked_key : int
  ; mutable max_timestamp : float
  ; table : file Path.Table.t
  }

let db_file = Path.relative Path.build_dir ".digest-db"

module P = Persistent.Make (struct
  type nonrec t = t

  let name = "DIGEST-DB"

  let version = 4
end)

let needs_dumping = ref false

(* CR-soon amokhov: replace this mutable table with a memoized function. This
   will probably require splitting this module in two, for dealing with source
   and target files, respectively. For source files, we receive updates via the
   file-watching API. For target files, we modify the digests ourselves, without
   subscribing for file-watching updates. *)
let cache =
  lazy
    ( match P.load db_file with
    | None ->
      { checked_key = 0; table = Path.Table.create 1024; max_timestamp = 0. }
    | Some cache ->
      cache.checked_key <- cache.checked_key + 1;
      cache )

let get_current_filesystem_time () =
  let special_path = Path.relative Path.build_dir ".filesystem-clock" in
  Io.write_file special_path "<dummy>";
  (Path.stat special_path).st_mtime

let delete_very_recent_entries () =
  let cache = Lazy.force cache in
  let now = get_current_filesystem_time () in
  match Float.compare cache.max_timestamp now with
  | Lt -> ()
  | Eq
  | Gt ->
    Path.Table.filteri_inplace cache.table ~f:(fun ~key:_ ~data ->
        match Float.compare data.timestamp now with
        | Lt -> true
        | Gt
        | Eq ->
          false)

let dump () =
  if !needs_dumping && Path.build_dir_exists () then (
    needs_dumping := false;
    delete_very_recent_entries ();
    P.dump db_file (Lazy.force cache)
  )

let () = Hooks.End_of_build.always dump

let invalidate_cached_timestamps () =
  ( if Lazy.is_val cache then
    let cache = Lazy.force cache in
    cache.checked_key <- cache.checked_key + 1 );
  delete_very_recent_entries ()

let set_max_timestamp cache (stat : Unix.stats) =
  cache.max_timestamp <- Float.max cache.max_timestamp stat.st_mtime

let set_with_stat fn digest stat =
  let cache = Lazy.force cache in
  let permissions = stat.Unix.st_perm in
  needs_dumping := true;
  set_max_timestamp cache stat;
  Path.Table.set cache.table fn
    { digest
    ; timestamp = stat.st_mtime
    ; stats_checked = cache.checked_key
    ; size = stat.st_size
    ; permissions
    }

let set fn digest =
  let stat = Path.stat fn in
  set_with_stat fn digest stat

let refresh_ stats fn =
  let digest = Digest.file_with_stats fn stats in
  set_with_stat fn digest stats;
  digest

let refresh fn =
  let stats = Path.stat fn in
  refresh_ stats fn

let chmod_error_reported = ref false

let refresh_and_chmod fn =
  let stats = Path.stat fn in
  let () =
    (* We remove write permissions to uniformize behavior regardless of whether
       the cache is activated. No need to be zealous in case the file is not
       cached anyway. See issue #3311. *)
    if Cache.cachable stats.st_kind then
      try
        Path.chmod ~stats:(Some stats) ~mode:0o222 ~op:`Remove fn
      with Unix.Unix_error _ ->
        if not !chmod_error_reported then (
          chmod_error_reported := true;
          User_warning.emit ~loc:(Loc.in_file fn)
            [ Pp.textf "unable to remove write permissions from target file" ]
        )
  in
  refresh_ stats fn

let peek_file fn =
  let cache = Lazy.force cache in
  match Path.Table.find cache.table fn with
  | None -> None
  | Some x ->
    Some
      ( if x.stats_checked = cache.checked_key then
        x.digest
      else (
        needs_dumping := true;
        let stat = Path.stat fn in
        let dirty = ref false in
        set_max_timestamp cache stat;
        if stat.st_mtime <> x.timestamp then (
          dirty := true;
          x.timestamp <- stat.st_mtime
        );
        if stat.st_perm <> x.permissions then (
          dirty := true;
          x.permissions <- stat.st_perm
        );
        if stat.st_size <> x.size then (
          dirty := true;
          x.size <- stat.st_size
        );
        if !dirty then x.digest <- Digest.file_with_stats fn stat;
        x.stats_checked <- cache.checked_key;
        x.digest
      ) )

let file fn =
  match peek_file fn with
  | None -> refresh fn
  | Some v -> v

let remove fn =
  let cache = Lazy.force cache in
  needs_dumping := true;
  Path.Table.remove cache.table fn
