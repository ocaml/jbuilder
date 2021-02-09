open Stdune
open Result.O
open Cache_intf
include Messages_intf

let invalid_args args =
  Result.Error
    (Printf.sprintf "invalid arguments:%s"
       (String.concat ~sep:" " (List.map ~f:Sexp.to_string args)))

let version_at_least ~min v =
  v.major > min.major || (v.major = min.major && v.minor >= min.minor)

let string_of_version { major; minor } = sprintf "%i.%i" major minor

let dyn_of_version { major; minor } =
  Dyn.Encoder.record
    [ ("major", Dyn.Encoder.int major); ("minor", Dyn.Encoder.int minor) ]

let hint_min_version = { major = 1; minor = 2 }

let hint_supported version = version_at_least ~min:hint_min_version version

let sexp_of_message : type a. version -> a message -> Sexp.t =
 fun version ->
  let cmd name args = Sexp.List (Sexp.Atom name :: args) in
  function
  | Hint keys ->
    if not (hint_supported version) then
      Code_error.raise "tried sending a not yet supported hint message"
        [ ("current version", dyn_of_version version)
        ; ("minimum version", dyn_of_version hint_min_version)
        ];
    let f k = Sexp.Atom (Digest.to_string k) in
    cmd "hint" @@ List.map ~f keys
  | Lang versions ->
    cmd "lang"
      (Sexp.Atom "dune-cache-protocol"
       ::
       (List.map ~f:(fun { major; minor } ->
            Sexp.List
              [ Sexp.Atom (string_of_int major)
              ; Sexp.Atom (string_of_int minor)
              ]))
         versions)
  | Promote promotion ->
    let key = Key.to_string promotion.key
    and f (path, digest) =
      Sexp.List
        [ Sexp.Atom (Path.Local.to_string (Path.Build.local path))
        ; Sexp.Atom (Digest.to_string digest)
        ]
    in
    let rest = [] in
    let rest =
      match promotion.duplication with
      | Some mode
        when version = { major = 1; minor = 0 } && mode = Duplication_mode.Copy
        ->
        User_error.raise
          [ Pp.textf "cache daemon v1.0 does not support copy duplication mode"
          ]
      | Some mode ->
        Sexp.List
          [ Sexp.Atom "duplication"
          ; Sexp.Atom (Duplication_mode.to_string mode)
          ]
        :: rest
      | None -> rest
    in
    let rest =
      match promotion.repository with
      | Some idx ->
        Sexp.List [ Sexp.Atom "repo"; Sexp.Atom (string_of_int idx) ] :: rest
      | None -> rest
    in
    cmd "promote"
      (Sexp.List [ Sexp.Atom "key"; Sexp.Atom key ]
       ::
       Sexp.List (Sexp.Atom "files" :: List.map ~f promotion.files)
       ::
       Sexp.List [ Sexp.Atom "metadata"; Sexp.List promotion.metadata ] :: rest)
  | SetBuildRoot root ->
    cmd "set-build-root" [ Sexp.Atom (Path.to_absolute_filename root) ]
  | SetCommonMetadata metadata -> cmd "set-common-metadata" metadata
  | SetRepos repositories ->
    let f { directory; remote; commit } =
      Sexp.List
        [ Sexp.List [ Sexp.Atom "dir"; Sexp.Atom directory ]
        ; Sexp.List [ Sexp.Atom "remote"; Sexp.Atom remote ]
        ; Sexp.List [ Sexp.Atom "commit_id"; Sexp.Atom commit ]
        ]
    in
    cmd "set-repos" (List.map ~f repositories)
  | Dedup file ->
    cmd "dedup"
      [ Sexp.List
          [ Sexp.Atom (Path.Local.to_string (Path.Build.local file.path))
          ; Sexp.Atom (Digest.to_string file.digest)
          ]
      ]

let int_of_string ?where s =
  match Int.of_string s with
  | Some s -> Ok s
  | None ->
    Result.Error
      (Printf.sprintf "invalid integer%s: %s"
         (match where with
         | Some l -> " in " ^ l
         | None -> "")
         s)

let lang_of_sexp = function
  | Sexp.Atom "dune-cache-protocol" :: versions ->
    let decode_version = function
      | Sexp.List [ Sexp.Atom major; Sexp.Atom minor ] ->
        let+ major = int_of_string ~where:"lang command version" major
        and+ minor = int_of_string ~where:"lang command version" minor in
        { major; minor }
      | v ->
        Result.Error
          (Printf.sprintf "invalid version in lang command: %s"
             (Sexp.to_string v))
    in
    Result.List.map ~f:decode_version versions
  | args -> invalid_args args

let initial_message_of_sexp = function
  | Sexp.List (Sexp.Atom "lang" :: args) ->
    let+ versions = lang_of_sexp args in
    Lang versions
  | exp ->
    Result.Error
      (Printf.sprintf "invalid initial message: %s" (Sexp.to_string exp))

let incoming_message_of_sexp version sexp =
  let open Result.O in
  let* path, digest =
    match sexp with
    | Sexp.List
        [ Sexp.Atom "dedup"; Sexp.List [ Sexp.Atom path; Sexp.Atom digest ] ]
      when version = { major = 1; minor = 2 } ->
      Ok (path, digest)
    | Sexp.List
        [ Sexp.Atom "dedup"
        ; Sexp.List
            (* Message protocol versions before v1.2 included an additional
               field [_path_in_cache] which is no longer used. *)
            [ Sexp.Atom path; Sexp.Atom _path_in_cache; Sexp.Atom digest ]
        ] ->
      Ok (path, digest)
    | exp ->
      Result.Error (Printf.sprintf "invalid command: %s" (Sexp.to_string exp))
  in
  match Digest.from_hex digest with
  | Some digest ->
    Result.Ok (Dedup { path = Path.Build.of_string path; digest })
  | None -> Result.Error (Printf.sprintf "invalid digest: %s" digest)

let outgoing_message_of_sexp version =
  let repos_of_sexp args =
    let convert = function
      | Sexp.List
          [ Sexp.List [ Sexp.Atom "dir"; Sexp.Atom directory ]
          ; Sexp.List [ Sexp.Atom "remote"; Sexp.Atom remote ]
          ; Sexp.List [ Sexp.Atom "commit_id"; Sexp.Atom commit ]
          ] ->
        Result.ok { directory; remote; commit }
      | invalid ->
        Result.Error
          (Printf.sprintf "invalid repo: %s" (Sexp.to_string invalid))
    in
    Result.List.map ~f:convert args
  and promote_of_sexp = function
    | Sexp.List [ Sexp.Atom "key"; Sexp.Atom key ]
      :: Sexp.List (Sexp.Atom "files" :: files)
         :: Sexp.List [ Sexp.Atom "metadata"; Sexp.List metadata ] :: rest as
      cmd ->
      let file = function
        | Sexp.List [ Sexp.Atom path; Sexp.Atom hash ] ->
          let+ d = Key.of_string hash in
          (Path.Build.of_local (Path.Local.of_string path), d)
        | sexp ->
          Result.Error
            (Printf.sprintf "invalid file in promotion message: %s"
               (Sexp.to_string sexp))
      in
      let* repository, rest =
        match rest with
        | Sexp.List [ Sexp.Atom "repo"; Sexp.Atom repo ] :: rest ->
          Result.map
            ~f:(fun repo -> (Some repo, rest))
            (int_of_string ~where:"repository index" repo)
        | _ -> Result.Ok (None, rest)
      in
      let+ duplication =
        match rest with
        | [ Sexp.List [ Sexp.Atom "duplication"; Sexp.Atom mode ] ] ->
          Result.map ~f:Option.some (Duplication_mode.of_string mode)
        | [] -> Result.Ok None
        | _ ->
          Result.Error
            (Printf.sprintf "invalid promotion message: %s"
               (Sexp.to_string (Sexp.List cmd)))
      and+ files = Result.List.map ~f:file files
      and+ key = Key.of_string key in
      { repository; files; key; metadata; duplication }
    | args -> invalid_args args
  and path_of_sexp = function
    | [ Sexp.Atom dir ] -> Result.ok (Path.of_string dir)
    | args -> invalid_args args
  and hint_of_sexp keys =
    let f = function
      | Sexp.Atom k -> (
        match Digest.from_hex k with
        | Some k -> Result.Ok k
        | None ->
          Result.Error (Format.asprintf "invalid key in hint message: %s" k))
      | k ->
        Result.Error
          (Format.asprintf "invalid expression in hint message: %a" Pp.to_fmt
             (Sexp.pp k))
    in
    Result.List.map ~f keys
  in
  function
  | Sexp.List (Sexp.Atom cmd :: args) ->
    Result.map_error
      ~f:(fun s -> cmd ^ ": " ^ s)
      (match cmd with
      | "hint" when hint_supported version ->
        let+ keys = hint_of_sexp args in
        Hint keys
      | "promote" ->
        let+ promotions = promote_of_sexp args in
        Promote promotions
      | "set-build-root" ->
        let+ path = path_of_sexp args in
        SetBuildRoot path
      | "set-common-metadata" -> Result.Ok (SetCommonMetadata args)
      | "set-repos" ->
        let+ repos = repos_of_sexp args in
        SetRepos repos
      | _ -> Result.Error (Printf.sprintf "unknown command: %s" cmd))
  | cmd ->
    Result.Error
      (Printf.sprintf "invalid command format: %s" (Sexp.to_string cmd))

let send_sexp output sexp =
  output_string output (Csexp.to_string sexp);
  flush output

let send version output message =
  send_sexp output (sexp_of_message version message)

let find_newest_common_version versions_a versions_b =
  let find a b =
    let f { major; minor } = (major, minor) in
    let a = Int.Map.of_list_map_exn ~f a
    and b = Int.Map.of_list_map_exn ~f b in
    let common =
      Int.Map.merge
        ~f:(fun _major minor_in_a minor_in_b ->
          match (minor_in_a, minor_in_b) with
          | Some a, Some b -> Some (min a b)
          | _ -> None)
        a b
    in
    Option.map
      ~f:(fun (major, minor) -> { major; minor })
      (Int.Map.max_binding common)
  in
  match find versions_a versions_b with
  | None -> Result.Error "no compatible versions"
  | Some version -> Result.ok version

let negotiate_version ~versions_supported_by_dune fd input output =
  send { major = 1; minor = 0 } output (Lang versions_supported_by_dune);
  let f msg =
    Unix.close fd;
    msg
  in
  Result.map_error ~f
    (let* sexp = Csexp.input input in
     let* (Lang versions) = initial_message_of_sexp sexp in
     find_newest_common_version versions_supported_by_dune versions)
