open Import

module Kind = struct
  type t =
    | Git
    | Hg

  let filenames = [ (".git", Git); (".hg", Hg) ]

  let of_filename = List.assoc filenames

  let of_dir_contents files =
    List.find_map filenames ~f:(fun (fname, kind) ->
        Option.some_if (String.Set.mem files fname) kind)

  let to_dyn t =
    Dyn.Variant
      ( (match t with
        | Git -> "Git"
        | Hg -> "Hg")
      , [] )

  let equal = ( = )
end

module T = struct
  type t =
    { root : Path.t
    ; kind : Kind.t
    }

  let to_dyn { root; kind } =
    Dyn.Encoder.record
      [ ("root", Path.to_dyn root); ("kind", Kind.to_dyn kind) ]

  let equal { root = ra; kind = ka } { root = rb; kind = kb } =
    Path.equal ra rb && Kind.equal ka kb

  (* No need to hash the kind as there is only only kind per directory *)
  let hash t = Path.hash t.root
end

include T

let git, hg =
  let get prog =
    lazy
      (match Bin.which ~path:(Env.path Env.initial) prog with
      | Some x -> x
      | None -> Utils.program_not_found prog ~loc:None)
  in
  (get "git", get "hg")

let select git hg t =
  Memo.Build.of_reproducible_fiber
    (match t.kind with
    | Git -> git t
    | Hg -> hg t)

let prog t =
  Lazy.force
    (match t.kind with
    | Git -> git
    | Hg -> hg)

let run t args =
  let open Fiber.O in
  let+ s =
    Process.run_capture Strict (prog t) args ~dir:t.root ~env:Env.initial
  in
  String.trim s

let git_accept () =
  Process.Accept (Predicate_lang.union [ Element 0; Element 128 ])

let run_git t args =
  let res =
    Process.run_capture (git_accept ()) (prog t) args ~dir:t.root
      ~env:Env.initial
      ~stderr_to:(Process.Io.file Config.dev_null Out)
  in
  let open Fiber.O in
  let+ res = res in
  match res with
  | Ok s -> Some (String.trim s)
  | Error 128 -> None
  | Error _ -> assert false

let hg_describe t =
  let open Fiber.O in
  let* s =
    run t [ "log"; "--rev"; "."; "-T"; "{latesttag} {latesttagdistance}" ]
  in
  let+ id = run t [ "id"; "-i" ] in
  let id, dirty_suffix =
    match String.drop_suffix id ~suffix:"+" with
    | Some id -> (id, "-dirty")
    | None -> (id, "")
  in
  let s =
    let s, dist = Option.value_exn (String.rsplit2 s ~on:' ') in
    match s with
    | "null" -> id
    | _ -> (
      match int_of_string dist with
      | 1 -> s
      | n -> sprintf "%s-%d-%s" s (n - 1) id
      | exception _ -> sprintf "%s-%s-%s" s dist id)
  in
  s ^ dirty_suffix

let make_fun name ~output_to_dyn ~git ~hg =
  let memo =
    Memo.create_no_cutoff name ~input:(module T) ~output_to_dyn (select git hg)
  in
  Staged.stage (Memo.exec memo)

let describe =
  Staged.unstage
  @@ make_fun "vcs-describe"
       ~output_to_dyn:(Dyn.Encoder.option String.to_dyn)
       ~git:(fun t -> run_git t [ "describe"; "--always"; "--dirty" ])
       ~hg:(fun x ->
         let open Fiber.O in
         let+ res = hg_describe x in
         Some res)

let commit_id =
  Staged.unstage
  @@ make_fun "vcs-commit-id"
       ~output_to_dyn:(Dyn.Encoder.option String.to_dyn)
       ~git:(fun t -> run_git t [ "rev-parse"; "HEAD" ])
       ~hg:(fun t ->
         let open Fiber.O in
         let+ res = run t [ "id"; "-i" ] in
         Some res)

let files =
  let run_zero_separated_hg t args =
    Process.run_capture_zero_separated Strict (prog t) args ~dir:t.root
      ~env:Env.initial
  in
  let run_zero_separated_git t args =
    let open Fiber.O in
    let+ res =
      Process.run_capture_zero_separated (git_accept ()) (prog t) args
        ~dir:t.root ~env:Env.initial
    in
    match res with
    | Ok s -> s
    | Error 128 -> []
    | Error _ -> assert false
  in
  let f run args t =
    let open Fiber.O in
    let+ l = run t args in
    List.map l ~f:Path.in_source
  in
  Staged.unstage
  @@ make_fun "vcs-files"
       ~output_to_dyn:(Dyn.Encoder.list Path.to_dyn)
       ~git:
         (f run_zero_separated_git
            [ "ls-tree"; "-z"; "-r"; "--name-only"; "HEAD" ])
       ~hg:(f run_zero_separated_hg [ "files"; "-0" ])
