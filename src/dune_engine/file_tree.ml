open! Stdune
open Import

module File = struct
  module T = struct
    type t =
      { ino : int
      ; dev : int
      }

    let to_dyn { ino; dev } =
      let open Dyn.Encoder in
      record [ ("ino", Int.to_dyn ino); ("dev", Int.to_dyn dev) ]

    let compare a b =
      match Int.compare a.ino b.ino with
      | Eq -> Int.compare a.dev b.dev
      | ne -> ne
  end

  include T

  let dummy = { ino = 0; dev = 0 }

  let of_stats (st : Unix.stats) = { ino = st.st_ino; dev = st.st_dev }

  module Map = Map.Make (T)

  let of_source_path p = of_stats (Path.stat (Path.source p))
end

module Dune_file = struct
  module Plain = struct
    type t =
      { mutable contents : Sub_dirs.Dir_map.per_dir
      ; for_subdirs : Sub_dirs.Dir_map.t
      }

    (** It's also possible to add GC for:

        - [contents.subdir_status]
        - [consumed nodes of for_subdirs]

        We don't do this for now because the benefits are likely small.*)

    let get_sexp_and_destroy t =
      let result = t.contents.sexps in
      t.contents <- { t.contents with sexps = [] };
      result
  end

  let fname = "dune"

  let jbuild_fname = "jbuild"

  type kind =
    | Plain
    | Ocaml_script

  type t =
    { path : Path.Source.t
    ; kind : kind
    ; (* for [kind = Ocaml_script], this is the part inserted with subdir *)
      plain : Plain.t
    }

  let get_static_sexp_and_possibly_destroy t =
    match t.kind with
    | Ocaml_script -> t.plain.contents.sexps
    | Plain -> Plain.get_sexp_and_destroy t.plain

  let kind t = t.kind

  let path t = t.path

  let sub_dirs (t : t option) =
    match t with
    | None -> Sub_dirs.default
    | Some t -> Sub_dirs.or_default t.plain.contents.subdir_status

  let load_plain sexps ~file ~from_parent ~project =
    let decoder =
      Dune_project.set_parsing_context project (Sub_dirs.decode ~file)
    in
    let active =
      let parsed =
        Dune_lang.Decoder.parse decoder Univ_map.empty
          (Dune_lang.Ast.List (Loc.none, sexps))
      in
      match from_parent with
      | None -> parsed
      | Some from_parent -> Sub_dirs.Dir_map.merge parsed from_parent
    in
    let contents = Sub_dirs.Dir_map.root active in
    { Plain.contents; for_subdirs = active }

  let load file ~file_exists ~from_parent ~project =
    let kind, plain =
      match file_exists with
      | false -> (Plain, load_plain [] ~file ~from_parent ~project)
      | true ->
        Io.with_lexbuf_from_file (Path.source file) ~f:(fun lb ->
            if Dune_lexer.is_script lb then
              let from_parent = load_plain [] ~file ~from_parent ~project in
              (Ocaml_script, from_parent)
            else
              let sexps = Dune_lang.Parser.parse lb ~mode:Many in
              (Plain, load_plain sexps ~file ~from_parent ~project))
    in
    { path = file; kind; plain }
end

module Readdir : sig
  type t = private
    { files : String.Set.t
    ; dirs : (string * Path.Source.t * File.t) list
    }

  val empty : t

  val of_source_path : Path.Source.t -> (t, Unix.error) Result.t
end = struct
  type t =
    { files : String.Set.t
    ; dirs : (string * Path.Source.t * File.t) list
    }

  let empty = { files = String.Set.empty; dirs = [] }

  let _to_dyn { files; dirs } =
    let open Dyn.Encoder in
    record
      [ ("files", String.Set.to_dyn files)
      ; ("dirs", list (triple string Path.Source.to_dyn File.to_dyn) dirs)
      ]

  let is_temp_file fn =
    String.is_prefix fn ~prefix:".#"
    || String.is_suffix fn ~suffix:".swp"
    || String.is_suffix fn ~suffix:"~"

  (* Returns [true] for special files such as character devices of sockets; see
     #3124 for more on issues caused by special devices *)
  let is_special (st_kind : Unix.file_kind) =
    match st_kind with
    | S_CHR
    | S_BLK
    | S_FIFO
    | S_SOCK ->
      true
    | _ -> false

  let of_source_path path =
    match Path.readdir_unsorted_with_kinds (Path.source path) with
    | Error unix_error ->
      User_warning.emit
        [ Pp.textf "Unable to read directory %s. Ignoring."
            (Path.Source.to_string_maybe_quoted path)
        ; Pp.text "Remove this message by ignoring by adding:"
        ; Pp.textf "(dirs \\ %s)" (Path.Source.basename path)
        ; Pp.textf "to the dune file: %s"
            (Path.Source.to_string_maybe_quoted
               (Path.Source.relative
                  (Path.Source.parent_exn path)
                  Dune_file.fname))
        ; Pp.textf "Reason: %s" (Unix.error_message unix_error)
        ];
      Error unix_error
    | Ok unsorted_contents ->
      let files, dirs =
        List.filter_partition_map unsorted_contents ~f:(fun (fn, kind) ->
            let path = Path.Source.relative path fn in
            if Path.Source.is_in_build_dir path then
              Skip
            else
              let is_directory, file =
                match kind with
                | S_DIR -> (true, File.of_source_path path)
                | S_LNK -> (
                  match Path.stat (Path.source path) with
                  | exception _ -> (false, File.dummy)
                  | { st_kind = S_DIR; _ } as st -> (true, File.of_stats st)
                  | _ -> (false, File.dummy))
                | _ -> (false, File.dummy)
              in
              if is_directory then
                Right (fn, path, file)
              else if is_temp_file fn || is_special kind then
                Skip
              else
                Left fn)
      in
      { files = String.Set.of_list files
      ; dirs =
          List.sort dirs ~compare:(fun (a, _, _) (b, _, _) ->
              String.compare a b)
      }
      |> Result.ok
end

module Dirs_visited : sig
  (** Unique set of all directories visited *)
  type t

  val singleton : Path.Source.t -> t

  module Per_fn : sig
    (** Stores the directories visited per node (basename) *)
    type t

    type dirs_visited

    val to_dyn : t -> Dyn.t

    val init : t

    val find : t -> Path.Source.t -> dirs_visited

    val add : t -> dirs_visited -> string * Path.Source.t * File.t -> t
  end
  with type dirs_visited := t
end = struct
  type t = Path.Source.t File.Map.t

  let singleton path = File.Map.singleton (File.of_source_path path) path

  module Per_fn = struct
    type nonrec t = t String.Map.t

    let init = String.Map.empty

    let find t path =
      String.Map.find t (Path.Source.basename path)
      |> Option.value ~default:File.Map.empty

    let add (acc : t) dirs_visited (fn, path, file) =
      if Sys.win32 then
        acc
      else
        let new_dirs_visited =
          File.Map.update dirs_visited file ~f:(function
            | None -> Some path
            | Some first_path ->
              User_error.raise
                [ Pp.textf
                    "Path %s has already been scanned. Cannot scan it again \
                     through symlink %s"
                    (Path.Source.to_string_maybe_quoted first_path)
                    (Path.Source.to_string_maybe_quoted path)
                ])
        in
        String.Map.add_exn acc fn new_dirs_visited

    let to_dyn t = String.Map.to_dyn (File.Map.to_dyn Path.Source.to_dyn) t
  end
end

module Output = struct
  type 'a t =
    { dir : 'a
    ; visited : Dirs_visited.Per_fn.t
    }

  let to_dyn f { dir; visited } =
    let open Dyn.Encoder in
    record [ ("dir", f dir); ("visited", Dirs_visited.Per_fn.to_dyn visited) ]
end

module Dir0 = struct
  type t =
    { path : Path.Source.t
    ; status : Sub_dirs.Status.t
    ; contents : contents
    ; project : Dune_project.t
    ; vcs : Vcs.t option
    }

  and contents =
    { files : String.Set.t
    ; sub_dirs : sub_dir String.Map.t
    ; dune_file : Dune_file.t option
    }

  and sub_dir =
    { sub_dir_status : Sub_dirs.Status.t
    ; virtual_ : bool
    ; sub_dir_as_t :
        ( Path.Source.t
        , t Output.t option
        , Path.Source.t -> t Output.t option )
        Memo.Cell.t
    }

  type error = Missing_run_t of Cram_test.t

  let rec to_dyn { path; status; contents; project = _; vcs } =
    let open Dyn in
    Record
      [ ("path", Path.Source.to_dyn path)
      ; ("status", Sub_dirs.Status.to_dyn status)
      ; ("contents", dyn_of_contents contents)
      ; ("vcs", Dyn.Encoder.option Vcs.to_dyn vcs)
      ]

  and dyn_of_sub_dir { sub_dir_status; sub_dir_as_t; virtual_ } =
    let open Dyn.Encoder in
    let path = Memo.Cell.input sub_dir_as_t in
    record
      [ ("status", Sub_dirs.Status.to_dyn sub_dir_status)
      ; ("sub_dir_as_t", Path.Source.to_dyn path)
      ; ("virtual_", bool virtual_)
      ]

  and dyn_of_contents { files; sub_dirs; dune_file } =
    let open Dyn.Encoder in
    record
      [ ("files", String.Set.to_dyn files)
      ; ("sub_dirs", String.Map.to_dyn dyn_of_sub_dir sub_dirs)
      ; ("dune_file", Dyn.Encoder.(option opaque dune_file))
      ; ("project", Dyn.opaque)
      ]

  module Contents = struct
    let create ~files ~sub_dirs ~dune_file = { files; sub_dirs; dune_file }
  end

  let create ~project ~path ~status ~contents ~vcs =
    { path; status; contents; project; vcs }

  let contents t = t.contents

  let path t = t.path

  let status t = t.status

  let files t = (contents t).files

  let sub_dirs t = (contents t).sub_dirs

  let dune_file t = (contents t).dune_file

  let project t = t.project

  let vcs t = t.vcs

  let file_paths t =
    Path.Source.Set.of_listing ~dir:t.path
      ~filenames:(String.Set.to_list (files t))

  let sub_dir_names t =
    String.Map.foldi (sub_dirs t) ~init:String.Set.empty ~f:(fun s _ acc ->
        String.Set.add acc s)

  let sub_dir_paths t =
    String.Map.foldi (sub_dirs t) ~init:Path.Source.Set.empty ~f:(fun s _ acc ->
        Path.Source.Set.add acc (Path.Source.relative t.path s))
end

module Settings : sig
  type t =
    { ancestor_vcs : Vcs.t option
    ; recognize_jbuilder_projects : bool
    }

  val set : t -> unit

  val get : unit -> t
end = struct
  type t =
    { ancestor_vcs : Vcs.t option
    ; recognize_jbuilder_projects : bool
    }

  let equal { ancestor_vcs; recognize_jbuilder_projects } y =
    Option.equal Vcs.equal ancestor_vcs y.ancestor_vcs
    && Bool.equal recognize_jbuilder_projects y.recognize_jbuilder_projects

  let to_dyn { ancestor_vcs; recognize_jbuilder_projects } =
    let open Dyn.Encoder in
    record
      [ ("ancestor_vcs", option Vcs.to_dyn ancestor_vcs)
      ; ("recognize_jbuilder_projects", bool recognize_jbuilder_projects)
      ]

  let t = Fdecl.create to_dyn

  let set x = Fdecl.set_idempotent ~equal t x

  let get () =
    let (_ : Memo.Run.t) = Memo.current_run () in
    Fdecl.get t
end

let init ~ancestor_vcs ~recognize_jbuilder_projects =
  Settings.set { ancestor_vcs; recognize_jbuilder_projects }

module rec Memoized : sig
  val root : unit -> Dir0.t

  (* Not part of the interface. Only necessary to call recursively *)
  val find_dir_raw :
       Path.Source.t
    -> ( Path.Source.t
       , Dir0.t Output.t option
       , Path.Source.t -> Dir0.t Output.t option )
       Memo.Cell.t

  val find_dir : Path.Source.t -> Dir0.t option
end = struct
  open Memoized

  module Get_subdir : sig
    (** Get all the sub directories of [path].*)
    val all :
         dirs_visited:Dirs_visited.t
      -> dirs:(string * Path.Source.t * File.t) list
      -> sub_dirs:Predicate_lang.Glob.t Sub_dirs.Status.Map.t
      -> parent_status:Sub_dirs.Status.t
      -> dune_file:Dune_file.t option (** to interpret [(subdir ..)] stanzas *)
      -> path:Path.Source.t
      -> Dirs_visited.Per_fn.t * Dir0.sub_dir String.Map.t
  end = struct
    let status ~status_map ~(parent_status : Sub_dirs.Status.t) dir :
        Sub_dirs.Status.t option =
      let status = Sub_dirs.status status_map ~dir in
      match status with
      | Ignored -> None
      | Status status ->
        Some
          (match (parent_status, status) with
          | Data_only, _ -> Data_only
          | Vendored, Normal -> Vendored
          | _, _ -> status)

    let make_subdir ~dir_status ~virtual_ path =
      let sub_dir_as_t = find_dir_raw path in
      { Dir0.sub_dir_status = dir_status; sub_dir_as_t; virtual_ }

    let physical ~dirs_visited ~dirs ~sub_dirs ~parent_status =
      let status_map =
        Sub_dirs.eval sub_dirs ~dirs:(List.map ~f:(fun (a, _, _) -> a) dirs)
      in
      List.fold_left dirs ~init:(Dirs_visited.Per_fn.init, String.Map.empty)
        ~f:(fun (dirs_visited_acc, subdirs) ((fn, path, _) as dir) ->
          match status ~status_map ~parent_status fn with
          | None -> (dirs_visited_acc, subdirs)
          | Some dir_status ->
            let dirs_visited_acc =
              Dirs_visited.Per_fn.add dirs_visited_acc dirs_visited dir
            in
            let sub_dir = make_subdir ~dir_status ~virtual_:false path in
            let subdirs = String.Map.add_exn subdirs fn sub_dir in
            (dirs_visited_acc, subdirs))

    let virtual_ ~sub_dirs ~parent_status ~dune_file ~init ~path =
      match dune_file with
      | None -> init
      | Some (df : Dune_file.t) ->
        (* Virtual directories are not in [Readdir.t]. Their presence is only *)
        let dirs = Sub_dirs.Dir_map.sub_dirs df.plain.for_subdirs in
        let status_map = Sub_dirs.eval sub_dirs ~dirs in
        List.fold_left dirs ~init ~f:(fun acc fn ->
            let path = Path.Source.relative path fn in
            match status ~status_map ~parent_status fn with
            | None -> acc
            | Some dir_status ->
              String.Map.update acc fn ~f:(function
                (* Physical directories have already been added so they are
                   skipped here.*)
                | Some _ as r -> r
                | None -> Some (make_subdir ~dir_status ~virtual_:true path)))

    let all ~dirs_visited ~dirs ~sub_dirs ~parent_status ~dune_file ~path =
      let visited, init =
        physical ~dirs_visited ~dirs ~sub_dirs ~parent_status
      in
      let init = virtual_ ~sub_dirs ~parent_status ~dune_file ~init ~path in
      (visited, init)
  end

  let dune_file ~(dir_status : Sub_dirs.Status.t) ~recognize_jbuilder_projects
      ~path ~files ~project =
    let file_exists =
      if dir_status = Data_only then
        false
      else if
        (not recognize_jbuilder_projects)
        && String.Set.mem files Dune_file.jbuild_fname
      then
        User_error.raise
          ~loc:
            (Loc.in_file
               (Path.source (Path.Source.relative path Dune_file.jbuild_fname)))
          [ Pp.text
              "jbuild files are no longer supported, please convert this file \
               to a dune file instead."
          ; Pp.text
              "Note: You can use \"dune upgrade\" to convert your project to \
               dune."
          ]
      else
        String.Set.mem files Dune_file.fname
    in
    let from_parent =
      let open Option.O in
      let* parent = Path.Source.parent path in
      let* parent = find_dir parent in
      let* dune_file = parent.contents.dune_file in
      let dir_basename = Path.Source.basename path in
      Sub_dirs.Dir_map.descend dune_file.plain.for_subdirs dir_basename
    in
    let dune_file_absent = (not file_exists) && Option.is_none from_parent in
    if dune_file_absent then
      None
    else (
      ignore
        (Dune_project.ensure_project_file_exists project
          : Dune_project.created_or_already_exist);
      let file = Path.Source.relative path Dune_file.fname in
      Some (Dune_file.load file ~file_exists ~project ~from_parent)
    )

  let contents { Readdir.dirs; files } ~dirs_visited ~project ~path
      ~(dir_status : Sub_dirs.Status.t) =
    let recognize_jbuilder_projects =
      let settings = Settings.get () in
      settings.recognize_jbuilder_projects
    in
    let dune_file =
      dune_file ~dir_status ~recognize_jbuilder_projects ~files ~project ~path
    in
    let sub_dirs = Dune_file.sub_dirs dune_file in
    let dirs_visited, sub_dirs =
      Get_subdir.all ~dirs_visited ~dirs ~sub_dirs ~parent_status:dir_status
        ~dune_file ~path
    in
    (Dir0.Contents.create ~files ~sub_dirs ~dune_file, dirs_visited)

  let get_vcs ~default:vcs ~path ~readdir:{ Readdir.files; dirs } =
    match
      match
        List.find_map dirs ~f:(fun (name, _, _) -> Vcs.Kind.of_filename name)
      with
      | Some kind -> Some kind
      | None -> Vcs.Kind.of_dir_contents files
    with
    | None -> vcs
    | Some kind -> Some { Vcs.kind; root = Path.(append_source root) path }

  let root () =
    let settings = Settings.get () in
    let path = Path.Source.root in
    let dir_status : Sub_dirs.Status.t = Normal in
    let readdir =
      match Readdir.of_source_path path with
      | Ok dir -> dir
      | Error m ->
        User_error.raise
          [ Pp.textf "Unable to load source %s.@.Reason:%s@."
              (Path.Source.to_string_maybe_quoted path)
              (Unix.error_message m)
          ]
    in
    let project =
      match
        Dune_project.load ~dir:path ~files:readdir.files
          ~infer_from_opam_files:true ~dir_status
      with
      | None -> Dune_project.anonymous ~dir:path
      | Some p -> p
    in
    let vcs =
      get_vcs ~default:settings.ancestor_vcs ~path:Path.Source.root ~readdir
    in
    let dirs_visited = Dirs_visited.singleton path in
    let contents, visited =
      contents readdir ~dirs_visited ~project ~path ~dir_status
    in
    let dir = Dir0.create ~project ~path ~status:dir_status ~contents ~vcs in
    { Output.dir; visited }

  let find_dir_raw_impl path : Dir0.t Output.t option =
    match Path.Source.parent path with
    | None -> Some (root ())
    | Some parent_dir ->
      let open Option.O in
      let* { Output.dir = parent_dir; visited = dirs_visited } =
        Memo.Cell.get_sync (find_dir_raw parent_dir)
      in
      let* dir_status, virtual_ =
        let basename = Path.Source.basename path in
        let+ sub_dir = String.Map.find parent_dir.contents.sub_dirs basename in
        let status =
          let status = sub_dir.sub_dir_status in
          if
            Dune_project.cram parent_dir.project
            && Cram_test.is_cram_suffix basename
          then
            Sub_dirs.Status.Data_only
          else
            status
        in
        (status, sub_dir.virtual_)
      in
      let dirs_visited = Dirs_visited.Per_fn.find dirs_visited path in
      let settings = Settings.get () in
      let readdir =
        if virtual_ then
          Readdir.empty
        else
          match Readdir.of_source_path path with
          | Ok dir -> dir
          | Error _ -> Readdir.empty
      in
      let project =
        if dir_status = Data_only then
          parent_dir.project
        else
          Option.value
            (Dune_project.load ~dir:path ~files:readdir.files
               ~infer_from_opam_files:settings.recognize_jbuilder_projects
               ~dir_status)
            ~default:parent_dir.project
      in
      let vcs = get_vcs ~default:parent_dir.vcs ~readdir ~path in
      let contents, visited =
        contents readdir ~dirs_visited ~project ~path ~dir_status
      in
      let dir = Dir0.create ~project ~path ~status:dir_status ~contents ~vcs in
      Some { Output.dir; visited }

  let find_dir_raw =
    let module Output = struct
      type t = Dir0.t Output.t option

      let to_dyn =
        let open Dyn.Encoder in
        option (Output.to_dyn Dir0.to_dyn)
    end in
    let memo =
      Memo.create "find-dir-raw" ~doc:"get file tree"
        ~input:(module Path.Source)
        ~output:(Simple (module Output))
        ~visibility:Memo.Visibility.Hidden Sync find_dir_raw_impl
    in
    Memo.cell memo

  let find_dir p =
    let open Option.O in
    let+ { Output.dir; visited = _ } = Memo.Cell.get_sync (find_dir_raw p) in
    dir

  let root () = Option.value_exn (find_dir Path.Source.root)
end

let root () = Memoized.root ()

let find_dir path = Memoized.find_dir path

let rec nearest_dir t = function
  | [] -> t
  | comp :: components -> (
    match String.Map.find (Dir0.sub_dirs t) comp with
    | None -> t
    | Some _ ->
      let path = Path.Source.relative (Dir0.path t) comp in
      let dir = Option.value_exn (find_dir path) in
      nearest_dir dir components)

let nearest_dir path =
  let components = Path.Source.explode path in
  nearest_dir (root ()) components

let nearest_vcs path = Dir0.vcs (nearest_dir path)

let files_of path =
  match find_dir path with
  | None -> Path.Source.Set.empty
  | Some dir ->
    Dir0.files dir |> String.Set.to_list
    |> Path.Source.Set.of_list_map ~f:(Path.Source.relative path)

let file_exists path =
  match find_dir (Path.Source.parent_exn path) with
  | None -> false
  | Some dir -> String.Set.mem (Dir0.files dir) (Path.Source.basename path)

let dir_exists path = Option.is_some (find_dir path)

module Dir = struct
  include Dir0

  let sub_dir_as_t (s : sub_dir) =
    (Memo.Cell.get_sync s.sub_dir_as_t |> Option.value_exn).dir

  let fold_sub_dirs (t : t) ~init ~f =
    String.Map.foldi t.contents.sub_dirs ~init ~f:(fun basename s acc ->
        f ~basename (sub_dir_as_t s) acc)

  let fold_dune_files (type acc) t ~(init : acc) ~f =
    let rec loop ~basename dir (acc : acc) : acc =
      let init =
        match dune_file dir with
        | None -> acc
        | Some dune_file -> f ~basename dir dune_file acc
      in
      fold_sub_dirs dir ~init ~f:(fun ~basename ->
          loop ~basename:(Some basename))
    in
    let basename = Path.Source.basename_opt t.path in
    loop ~basename t init

  let rec fold t ~traverse ~init:acc ~f =
    let must_traverse = Sub_dirs.Status.Map.find traverse t.status in
    match must_traverse with
    | false -> acc
    | true ->
      let acc = f t acc in
      fold_sub_dirs t ~init:acc ~f:(fun ~basename:_ t acc ->
          fold t ~traverse ~init:acc ~f)

  let cram_tests (t : t) =
    match Dune_project.cram t.project with
    | false -> []
    | true ->
      let file_tests =
        String.Set.to_list t.contents.files
        |> List.filter_map ~f:(fun s ->
               if Cram_test.is_cram_suffix s then
                 Some (Ok (Cram_test.File (Path.Source.relative t.path s)))
               else
                 None)
      in
      let dir_tests =
        String.Map.to_list t.contents.sub_dirs
        |> List.filter_map ~f:(fun (name, sub_dir) ->
               match Cram_test.is_cram_suffix name with
               | false -> None
               | true ->
                 let contents =
                   (Memo.Cell.get_sync sub_dir.sub_dir_as_t |> Option.value_exn)
                     .dir
                 in
                 let dir = contents.path in
                 let fname = "run.t" in
                 let test =
                   let file = Path.Source.relative dir fname in
                   Cram_test.Dir { file; dir }
                 in
                 let files = contents.contents.files in
                 if String.Set.is_empty files then
                   None
                 else
                   Some
                     (if String.Set.mem files fname then
                       Ok test
                     else
                       Error (Missing_run_t test)))
      in
      file_tests @ dir_tests
end

let fold_with_progress ~traverse ~init ~f =
  let root = root () in
  let nb_path_visited = ref 0 in
  Console.Status_line.set (fun () ->
      Some (Pp.textf "Scanned %i directories" !nb_path_visited));
  let res =
    Dir.fold root ~traverse ~init ~f:(fun dir acc ->
        incr nb_path_visited;
        if !nb_path_visited mod 100 = 0 then Console.Status_line.refresh ();
        f dir acc)
  in
  Console.Status_line.set (Fun.const None);
  res

let find_dir_specified_on_command_line ~dir =
  match find_dir dir with
  | Some dir -> dir
  | None ->
    User_error.raise
      [ Pp.textf "Don't know about directory %s specified on the command line!"
          (Path.Source.to_string_maybe_quoted dir)
      ]

let is_vendored dir =
  match find_dir dir with
  | None -> false
  | Some d -> Dir.status d = Vendored
