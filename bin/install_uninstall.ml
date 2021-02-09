open Stdune
open Import

let interpret_destdir ~destdir path =
  match destdir with
  | None -> path
  | Some prefix ->
    Path.append_local (Path.of_string prefix) (Path.local_part path)

let get_dirs context ~prefix_from_command_line ~libdir_from_command_line =
  match prefix_from_command_line with
  | Some p ->
    let prefix = Path.of_string p in
    let dir = Option.value ~default:"lib" libdir_from_command_line in
    Fiber.return (prefix, Some (Path.relative prefix dir))
  | None ->
    let open Fiber.O in
    let* prefix = Memo.Build.run (Context.install_prefix context) in
    let libdir =
      match libdir_from_command_line with
      | None -> Memo.Build.run (Context.install_ocaml_libdir context)
      | Some l -> Fiber.return (Some (Path.relative prefix l))
    in
    let+ libdir = libdir in
    (prefix, libdir)

let resolve_package_install setup pkg =
  match Import.Main.package_install_file setup pkg with
  | Ok path -> path
  | Error () ->
    let pkg = Package.Name.to_string pkg in
    User_error.raise
      [ Pp.textf "Unknown package %s!" pkg ]
      ~hints:
        (User_message.did_you_mean pkg
           ~candidates:
             (Package.Name.Map.keys setup.conf.packages
             |> List.map ~f:Package.Name.to_string))

let print_unix_error f =
  try f () with
  | Unix.Unix_error (e, _, _) ->
    User_message.prerr (User_error.make [ Pp.text (Unix.error_message e) ])

module Special_file = struct
  type t =
    | META
    | Dune_package

  let of_entry (e : _ Install.Entry.t) =
    match e.section with
    | Lib ->
      let dst = Install.Dst.to_string e.dst in
      if dst = Findlib.meta_fn then
        Some META
      else if dst = Dune_package.fn then
        Some Dune_package
      else
        None
    | _ -> None
end

(** Operations that act on real files or just pretend to (for --dry-run) *)
module type File_operations = sig
  val copy_file :
       src:Path.t
    -> dst:Path.t
    -> executable:bool
    -> special_file:Special_file.t option
    -> package:Package.Name.t
    -> conf:Dune_rules.Artifact_substitution.conf
    -> unit Fiber.t

  val mkdir_p : Path.t -> unit

  val remove_if_exists : Path.t -> unit

  val remove_dir_if_empty : Path.t -> unit
end

module type Workspace = sig
  val workspace : Dune_rules.Main.workspace
end

module File_ops_dry_run : File_operations = struct
  let copy_file ~src ~dst ~executable ~special_file:_ ~package:_ ~conf:_ =
    Format.printf "Copying %s to %s (executable: %b)\n"
      (Path.to_string_maybe_quoted src)
      (Path.to_string_maybe_quoted dst)
      executable;
    Fiber.return ()

  let mkdir_p path =
    Format.printf "Creating directory %s\n" (Path.to_string_maybe_quoted path)

  let remove_if_exists path =
    Format.printf "Removing (if it exists) %s\n"
      (Path.to_string_maybe_quoted path)

  let remove_dir_if_empty path =
    Format.printf "Removing directory (if empty) %s\n"
      (Path.to_string_maybe_quoted path)
end

module File_ops_real (W : Workspace) : File_operations = struct
  open W

  let get_vcs p = Dune_engine.File_tree.nearest_vcs p

  type load_special_file_result =
    | No_version_needed
    | Need_version of (Format.formatter -> version:string -> unit)

  let copy_special_file ~src ~package ~ic ~oc ~f =
    let plain_copy () =
      seek_in ic 0;
      Io.copy_channels ic oc;
      Fiber.return ()
    in
    match f ic with
    | exception _ ->
      User_warning.emit ~loc:(Loc.in_file src)
        [ Pp.text
            "Failed to parse file, not adding version and locations \
             information."
        ];
      plain_copy ()
    | No_version_needed -> plain_copy ()
    | Need_version print -> (
      match
        let open Option.O in
        let* package = Package.Name.Map.find workspace.conf.packages package in
        Package.dir package |> get_vcs
      with
      | None -> plain_copy ()
      | Some vcs ->
        let open Fiber.O in
        let+ version = Memo.Build.run (Dune_engine.Vcs.describe vcs) in
        let ppf = Format.formatter_of_out_channel oc in
        print ppf ~version;
        Format.pp_print_flush ppf ())

  let process_meta ic =
    let lb = Lexing.from_channel ic in
    let meta : Dune_rules.Meta.t =
      { name = None; entries = Dune_rules.Meta.parse_entries lb }
    in
    let need_more_versions =
      try
        let (_ : Dune_rules.Meta.t) =
          Dune_rules.Meta.add_versions meta ~get_version:(fun _ ->
              raise_notrace Exit)
        in
        false
      with
      | Exit -> true
    in
    if not need_more_versions then
      No_version_needed
    else
      Need_version
        (fun ppf ~version ->
          let meta =
            Dune_rules.Meta.add_versions meta ~get_version:(fun _ ->
                Some version)
          in
          Pp.to_fmt ppf (Dune_rules.Meta.pp meta.entries))

  let replace_sites
      ~(get_location : Dune_engine.Section.t -> Package.Name.t -> Stdune.Path.t)
      dp =
    match
      List.find_map dp ~f:(function
        | Dune_lang.List [ Atom (A "name"); Atom (A name) ] -> Some name
        | _ -> None)
    with
    | None -> dp
    | Some name ->
      List.map dp ~f:(function
        | Dune_lang.List ((Atom (A "sites") as sexp_sites) :: sites) ->
          let sites =
            List.map sites ~f:(function
              | Dune_lang.List [ (Atom (A section) as section_sexp); _ ] ->
                let path =
                  get_location
                    (Option.value_exn (Section.of_string section))
                    (Package.Name.of_string name)
                in
                let open Dune_lang.Encoder in
                pair sexp string (section_sexp, Path.to_absolute_filename path)
              | _ -> assert false)
          in
          Dune_lang.List (sexp_sites :: sites)
        | x -> x)

  let process_dune_package ~get_location ic =
    let lb = Lexing.from_channel ic in
    let dp =
      Dune_lang.Parser.parse ~mode:Many lb
      |> List.map ~f:Dune_lang.Ast.remove_locs
    in
    (* replace sites with external path in the file *)
    let dp = replace_sites ~get_location dp in
    (* replace version if needed in the file *)
    if
      List.exists dp ~f:(function
        | Dune_lang.List (Atom (A "version") :: _)
        | Dune_lang.List [ Atom (A "use_meta"); Atom (A "true") ]
        | Dune_lang.List [ Atom (A "use_meta") ] ->
          true
        | _ -> false)
    then
      No_version_needed
    else
      Need_version
        (fun ppf ~version ->
          let version =
            Dune_lang.List
              [ Dune_lang.atom "version"
              ; Dune_lang.atom_or_quoted_string version
              ]
          in
          let dp =
            match dp with
            | lang :: name :: rest -> lang :: name :: version :: rest
            | [ lang ] -> [ lang; version ]
            | [] -> [ version ]
          in
          Format.pp_open_vbox ppf 0;
          List.iter dp ~f:(fun x ->
              Dune_lang.Deprecated.pp ppf x;
              Format.pp_print_cut ppf ());
          Format.pp_close_box ppf ())

  let copy_file ~src ~dst ~executable ~special_file ~package
      ~(conf : Dune_rules.Artifact_substitution.conf) =
    let chmod =
      if executable then
        fun _ ->
      0o755
      else
        fun _ ->
      0o644
    in
    let ic, oc = Io.setup_copy ~chmod ~src ~dst () in
    Fiber.finalize
      ~finally:(fun () ->
        Io.close_both (ic, oc);
        Fiber.return ())
      (fun () ->
        match (special_file : Special_file.t option) with
        | Some META -> copy_special_file ~src ~package ~ic ~oc ~f:process_meta
        | Some Dune_package ->
          copy_special_file ~src ~package ~ic ~oc
            ~f:(process_dune_package ~get_location:conf.get_location)
        | None ->
          Dune_rules.Artifact_substitution.copy ~conf ~input_file:src
            ~input:(input ic) ~output:(output oc))

  let remove_if_exists dst =
    if Path.exists dst then (
      Printf.eprintf "Deleting %s\n%!" (Path.to_string_maybe_quoted dst);
      print_unix_error (fun () -> Path.unlink dst)
    )

  let remove_dir_if_empty dir =
    if Path.exists dir then
      match Path.readdir_unsorted dir with
      | Ok [] ->
        Printf.eprintf "Deleting empty directory %s\n%!"
          (Path.to_string_maybe_quoted dir);
        print_unix_error (fun () -> Path.rmdir dir)
      | Error e ->
        User_message.prerr (User_error.make [ Pp.text (Unix.error_message e) ])
      | _ -> ()

  let mkdir_p p = Path.mkdir_p p
end

module Sections = struct
  type t =
    | All
    | Only of Section.Set.t

  let sections_conv : Section.t list Cmdliner.Arg.converter =
    let all =
      Section.all |> Section.Set.to_list
      |> List.map ~f:(fun section -> (Section.to_string section, section))
    in
    Arg.list ~sep:',' (Arg.enum all)

  let term =
    let doc = "sections that should be installed" in
    let open Cmdliner.Arg in
    let+ sections =
      value & opt (some sections_conv) None & info [ "sections" ] ~doc
    in
    match sections with
    | None -> All
    | Some sections -> Only (Section.Set.of_list sections)

  let should_install t section =
    match t with
    | All -> true
    | Only set -> Section.Set.mem set section
end

let file_operations ~dry_run ~workspace : (module File_operations) =
  if dry_run then
    (module File_ops_dry_run)
  else
    (module File_ops_real (struct
      let workspace = workspace
    end))

let package_is_vendored (pkg : Dune_engine.Package.t) =
  let dir = Package.dir pkg in
  Dune_engine.File_tree.is_vendored dir

let install_uninstall ~what =
  let doc = sprintf "%s packages." (String.capitalize what) in
  let name_ = Arg.info [] ~docv:"PACKAGE" in
  let term =
    let+ common = Common.term
    and+ prefix_from_command_line =
      Arg.(
        value
        & opt (some string) None
        & info [ "prefix" ] ~docv:"PREFIX"
            ~doc:
              "Directory where files are copied. For instance binaries are \
               copied into $(i,\\$prefix/bin), library files into \
               $(i,\\$prefix/lib), etc... It defaults to the current opam \
               prefix if opam is available and configured, otherwise it uses \
               the same prefix as the ocaml compiler.")
    and+ libdir_from_command_line =
      Arg.(
        value
        & opt (some string) None
        & info [ "libdir" ] ~docv:"PATH"
            ~doc:
              "Directory where library files are copied, relative to \
               $(b,prefix) or absolute. If $(b,--prefix) is specified the \
               default is $(i,\\$prefix/lib), otherwise it is the output of \
               $(b,ocamlfind printconf destdir)")
    and+ destdir =
      Arg.(
        value
        & opt (some string) None
        & info [ "destdir" ] ~env:(env_var "DESTDIR") ~docv:"PATH"
            ~doc:
              "When passed, this directory is prepended to all installed paths.")
    and+ mandir =
      let doc =
        "When passed, manually override the directory to install man pages"
      in
      Arg.(value & opt (some string) None & info [ "mandir" ] ~docv:"PATH" ~doc)
    and+ dry_run =
      Arg.(
        value & flag
        & info [ "dry-run" ]
            ~doc:"Only display the file operations that would be performed.")
    and+ relocatable =
      Arg.(
        value & flag
        & info [ "relocatable" ]
            ~doc:
              "Make the binaries relocatable (the installation directory can \
               be moved).")
    and+ pkgs = Arg.(value & pos_all package_name [] name_)
    and+ context =
      Arg.(
        value
        & opt (some Arg.context_name) None
        & info [ "context" ] ~docv:"CONTEXT"
            ~doc:
              "Select context to install from. By default, install files from \
               all defined contexts.")
    and+ sections = Sections.term in
    Common.set_common ~log_file:No_log_file common ~targets:[];
    Scheduler.go ~common (fun () ->
        let open Fiber.O in
        let* workspace = Memo.Build.run (Import.Main.scan_workspace common) in
        let contexts =
          match context with
          | None -> workspace.contexts
          | Some name -> [ Import.Main.find_context_exn workspace ~name ]
        in
        let pkgs =
          match pkgs with
          | [] ->
            Package.Name.Map.fold workspace.conf.packages ~init:[]
              ~f:(fun pkg acc ->
                if package_is_vendored pkg then
                  acc
                else
                  let name = Package.name pkg in
                  name :: acc)
            |> List.rev
          | l -> l
        in
        let install_files, missing_install_files =
          List.concat_map pkgs ~f:(fun pkg ->
              let fn = resolve_package_install workspace pkg in
              List.map contexts ~f:(fun ctx ->
                  let fn =
                    Path.append_source (Path.build ctx.Context.build_dir) fn
                  in
                  if Path.exists fn then
                    Left (ctx, (pkg, fn))
                  else
                    Right fn))
          |> List.partition_map ~f:Fun.id
        in
        if missing_install_files <> [] then
          User_error.raise
            [ Pp.textf "The following <package>.install are missing:"
            ; Pp.enumerate missing_install_files ~f:(fun p ->
                  Pp.text (Path.to_string p))
            ]
            ~hints:[ Pp.text "try running: dune build @install" ];
        (match
           (contexts, prefix_from_command_line, libdir_from_command_line)
         with
        | _ :: _ :: _, Some _, _
        | _ :: _ :: _, _, Some _ ->
          User_error.raise
            [ Pp.text
                "Cannot specify --prefix or --libdir when installing into \
                 multiple contexts!"
            ]
        | _ -> ());
        let module CMap = Map.Make (Context) in
        let install_files_by_context =
          CMap.of_list_multi install_files
          |> CMap.to_list
          |> List.map ~f:(fun (context, install_files) ->
                 let entries_per_package =
                   List.map install_files ~f:(fun (package, install_file) ->
                       let entries = Install.load_install_file install_file in
                       let entries =
                         List.filter entries
                           ~f:(fun (entry : Path.t Install.Entry.t) ->
                             Sections.should_install sections entry.section)
                       in
                       match
                         List.filter_map entries ~f:(fun entry ->
                             Option.some_if
                               (not (Path.exists entry.src))
                               entry.src)
                       with
                       | [] -> (package, entries)
                       | missing_files ->
                         User_error.raise
                           [ Pp.textf
                               "The following files which are listed in %s \
                                cannot be installed because they do not exist:"
                               (Path.to_string_maybe_quoted install_file)
                           ; Pp.enumerate missing_files ~f:(fun p ->
                                 Pp.verbatim (Path.to_string_maybe_quoted p))
                           ])
                 in
                 (context, entries_per_package))
        in
        let (module Ops) = file_operations ~dry_run ~workspace in
        let files_deleted_in = ref Path.Set.empty in
        let+ () =
          let mandir =
            Option.map ~f:Path.of_string
              (match mandir with
              | Some _ -> mandir
              | None -> Dune_rules.Setup.mandir)
          in
          Fiber.sequential_iter install_files_by_context
            ~f:(fun (context, entries_per_package) ->
              let* prefix, libdir =
                get_dirs context ~prefix_from_command_line
                  ~libdir_from_command_line
              in
              let conf =
                Dune_rules.Artifact_substitution.conf_for_install ~relocatable
                  ~default_ocamlpath:context.default_ocamlpath
                  ~stdlib_dir:context.stdlib_dir ~prefix ~libdir ~mandir
              in
              Fiber.sequential_iter entries_per_package
                ~f:(fun (package, entries) ->
                  let paths =
                    Install.Section.Paths.make ~package ~destdir:prefix ?libdir
                      ?mandir ()
                  in
                  Fiber.sequential_iter entries ~f:(fun entry ->
                      let special_file = Special_file.of_entry entry in
                      let dst =
                        Install.Entry.relative_installed_path entry ~paths
                        |> interpret_destdir ~destdir
                      in
                      let dir = Path.parent_exn dst in
                      if what = "install" then (
                        Ops.remove_if_exists dst;
                        Printf.eprintf "Installing %s\n%!"
                          (Path.to_string_maybe_quoted dst);
                        Ops.mkdir_p dir;
                        let executable =
                          Section.should_set_executable_bit entry.section
                        in
                        Ops.copy_file ~src:entry.src ~dst ~executable
                          ~special_file ~package ~conf
                      ) else (
                        Ops.remove_if_exists dst;
                        files_deleted_in := Path.Set.add !files_deleted_in dir;
                        Fiber.return ()
                      ))))
        in
        Path.Set.to_list !files_deleted_in
        (* This [List.rev] is to ensure we process children directories before
           their parents *)
        |> List.rev
        |> List.iter ~f:Ops.remove_dir_if_empty)
  in
  (term, Cmdliner.Term.info what ~doc ~man:Common.help_secs)

let install = install_uninstall ~what:"install"

let uninstall = install_uninstall ~what:"uninstall"
