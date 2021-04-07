open! Stdune
open Import
module Dune_section = Section

(* The path after the man section mangling done by opam-installer. This roughly
   follows [add_man_section_dir] in [src/format/opamFile.ml] in opam. *)
module Dst : sig
  type t

  val to_string : t -> string

  val add_prefix : string -> t -> t

  val to_install_file :
    t -> src_basename:string -> section:Section.t -> string option

  val of_install_file :
    string option -> src_basename:string -> section:Section.t -> t

  val explicit : string -> t

  val compare : t -> t -> Ordering.t

  val infer : src_basename:string -> Section.t -> t

  include Dune_lang.Conv.S with type t := t

  val to_dyn : t -> Dyn.t
end = struct
  type t = string

  let to_string t = t

  let add_prefix p t = Filename.concat p t

  let explicit t = t

  let compare = Poly.compare

  let man_subdir s =
    let s =
      match String.drop_suffix ~suffix:".gz" s with
      | Some s -> s
      | None -> s
    in
    match String.rsplit2 ~on:'.' s with
    | None -> None
    | Some (_, "") -> None
    | Some (_, r) -> (
      match r.[0] with
      | '1' .. '8' as c -> Some (sprintf "man%c" c)
      | _ -> None)

  let infer ~src_basename:p section =
    match section with
    | Section.Man -> (
      match man_subdir p with
      | Some subdir -> Filename.concat subdir p
      | None -> p)
    | _ -> p

  let of_install_file t ~src_basename ~section =
    match t with
    | None -> infer ~src_basename section
    | Some s -> s

  let to_install_file t ~src_basename ~section =
    match t with
    | s ->
      let s' = infer ~src_basename section in
      if String.equal s s' then
        None
      else
        Some s

  let decode = Dune_lang.Decoder.string

  let encode = Dune_lang.Encoder.string

  let to_dyn = Dyn.Encoder.string
end

module Section_with_site = struct
  type t =
    | Section of Section.t
    | Site of
        { pkg : Package.Name.t
        ; site : Section.Site.t
        }

  (* let compare : t -> t -> Ordering.t = Poly.compare *)

  let to_dyn x =
    let open Dyn.Encoder in
    match x with
    | Section s -> constr "Section" [ Section.to_dyn s ]
    | Site { pkg; site } ->
      constr "Section" [ Package.Name.to_dyn pkg; Section.Site.to_dyn site ]

  let to_string = function
    | Section s -> Section.to_string s
    | Site { pkg; site } ->
      sprintf "(site %s %s)"
        (Package.Name.to_string pkg)
        (Section.Site.to_string site)

  let decode =
    let open Dune_lang.Decoder in
    sum
      ((Dune_section.enum_decoder
       |> List.map ~f:(fun (k, d) -> (k, return (Section d))))
      @ [ ( "site"
          , Dune_lang.Syntax.since Section.dune_site_syntax (0, 1)
            >>> pair Package.Name.decode Section.Site.decode
            >>| fun (pkg, site) -> Site { pkg; site } )
        ])

  let encode =
    let open Dune_lang.Encoder in
    function
    | Section s -> Section.encode s
    | Site { pkg; site } ->
      constr "site" (pair Package.Name.encode Section.Site.encode) (pkg, site)
end

module Section = struct
  include Section

  module Paths = struct
    type t =
      { lib : Path.t
      ; lib_root : Path.t
      ; libexec : Path.t
      ; libexec_root : Path.t
      ; bin : Path.t
      ; sbin : Path.t
      ; toplevel : Path.t
      ; share : Path.t
      ; share_root : Path.t
      ; etc : Path.t
      ; etc_root : Path.t
      ; doc : Path.t
      ; stublibs : Path.t
      ; man : Path.t
      }

    let make ~package ~destdir ?(libdir = Path.relative destdir "lib")
        ?(mandir = Path.relative destdir "man") () =
      let package = Package.Name.to_string package in
      let lib_root = libdir in
      let libexec_root = libdir in
      let share_root = Path.relative destdir "share" in
      let etc_root = Path.relative destdir "etc" in
      let doc_root = Path.relative destdir "doc" in
      { etc_root
      ; lib_root
      ; libexec_root
      ; share_root
      ; bin = Path.relative destdir "bin"
      ; sbin = Path.relative destdir "sbin"
      ; man = mandir
      ; toplevel = Path.relative libdir "toplevel"
      ; stublibs = Path.relative libdir "stublibs"
      ; lib = Path.relative lib_root package
      ; libexec = Path.relative libexec_root package
      ; share = Path.relative share_root package
      ; etc = Path.relative etc_root package
      ; doc = Path.relative doc_root package
      }

    let get t section =
      match section with
      | Lib -> t.lib
      | Lib_root -> t.lib_root
      | Libexec -> t.libexec
      | Libexec_root -> t.libexec_root
      | Bin -> t.bin
      | Sbin -> t.sbin
      | Toplevel -> t.toplevel
      | Share -> t.share
      | Share_root -> t.share_root
      | Etc -> t.etc
      | Etc_root -> t.etc_root
      | Doc -> t.doc
      | Stublibs -> t.stublibs
      | Man -> t.man
      | Misc -> Code_error.raise "Install.Paths.get" []

    let get_local_location context section package_name =
      (* check that we get the good path *)
      let install_dir = Local_install_path.dir ~context in
      let install_dir = Path.build install_dir in
      let paths = make ~package:package_name ~destdir:install_dir () in
      get paths section

    let install_path t section p =
      Path.relative (get t section) (Dst.to_string p)
  end
end

module Entry = struct
  type 'src t =
    { src : 'src
    ; dst : Dst.t
    ; section : Section.t
    }

  let compare x y =
    match Section.compare x.section y.section with
    | (Lt | Gt) as c -> c
    | Eq -> (
      match Dst.compare x.dst y.dst with
      | (Lt | Gt) as c -> c
      | Eq -> Path.Build.compare x.src y.src)

  let adjust_dst_gen =
    let error (source_pform : Dune_lang.Template.Pform.t) =
      User_error.raise ~loc:source_pform.loc
        [ Pp.textf
            "Because this file is installed in the 'bin' section, you cannot \
             use the %s %s in its basename."
            (Dune_lang.Template.Pform.describe_kind source_pform)
            (Dune_lang.Template.Pform.describe source_pform)
        ]
    in
    fun ~(src_suffix : String_with_vars.known_suffix) ~dst ~section ->
      match dst with
      | Some dst' when Filename.extension dst' = ".exe" -> Dst.explicit dst'
      | _ ->
        let dst =
          match dst with
          | None ->
            let src_basename =
              match src_suffix with
              | Full s -> Filename.basename s
              | Partial { source_pform; suffix } -> (
                match String.rsplit2 ~on:'/' suffix with
                | Some (_, basename) -> basename
                | None -> error source_pform)
            in
            Dst.infer ~src_basename section
          | Some dst -> Dst.explicit dst
        in
        let is_executable =
          let has_ext ext =
            match src_suffix with
            | Full s -> String.is_suffix s ~suffix:ext
            | Partial { source_pform; suffix } ->
              if String.is_suffix suffix ~suffix:ext then
                true
              else if String.is_suffix ext ~suffix then
                error source_pform
              else
                false
          in
          has_ext ".exe" || has_ext ".bc"
        in
        if
          Sys.win32 && is_executable
          && Filename.extension (Dst.to_string dst) <> ".exe"
        then
          Dst.explicit (Dst.to_string dst ^ ".exe")
        else
          dst

  let adjust_dst ~src ~dst ~section =
    adjust_dst_gen ~src_suffix:(String_with_vars.known_suffix src) ~dst ~section

  let adjust_dst' ~src ~dst ~section =
    adjust_dst_gen
      ~src_suffix:(Full (Path.to_string (Path.build src)))
      ~dst ~section

  let make section ?dst src =
    let dst = adjust_dst' ~src ~dst ~section in
    { src; dst; section }

  let make_with_site section ?dst get_section src =
    match section with
    | Section_with_site.Section section -> make section ?dst src
    | Site { pkg; site } ->
      let section = get_section ~pkg ~site in
      let dst = adjust_dst' ~src ~dst ~section in
      let dst = Dst.add_prefix (Section.Site.to_string site) dst in
      let dst_with_pkg_prefix =
        Dst.add_prefix (Package.Name.to_string pkg) dst
      in
      let (section : Section.t), dst =
        match section with
        | Lib -> (Lib_root, dst_with_pkg_prefix)
        | Libexec -> (Libexec_root, dst_with_pkg_prefix)
        | Share -> (Share_root, dst_with_pkg_prefix)
        | Etc
        | Etc_root
        | Doc ->
          User_error.raise
            [ Pp.textf "Can't have site in etc and doc for opam" ]
        | Lib_root
        | Libexec_root
        | Bin
        | Sbin
        | Toplevel
        | Share_root
        | Stublibs
        | Man
        | Misc ->
          (section, dst)
      in
      { src; dst; section }

  let set_src t src = { t with src }

  let relative_installed_path t ~paths =
    Section.Paths.install_path paths t.section t.dst

  let add_install_prefix t ~paths ~prefix =
    let opam_will_install_in_this_dir = Section.Paths.get paths t.section in
    let i_want_to_install_the_file_as =
      relative_installed_path t ~paths
      |> Path.as_in_source_tree_exn |> Path.append_source prefix
    in
    let dst =
      Path.reach i_want_to_install_the_file_as
        ~from:opam_will_install_in_this_dir
    in
    { t with dst = Dst.explicit dst }

  let of_install_file ~src ~dst ~section =
    { src
    ; section
    ; dst = Dst.of_install_file ~section ~src_basename:(Path.basename src) dst
    }
end

module Entry_with_site = struct
  type 'src t =
    { src : 'src
    ; dst : Dst.t
    ; section : Section_with_site.t
    }
end

let files entries =
  Path.Set.of_list_map entries ~f:(fun (entry : Path.Build.t Entry.t) ->
      Path.build entry.src)

let group entries =
  List.map entries ~f:(fun (entry : Path.Build.t Entry.t) ->
      (entry.section, entry))
  |> Section.Map.of_list_multi

let gen_install_file entries =
  let buf = Buffer.create 4096 in
  let pr fmt = Printf.bprintf buf (fmt ^^ "\n") in
  Section.Map.iteri (group entries) ~f:(fun section entries ->
      pr "%s: [" (Section.to_string section);
      List.sort ~compare:Entry.compare entries
      |> List.iter ~f:(fun (e : Path.Build.t Entry.t) ->
             let src = Path.to_string (Path.build e.src) in
             match
               Dst.to_install_file
                 ~src_basename:(Path.Build.basename e.src)
                 ~section:e.section e.dst
             with
             | None -> pr "  %S" src
             | Some dst -> pr "  %S {%S}" src dst);
      pr "]");
  Buffer.contents buf

let pos_of_opam_value : OpamParserTypes.value -> OpamParserTypes.pos = function
  | Bool (pos, _) -> pos
  | Int (pos, _) -> pos
  | String (pos, _) -> pos
  | Relop (pos, _, _, _) -> pos
  | Prefix_relop (pos, _, _) -> pos
  | Logop (pos, _, _, _) -> pos
  | Pfxop (pos, _, _) -> pos
  | Ident (pos, _) -> pos
  | List (pos, _) -> pos
  | Group (pos, _) -> pos
  | Option (pos, _, _) -> pos
  | Env_binding (pos, _, _, _) -> pos

let load_install_file path =
  let open OpamParserTypes in
  let file = Opam_file.load path in
  let fail (fname, line, col) msg =
    let pos : Lexing.position =
      { pos_fname = fname; pos_lnum = line; pos_bol = 0; pos_cnum = col }
    in
    User_error.raise ~loc:{ start = pos; stop = pos } [ Pp.text msg ]
  in
  List.concat_map file.file_contents ~f:(function
    | Variable (pos, section, files) -> (
      match Section.of_string section with
      | None -> fail pos "Unknown install section"
      | Some section -> (
        match files with
        | List (_, l) ->
          let install_file src dst =
            let src = Path.of_string src in
            Entry.of_install_file ~src ~dst ~section
          in
          List.map l ~f:(function
            | String (_, src) -> install_file src None
            | Option (_, String (_, src), [ String (_, dst) ]) ->
              install_file src (Some dst)
            | v -> fail (pos_of_opam_value v) "Invalid value in .install file")
        | v -> fail (pos_of_opam_value v) "Invalid value for install section"))
    | Section (pos, _) -> fail pos "Sections are not allowed in .install file")
