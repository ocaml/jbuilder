open Stdune
open Import

(** A string that is "%%VERSION%%" but not expanded by [dune subst] *)
let literal_version = "%%" ^ "VERSION%%"

let doc = "Substitute watermarks in source files."

let man =
  let var name desc =
    `Blocks [ `Noblank; `P ("- $(b,%%" ^ name ^ "%%), " ^ desc) ]
  in
  let opam field =
    var
      ("PKG_" ^ String.uppercase field)
      ("contents of the $(b," ^ field ^ ":) field from the opam file")
  in
  [ `S "DESCRIPTION"
  ; `P
      {|Substitute $(b,%%ID%%) strings in source files, in a similar fashion to
          what topkg does in the default configuration.|}
  ; `P
      ({|This command is only meant to be called when a user pins a package to
          its development version. Especially it replaces $(b,|}
     ^ literal_version
     ^ {|) strings by the version obtained from the vcs. Currently only git is
            supported and the version is obtained from the output of:|}
      )
  ; `Pre {|  \$ git describe --always --dirty|}
  ; `P
      {|$(b,dune subst) substitutes the variables that topkg substitutes with
          the default configuration:|}
  ; var "NAME" "the name of the project (from the dune-project file)"
  ; var "VERSION" "output of $(b,git describe --always --dirty)"
  ; var "VERSION_NUM"
      ("same as $(b," ^ literal_version
     ^ ") but with a potential leading 'v' or 'V' dropped")
  ; var "VCS_COMMIT_ID" "commit hash from the vcs"
  ; opam "maintainer"
  ; opam "authors"
  ; opam "homepage"
  ; opam "issues"
  ; opam "doc"
  ; opam "license"
  ; opam "repo"
  ; `P
      {|In order to call $(b,dune subst) when your package is pinned, add this line
          to the $(b,build:) field of your opam file:|}
  ; `Pre {|  [dune "subst"] {pinned}|}
  ; `P
      {|Note that this command is meant to be called only from opam files and
          behaves a bit differently from other dune commands. In particular it
          doesn't try to detect the root and must be called from the root of
          the project.|}
  ; `Blocks Common.help_secs
  ]

let info = Term.info "subst" ~doc ~man

let term =
  let+ () = Common.build_info
  and+ debug_backtraces = Common.debug_backtraces in
  let config : Config.t =
    { Config.default with display = Quiet; concurrency = Fixed 1 }
  in
  Dune_engine.Clflags.debug_backtraces debug_backtraces;
  Path.set_root (Path.External.cwd ());
  Path.Build.set_build_dir (Path.Build.Kind.of_string Common.default_build_dir);
  Config.init config;
  Log.init_disabled ();
  Dune_engine.Scheduler.go ~config Watermarks.subst

let command = (term, info)
