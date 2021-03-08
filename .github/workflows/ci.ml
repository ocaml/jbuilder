open StdLabels

let skip_test =
  match Sys.getenv "SKIP_TEST" with
  | exception Not_found -> false
  | s -> bool_of_string s

let test_dependencies =
  match Sys.getenv "TEST_DEPS" with
  | exception Not_found -> false
  | s -> bool_of_string s

let run cmd args =
  (* broken when arguments contain spaces but it's good enough for now. *)
  let cmd = String.concat " " (cmd :: args) in
  match Sys.command cmd with
  | 0 -> ()
  | n ->
    Printf.eprintf "'%s' failed with code %d" cmd n;
    exit n

let opam args = run "opam" args

let packages () =
  let packages = Sys.readdir "." |> Array.to_list in
  let packages =
    List.fold_left packages ~init:[] ~f:(fun acc fname ->
      if Filename.check_suffix fname ".opam" then
        Filename.chop_suffix fname ".opam" :: acc
      else
        acc)
  in
  packages

let ignore_constraint () =
  "--ignore-constraints-on="^
  (String.concat ~sep:"," (packages ()))

let pin () =
  let packages = packages () in
  let packages =
    if skip_test then
      List.filter packages ~f:(fun pkg -> pkg = "dune")
    else
      packages
  in
  List.iter packages ~f:(fun package ->
    opam [ "pin"; "add"; package ^ ".next"; "."; "--no-action" ])

let opam_install_deps args =
  opam ([ "install"; "--deps-only"; ignore_constraint () ] @
        (if test_dependencies then [ "--with-test" ] else [])
        @ args
       )

let install_deps_of_dune () =
  opam_install_deps ["./dune.opam"]

let install_deps_of_test () =
  if Sys.win32 then (
    opam [ "install"; "./dune-configurator.opam"; "--deps-only"; ignore_constraint () ];
  ) else (
    opam_install_deps ["."];
    run "make" [ "dev-deps" ];
  )

let test () =
  if Sys.win32 then (
    run "make" [ "test-windows" ]
  ) else (
    run "make" [ "test" ]
  )

let () =
  match Sys.argv with
  | [| _; "pin" |] -> pin ()
  | [| _; "test" |] -> test ()
  | [| _; "install-deps-of-dune" |] -> install_deps_of_dune ()
  | [| _; "install-deps-of-test" |] -> install_deps_of_test ()
  | _ ->
    prerr_endline "Usage: ci.ml [pin | test | install-deps-of-dune | install-deps-of-test]";
    exit 1
