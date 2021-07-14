open Stdune
open Dune_rules
open Dune_engine
open Dune_tests_common

let () = init ()

let foo_meta = {|
requires = "bar"
requires(ppx_driver) = "baz"
|}

let db_path =
  Path.of_filename_relative_to_initial_cwd "../unit-tests/findlib-db"

let print_pkg ppf pkg =
  let info = Dune_package.Lib.info pkg in
  let name = Lib_info.name info in
  Format.fprintf ppf "<package:%s>" (Lib_name.to_string name)

let findlib =
  let lib_config : Lib_config.t =
    { has_native = true
    ; ext_lib = ".a"
    ; ext_obj = ".o"
    ; os_type = Ocaml_config.Os_type.Other ""
    ; architecture = ""
    ; system = ""
    ; model = ""
    ; natdynlink_supported = Dynlink_supported.By_the_os.of_bool true
    ; ext_dll = ".so"
    ; stdlib_dir = Path.root
    ; ccomp_type = Other "gcc"
    ; profile = Profile.Dev
    ; ocaml_version_string = "4.02.3"
    ; ocaml_version = Ocaml_version.make (4, 2, 3)
    ; instrument_with = []
    ; context_name = Context_name.of_string "default"
    }
  in
  Findlib.create ~paths:[ db_path ] ~lib_config

let%expect_test _ =
  let pkg =
    match
      Lib_name.of_string "foo" |> Findlib.find findlib |> Memo.Build.run
      |> Test_scheduler.(run (create ()))
    with
    | Ok (Library x) -> x
    | _ -> assert false
  in
  (* "foo" should depend on "baz" *)
  let info = Dune_package.Lib.info pkg in
  let requires = Lib_info.requires info in
  let dyn = Dyn.Encoder.list Lib_dep.to_dyn requires in
  let pp = Dyn.pp dyn in
  Format.printf "%a@." Pp.to_fmt pp;
  [%expect {|[ "baz" ]|}]

(* Meta parsing/simplification *)

let%expect_test _ =
  Meta.of_string foo_meta ~name:(Some (Package.Name.of_string "foo"))
  |> Meta.Simplified.to_dyn |> print_dyn;
  [%expect
    {|
    { name = Some "foo"
    ; vars =
        map
          { "requires" :
              { set_rules =
                  [ { var = "requires"
                    ; predicates = []
                    ; action = Set
                    ; value = "bar"
                    }
                  ; { var = "requires"
                    ; predicates = [ Pos "ppx_driver" ]
                    ; action = Set
                    ; value = "baz"
                    }
                  ]
              ; add_rules = []
              }
          }
    ; subs = []
    } |}]

let conf () =
  Findlib.Config.load
    (Path.relative db_path "../toolchain")
    ~toolchain:"tlc" ~context:"<context>"
  |> Memo.Build.run
  |> Test_scheduler.(run (create ()))

let%expect_test _ =
  let conf = conf () in
  print_dyn (Findlib.Config.to_dyn conf);
  [%expect
    {|
    { vars =
        map
          { "FOO_BAR" :
              { set_rules =
                  [ { preds_required = set { 6; 7 }
                    ; preds_forbidden = set {}
                    ; value = "my variable"
                    }
                  ]
              ; add_rules = []
              }
          }
    ; preds = set { 6 }
    } |}];
  print_dyn (Env.to_dyn (Findlib.Config.env conf));
  [%expect {| map { "FOO_BAR" : "my variable" } |}]
