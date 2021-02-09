open Dune_action_plugin.V1
module Glob = Dune_glob.V1
module Private = Dune_action_plugin.Private

let%expect_test _ =
  try ignore @@ Path.of_string "/some/absolute/path" with
  | Invalid_argument message ->
    print_endline message;
    [%expect
      {| Path "/some/absolute/path" is absolute. All paths used with dune-action-plugin must be relative. |}]

let%expect_test _ =
  let action =
    read_file ~path:(Path.of_string "some_dir/some_file")
    |> map ~f:print_endline
  in
  Private.do_run action;
  [%expect {|
    Hello from foo!
  |}]

let%expect_test _ =
  let action =
    read_directory_with_glob ~glob:Glob.universal
      ~path:(Path.of_string "some_dir")
    |> map ~f:(fun data -> String.concat "," data |> print_endline)
  in
  Private.do_run action;
  [%expect {|
    some_file
  |}]

let%expect_test _ =
  let action =
    write_file ~path:(Path.of_string "another_file") ~data:"Hello world!"
  in
  Private.do_run action;
  [%expect {| |}]

let run_action_expect_throws action =
  try
    Private.do_run action;
    print_endline "SHOULD BE UNREACHABLE"
  with
  | Private.Execution_error.E message -> print_endline message

let%expect_test _ =
  let action =
    read_file ~path:(Path.of_string "file_that_does_not_exist") |> map ~f:ignore
  in
  run_action_expect_throws action;
  [%expect
    {|
    read_file: file_that_does_not_exist: No such file or directory
  |}]

let%expect_test _ =
  let action =
    read_directory_with_glob ~glob:Glob.universal
      ~path:(Path.of_string "directory_that_does_not_exist")
    |> map ~f:ignore
  in
  run_action_expect_throws action;
  [%expect {|
    read_directory: No such file or directory
  |}]

let%expect_test _ =
  let action =
    write_file
      ~path:(Path.of_string "directory_that_does_not_exist/some_file")
      ~data:"foo"
  in
  run_action_expect_throws action;
  [%expect
    {|
    write_file: directory_that_does_not_exist/some_file: No such file or directory
  |}]
