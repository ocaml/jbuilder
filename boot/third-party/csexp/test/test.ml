module Sexp = struct
  type t =
    | Atom of string
    | List of t list
end

module Csexp = Csexp.Make (Sexp)
open Csexp

let roundtrip x =
  let str = to_string x in
  match parse_string str with
  | Result.Error (_, msg) -> failwith msg
  | Result.Ok exp ->
    assert (exp = x);
    print_string str

let%expect_test _ =
  roundtrip (Sexp.Atom "foo");
  [%expect {|3:foo|}]

let%expect_test _ =
  roundtrip (Sexp.List []);
  [%expect {|()|}]

let%expect_test _ =
  roundtrip (Sexp.List [ Sexp.Atom "Hello"; Sexp.Atom "World!" ]);
  [%expect {|(5:Hello6:World!)|}]

let%expect_test _ =
  roundtrip
    (Sexp.List
       [ Sexp.List
           [ Sexp.Atom "metadata"
           ; Sexp.List [ Sexp.Atom "foo"; Sexp.Atom "bar" ]
           ]
       ; Sexp.List
           [ Sexp.Atom "produced-files"
           ; Sexp.List
               [ Sexp.List
                   [ Sexp.Atom "/tmp/coin"
                   ; Sexp.Atom
                       "/tmp/dune-memory/v2/files/b2/b295e63b0b8e8fae971d9c493be0d261.1"
                   ]
               ]
           ]
       ]);
  [%expect
    {|((8:metadata(3:foo3:bar))(14:produced-files((9:/tmp/coin63:/tmp/dune-memory/v2/files/b2/b295e63b0b8e8fae971d9c493be0d261.1))))|}]

let print_parsed r =
  match r with
  | Error msg -> Printf.printf "Error %S" msg
  | Ok sexp -> Printf.printf "Ok %S" (Csexp.to_string sexp)

let parse s =
  match parse_string s with
  | Ok x -> print_parsed (Ok x)
  | Error (_, msg) -> print_parsed (Error msg)

let%expect_test _ =
  parse "(3:foo)";
  [%expect {|
    Ok "(3:foo)" |}]

let%expect_test _ =
  parse "";
  [%expect {| Error "premature end of input" |}]

let%expect_test _ =
  parse "(";
  [%expect {| Error "premature end of input" |}]

let%expect_test _ =
  parse "(a)";
  [%expect {| Error "invalid character 'a', expected '(', ')' or '0'..'9'" |}]

let%expect_test _ =
  parse "(:)";
  [%expect {| Error "invalid character ':', expected '(', ')' or '0'..'9'" |}]

let%expect_test _ =
  parse "(4:foo)";
  [%expect {| Error "premature end of input" |}]

let%expect_test _ =
  parse "(5:foo)";
  [%expect {| Error "premature end of input" |}]

let%expect_test _ =
  parse "(3:foo)";
  [%expect {| Ok "(3:foo)" |}]

let sexp_then_stuff s =
  let fn, oc = Filename.open_temp_file "csexp-test" "" ~mode:[ Open_binary ] in
  let delete = lazy (Sys.remove fn) in
  at_exit (fun () -> Lazy.force delete);
  output_string oc s;
  close_out oc;
  let ic = open_in_bin fn in
  Csexp.input ic |> print_parsed;
  print_newline ();
  print_char (input_char ic);
  close_in ic;
  Lazy.force delete

let%expect_test _ =
  sexp_then_stuff "(3:foo)(3:foo)";
  [%expect {|
    Ok "(3:foo)"
    ( |}]

let%expect_test _ =
  sexp_then_stuff "(3:foo)Additional_stuff";
  [%expect {|
    Ok "(3:foo)"
    A |}]

let%expect_test _ =
  parse "(3:foo)(3:foo)";
  [%expect {| Error "data after canonical S-expression" |}]

let%expect_test _ =
  parse "(3:foo)additional_stuff";
  [%expect {| Error "data after canonical S-expression" |}]

let parse_many s =
  match parse_string_many s with
  | Error (_, msg) -> print_parsed (Error msg)
  | Ok xs -> xs |> List.iter (fun x -> print_parsed (Ok x))

let%expect_test "parse_string_many - parse empty string" =
  parse_many "";
  [%expect {| |}]

let%expect_test "parse_string_many - parse a single csexp" =
  parse_many "(3:foo)";
  [%expect {| Ok "(3:foo)" |}]

let%expect_test "parse_string_many - parse many csexp" =
  parse_many "(3:foo)(3:bar)";
  [%expect {| Ok "(3:foo)"Ok "(3:bar)" |}]
