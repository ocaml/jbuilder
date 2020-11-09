(* ounit compatibility layer for fort tests *)
open OUnit2

type ('a, 'b) either =
  | Left of 'a
  | Right of 'b

let str_of_either f g = function
  | Left a -> f a
  | Right b -> g b

let try_with f = try Right (f ()) with exn -> Left exn

let expect_equal_app ?printer ?msg f x g y =
  let fx = try_with (fun () -> f x) in
  let gy = try_with (fun () -> g y) in
  let printer =
    let right x =
      match printer with
      | None -> "<no printer>"
      | Some p -> p x in
    str_of_either Printexc.to_string right in
  assert_equal ~printer ?msg fx gy

let collected_tests = ref []

let id x = x
let not_found () = raise Not_found

let bool_printer i = Printf.sprintf "%b" i
let int_printer i = Printf.sprintf "%d" i
let str_printer s = "\"" ^ String.escaped s ^ "\""
let ofs_printer (i0,i1) = Printf.sprintf "(%d,%d)" i0 i1
let list_printer f l =
   "[" ^ (String.concat "; " (List.map f l)) ^ "]"
let arr_printer f a =
   "[|" ^ (String.concat "; " (List.map f (Array.to_list a))) ^ "|]"
let opt_printer f = function
  | None -> "<None>"
  | Some s -> "Some (" ^ (f s) ^ ")"

let arr_str_printer = arr_printer str_printer
let arr_ofs_printer = arr_printer ofs_printer
let list_ofs_printer = list_printer ofs_printer

let fail = assert_failure

let expect_eq_bool ?msg f x g y =
  expect_equal_app ?msg ~printer:string_of_bool f x g y
let expect_eq_str ?msg f x g y =
  expect_equal_app ?msg ~printer:str_printer f x g y
let expect_eq_ofs ?msg f x g y =
  expect_equal_app ?msg ~printer:ofs_printer f x g y
let expect_eq_arr_str ?msg f x g y =
  expect_equal_app ?msg ~printer:arr_str_printer f x g y
let expect_eq_arr_ofs  ?msg f x g y =
  expect_equal_app ?msg ~printer:arr_ofs_printer f x g y

let expect_eq_list_str ?msg f x g y =
  expect_equal_app ?msg ~printer:(list_printer str_printer) f x g y

let expect_pass name run =
  collected_tests := (name >:: (fun _ -> run ())) :: !collected_tests

let run_test_suite suite_name =
  run_test_tt_main (suite_name >::: !collected_tests)
