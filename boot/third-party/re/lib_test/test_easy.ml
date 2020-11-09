(* Tests for Re higher-level functions *)

open OUnit

module String = struct
  [@@@ocaml.warning "-32-3"]
  let capitalize_ascii   = String.capitalize
  let uncapitalize_ascii = String.uncapitalize
  let uppercase_ascii    = String.uppercase
  let lowercase_ascii    = String.lowercase

  include String
end

let pp_str x = x
let quote = Printf.sprintf "'%s'"
let pp_list l =
  l
  |> List.map quote
  |> String.concat ", "
  |> Printf.sprintf "[ %s ]"

let re_whitespace = Re.Posix.compile_pat "[\t ]+"
let re_empty = Re.Posix.compile_pat ""
let re_eol = Re.compile Re.eol
let re_bow = Re.compile Re.bow
let re_eow = Re.compile Re.eow

let test_iter () =
  let re = Re.Posix.compile_pat "(ab)+" in
  assert_equal ~printer:pp_list
    ["abab"; "ab"; "ab"] (Re.matches re "aabab aaabba  dab ");
  assert_equal ~printer:pp_list
    ["ab"; "abab"] (Re.matches ~pos:2 ~len:7 re "abab ababab");
  assert_equal ~printer:pp_list
    [""; ""] (Re.matches re_empty "ab");
  ()

let test_split () =
  assert_equal ~printer:pp_list
    ["aa"; "bb"; "c"; "d"] (Re.split re_whitespace "aa bb c d ");
  assert_equal ~printer:pp_list
    ["a"; "b"] (Re.split ~pos:1 ~len:4 re_whitespace "aa b c d");
  assert_equal ~printer:pp_list
    ["a"; "full_word"; "bc"] (Re.split re_whitespace " a full_word bc   ");
  assert_equal ~printer:pp_list
    ["a"; "b"; "c"; "d"] (Re.split re_empty "abcd");
  assert_equal ~printer:pp_list
    ["a"; "\nb"] (Re.split re_eol "a\nb");
  assert_equal ~printer:pp_list
    ["a "; "b"] (Re.split re_bow "a b");
  assert_equal ~printer:pp_list
    ["a"; " b"] (Re.split re_eow "a b");
  ()

let map_split_delim =
  List.map
    (function
      | `Text x -> `T x
      | `Delim s -> `D (Re.Group.get s 0)
    )

let pp_list' l =
  pp_list
    (List.map
      (function `T s -> s
      | `D s -> "delim '" ^ s ^ "'"
      ) l
    )

let test_split_full () =
  assert_equal ~printer:pp_list'
    [`T "aa"; `D " "; `T "bb"; `D " "; `T "c"; `D " "; `T "d"; `D " "]
    (Re.split_full re_whitespace "aa bb c d " |> map_split_delim);
  assert_equal ~printer:pp_list'
    [`T "a"; `D " \t"; `T "b"; `D " "]
    (Re.split_full ~pos:1 ~len:5 re_whitespace "aa \tb c d" |> map_split_delim);
  assert_equal ~printer:pp_list'
    [`D " "; `T "a"; `D " "; `T "full_word"; `D " "; `T "bc"; `D "   "]
    (Re.split_full re_whitespace " a full_word bc   " |> map_split_delim);
  assert_equal ~printer:pp_list'
    [`D ""; `T "a"; `D ""; `T  "b"] (* XXX: not trivial *)
    (Re.split_full re_empty "ab" |> map_split_delim);
  ()

let test_replace () =
  let re = Re.Posix.compile_pat "[a-zA-Z]+" in
  let f sub = String.capitalize_ascii (Re.Group.get sub 0) in
  assert_equal ~printer:pp_str  " Hello World; I Love Chips!"
    (Re.replace re ~f " hello world; I love chips!");
  assert_equal ~printer:pp_str " Allo maman, bobo"
    (Re.replace ~all:false re ~f " allo maman, bobo");
  ()

let test_replace_string () =
  let re = Re.Posix.compile_pat "_[a-zA-Z]+_" in
  assert_equal ~printer:pp_str "goodbye world"
    (Re.replace_string re ~by:"goodbye" "_hello_ world");
  assert_equal ~printer:pp_str "The quick brown fox"
    (Re.replace_string ~all:false re ~by:"brown" "The quick _XXX_ fox");
  ()

let test_bug_55 () =
  let re = Re.(compile bol) in
  let res = Re.replace_string re ~by:"z" "abc" in
  assert_equal ~printer:pp_str "zabc" res;
  let re = Re.(compile eow) in
  let res = Re.replace_string re ~by:"X" "one two three" in
  assert_equal ~printer:pp_str "oneX twoX threeX" res

let suite = "easy" >:::
  [ "iter" >:: test_iter
  ; "split" >:: test_split
  ; "split_full" >:: test_split_full
  ; "replace" >:: test_replace
  ; "replace_string" >:: test_replace_string
  ; "test sub 0 length matches" >:: test_bug_55
  ]

let () =
  ignore (run_test_tt_main suite)
