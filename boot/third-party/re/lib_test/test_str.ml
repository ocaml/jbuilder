open Fort_unit
open OUnit2

module type Str_intf = module type of Str

module Test_matches (R : Str_intf) = struct
  let groups () =
    let group i =
      try `Found (R.group_beginning i)
      with
      | Not_found -> `Not_found
      | Invalid_argument _ -> `Not_exists in
    let rec loop acc i =
      match group i with
      | `Found p -> loop ((p, R.group_end i)::acc) (i + 1)
      | `Not_found -> loop ((-1, -1)::acc) (i + 1)
      | `Not_exists -> List.rev acc in
    loop [] 0

  let eq_match ?(pos=0) ?(case=true) r s =
    let pat = if case then R.regexp r else R.regexp_case_fold r in
    try
      ignore (R.search_forward pat s pos);
      Some (groups ())
    with Not_found -> None
end

module T_str = Test_matches(Str)
module T_re = Test_matches(Re.Str)

let eq_match ?pos ?case r s =
  expect_equal_app
    ~msg:(str_printer s)
    ~printer:(opt_printer (list_printer ofs_printer))
    (fun () -> T_str.eq_match ?pos ?case r s) ()
    (fun () -> T_re.eq_match ?pos ?case r s) ()
;;

let split_result_conv = List.map (function
    | Str.Delim x -> Re.Str.Delim x
    | Str.Text x -> Re.Str.Text x)

let pp_split_result_list =
  Fmt.pp_olist (fun fmt x ->
      let (tag, arg) =
        match x with
        | Re.Str.Delim x -> ("Delim", x)
        | Re.Str.Text x -> ("Text", x) in
      Fmt.fprintf fmt "%s@ (\"%s\")" tag arg)

let pp_fs pp_args pp_out fmt (name, re, args, ex, res) =
  let f fmt (mod_, r) =
    Fmt.fprintf fmt "%s.%s %a %a = %a"
      mod_ name Fmt.quote re pp_args args pp_out r in
  Fmt.fprintf fmt "@.%a@.%a"
    f ("Str", ex)
    f ("Re.Str", res)

type ('a, 'b) test =
  { name: string
  ; pp_args : 'a Fmt.t
  ; pp_out : 'b Fmt.t
  ; re_str: Re.Str.regexp -> 'a -> 'b
  ; str: Str.regexp -> 'a -> 'b }

let bounded_split_t =
  { name = "bounded_split"
  ; pp_args = (fun fmt (s, n) -> Fmt.fprintf fmt "%a %d" Fmt.quote s n)
  ; pp_out = Fmt.pp_str_list
  ; re_str = (fun re (s, n) -> Re.Str.bounded_split re s n)
  ; str = (fun re (s, n) -> Str.bounded_split re s n) }

let bounded_full_split_t =
  { bounded_split_t with
    name = "bounded_full_split"
  ; pp_out = pp_split_result_list
  ; re_str = (fun re (s, n) -> Re.Str.bounded_full_split re s n)
  ; str = (fun re (s, n) ->
      split_result_conv (Str.bounded_full_split re s n)) }

let full_split_t =
  { bounded_full_split_t with
    name = "full_split"
  ; pp_args = (fun fmt s -> Fmt.fprintf fmt "%a" Fmt.quote s)
  ; re_str = (fun re s -> Re.Str.full_split re s)
  ; str = (fun re s -> split_result_conv (Str.full_split re s)) }

let split_delim_t =
  { full_split_t with
    name = "split_delim"
  ; pp_out = Fmt.pp_str_list
  ; re_str = (fun re s -> Re.Str.split_delim re s)
  ; str = (fun re s -> Str.split_delim re s) }

let split_t =
  { name = "split"
  ; pp_out = Fmt.pp_str_list
  ; pp_args = full_split_t.pp_args
  ; re_str = (fun re s -> Re.Str.split re s)
  ; str = (fun re s -> Str.split re s) }

let global_replace_t =
  { name = "global_replace"
  ; pp_out = Fmt.pp_print_string
  ; pp_args = (fun fmt (r, s) -> Fmt.fprintf fmt "%a %a"
                  Fmt.quote r Fmt.quote s)
  ; re_str = (fun re (r, s) -> Re.Str.global_replace re r s)
  ; str = (fun re (r, s) -> Str.global_replace re r s) }

let test t re args =
  assert_equal
    ~pp_diff:(fun fmt (ex, act) ->
        pp_fs t.pp_args t.pp_out fmt (t.name, re, args, ex, act))
    ~printer:(Fmt.to_to_string t.pp_out)
    (t.re_str (Re.Str.regexp re) args)
    (t.str (Str.regexp re) args)

let split_delim re s = test split_delim_t re s

let split re s = test split_t re s

let full_split re s = test full_split_t re s

let bounded_split re s n = test bounded_split_t re (s, n)

let bounded_full_split re s n = test bounded_full_split_t re (s, n)

let global_replace re r s = test global_replace_t re (r, s)

let _ =
  (* Literal Match *)
  expect_pass "str" (fun () ->
    eq_match  "a"                 "a";
    eq_match  "a"                 "b";
  );

  (* Basic Operations *)

  expect_pass "alt" (fun () ->
    eq_match  "a\\|b"              "a";
    eq_match  "a\\|b"              "b";
    eq_match  "a\\|b"              "c";
  );

  expect_pass "seq" (fun () ->
    eq_match  "ab"                "ab";
    eq_match  "ab"                "ac";
  );

  expect_pass "epsilon" (fun () ->
    eq_match  ""                  "";
    eq_match  ""                  "a";
  );

  expect_pass "rep" (fun () ->
    eq_match  "a*"                "";
    eq_match  "a*"                "a";
    eq_match  "a*"                "aa";
    eq_match  "a*"                "b";
  );

  expect_pass "rep1" (fun () ->
    eq_match  "a+"                "a";
    eq_match  "a+"                "aa";
    eq_match  "a+"                "";
    eq_match  "a+"                "b";
  );

  expect_pass "opt" (fun () ->
    eq_match  "a?"                "";
    eq_match  "a?"                "a";
  );

  (* String, line, word *)

  expect_pass "bol" (fun () ->
    eq_match  "^a"                "ab";
    eq_match  "^a"                "b\na";
    eq_match  "^a"                "ba";
  );

  expect_pass "eol" (fun () ->
    eq_match  "a$"                "ba";
    eq_match  "a$"                "a\nb";
    eq_match  "a$"                "ba\n";
    eq_match  "a$"                "ab";
  );

  expect_pass "start" (fun () ->
    eq_match ~pos:1 "Za"         "xab";
    eq_match ~pos:1 "Za"         "xb\na";
    eq_match ~pos:1 "Za"         "xba";
  );

  (* Match semantics *)

  expect_pass "match semantics" (fun () ->
    eq_match "\\(a\\|b\\)*b"         "aabaab";
    eq_match "aa\\|aaa"            "aaaa";
    eq_match "aaa\\|aa"            "aaaa";
  );

  (* Group (or submatch) *)

  (* TODO: infinite loop *)
  expect_pass "group" (fun () ->
    eq_match "\\(a\\)\\(a\\)?\\(b\\)"   "ab";
  );

  (* Character set *)

  expect_pass "rg" (fun () ->
    eq_match "[0-9]+"             "0123456789";
    eq_match "[0-9]+"             "a";
  );

  expect_pass "compl" (fun () ->
    eq_match "[^0-9a-z]+"         "A:Z+";
    eq_match "[^0-9a-z]+"         "0";
    eq_match "[^0-9a-z]+"         "a";
  );

  (* Case modifiers *)

  expect_pass "no_case" (fun () ->
    eq_match ~case:false "abc"    "abc";
    eq_match ~case:false "abc"    "ABC";
  );

  expect_pass "global_replace" (fun () ->
    global_replace "needle" "test" "needlehaystack";
    global_replace "needle" "" "";
    global_replace "needle" "" "needle";
    global_replace "xxx" "yyy" "zzz";

    global_replace "test\\([0-9]*\\)" "\\1-foo-\\1" "test100 test200 test";
    global_replace "test\\([0-9]*\\)" "'\\-0'" "test100 test200 test";

    (* Regrssion test for #129 *)
    global_replace "\\(X+\\)" "A\\1YY" "XXXXXXZZZZ"
  );

  expect_pass "bounded_split, bounded_full_split" (fun () ->
      List.iter (fun (re, s, n) ->
          bounded_full_split re s n;
          bounded_split re s n)
        [ ",", "foo,bar,baz", 5
        ; ",", "foo,bar,baz", 1
        ; ",", "foo,bar,baz", 0
        ; ",\\|",  "foo,bar|baz", 4 ]
    );

  expect_pass "split, full_split, split_delim" (fun () ->
      List.iter (fun (re, s) ->
          split re s;
          full_split re s;
          split_delim re s)
        [ "re", ""
        ; " ", "foo bar"
        ; "\b", "one-two three"
        ; "[0-9]", "One3TwoFive"]
    );

  run_test_suite "test_str"
