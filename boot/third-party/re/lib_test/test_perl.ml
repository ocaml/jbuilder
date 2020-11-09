open Re
open Re.Perl
open Fort_unit

let eq_re ?opts r s = expect_equal_app ~msg:s id r (re ?opts) s
;;

let parse_error_re ?opts s =
  try ignore (re ?opts s); OUnit2.assert_failure s with
  | Re.Perl.Parse_error -> ()

(* 
 * Tests based on description of Perl regular expressions given at
 *   http://www.perl.com/CPAN-local/doc/manual/html/pod/perlre.html
*)

let _ =
  expect_pass "ordinary characters" (fun () ->
    eq_re (char 'a')                      "a";
  );

  expect_pass "concatenation" (fun () ->
    eq_re (seq [char 'a'; char 'b'])      "ab";
  );

  expect_pass "escaping metacharacters" (fun () ->
    eq_re (char '^')                      "\\^";
    eq_re (char '.')                      "\\.";
    eq_re (char '$')                      "\\$";
    eq_re (char '|')                      "\\|";
    eq_re (char '(')                      "\\(";
    eq_re (char ')')                      "\\)";
    eq_re (char '[')                      "\\[";
    eq_re (char ']')                      "\\]";
    eq_re (char '*')                      "\\*";
    eq_re (char '+')                      "\\+";
    eq_re (char '?')                      "\\?";
    eq_re (char '\\')                     "\\\\";
  );

  expect_pass "basic metacharacters" (fun () ->
    eq_re bos                             "^";
    eq_re notnl                           ".";
    eq_re eos                             "$";

    eq_re (alt [char 'a'; char 'b'])      "a|b";
    eq_re (alt [seq [char 'a'; char 'a']; seq [char 'b'; char 'b']])
      "aa|bb";
    eq_re (group (char 'a'))              "(a)";
    eq_re (seq [group (alt [char 'a'; char 'b']); char 'c'])
      "(a|b)c";

    eq_re (alt [char 'b'; char 'a'])      "[ab]";
    eq_re (rg 'a' 'z')                    "[a-z]";
    eq_re (alt [char '.'; char '%'; char '$'; rg 'a' 'z'])
      "[a-z$%.]";
    eq_re (alt [char 'z'; char 'a'; char '-'])
      "[-az]";
    eq_re (alt [char 'z'; char '-'; char 'a'])
      "[az-]";
    eq_re (alt [char 'z'; char '-'; char 'a'])
      "[a\\-z]";
    eq_re (alt [char 'a'; char ']'])      "[]a]";
    eq_re (alt [char ']'; char '-'])      "[]-]";
    eq_re (alt [char '^'; char 'a'])      "[a^]";
    eq_re (compl [rg 'a' 'z'])            "[^a-z]";
    eq_re (compl [char '$'; rg 'a' 'z'])  "[^a-z$]";
    eq_re (alt [char 'z'; char 'a'; char '-'; space])
      "[a-\\sz]";
    parse_error_re                        "[\\";
    parse_error_re                        "[a-\\s";
  );

  expect_pass "greedy quantifiers" (fun () ->
    eq_re (greedy (rep (char 'a')))       "a*";
    eq_re (greedy (rep1 (char 'a')))      "a+";
    eq_re (greedy (opt (char 'a')))       "a?";
    eq_re (greedy (repn (char 'a') 10 (Some 10)))
      "a{10}";
    eq_re (greedy (repn (char 'a') 10 None))
      "a{10,}";
    eq_re (greedy (repn (char 'a') 10 (Some 12)))
      "a{10,12}";
  );

  expect_pass "non-greedy quantifiers" (fun () ->
    eq_re (non_greedy (rep (char 'a')))   "a*?";
    eq_re (non_greedy (rep1 (char 'a')))  "a+?";
    eq_re (non_greedy (opt (char 'a')))   "a??";
    eq_re (non_greedy (repn (char 'a') 10 (Some 10)))
      "a{10}?";
    eq_re (non_greedy (repn (char 'a') 10 None))
      "a{10,}?";
    eq_re (non_greedy (repn (char 'a') 10 (Some 12)))
      "a{10,12}?";
  );

  (* escape sequences (\t, etc) ? *)

  expect_pass "character sets" (fun () ->
    eq_re (alt [alnum; char '_'])         "\\w";
    eq_re (compl [alnum; char '_'])       "\\W";
    eq_re space                           "\\s";
    eq_re (compl [space])                 "\\S";
    eq_re digit                           "\\d";
    eq_re (compl [digit])                 "\\D";
  );

  expect_pass "sets in classes" (fun () ->
    eq_re (alt [space; char 'a'])      "[a\\s]";
  );

  expect_pass "zero-width assertions" (fun () ->
    eq_re (alt [bow; eow])                "\\b";
    eq_re not_boundary                    "\\B";

    eq_re bos                             "\\A";
    eq_re leol                            "\\Z";
    eq_re eos                             "\\z";
    eq_re start                           "\\G";
  );

  expect_pass "backreferences" (fun () ->
    expect_equal_app
      (fun () -> raise Not_supported) ()
      re "\\0"
  );

  expect_pass "comments" (fun () ->
    eq_re (seq [char 'a'; epsilon; char 'b'])
      "a(?#comment)b";
    parse_error_re "(?#";
  );

  expect_pass "clustering" (fun () ->
    (* modifier support ? *)
    eq_re (char 'a')                      "(?:a)";
    eq_re (seq [alt [char 'a'; char 'b']; char 'c'])
      "(?:a|b)c";
  );

  (* lookahead assertions *)

  (* independent subexpression *)

  (* conditional expression *)

  (* pattern-match modifiers *)

  expect_pass "options" (fun () ->
    eq_re ~opts:[`Anchored]
      (seq [start; char 'a'])             "a";
    eq_re ~opts:[`Caseless]
      (no_case (char 'b'))                "b";
    eq_re ~opts:[`Dollar_endonly]
      leol                                "$";
    eq_re ~opts:[`Dollar_endonly; `Multiline]
      eol                                 "$";
    eq_re ~opts:[`Dotall]
      any                                 ".";
    (* Extended ? *)
    eq_re ~opts:[`Multiline]
      bol                                 "^";
    eq_re ~opts:[`Multiline]
      eol                                 "$";
    eq_re ~opts:[`Ungreedy]
      (non_greedy (rep (char 'a')))       "a*";
    eq_re ~opts:[`Ungreedy]
      (greedy (rep (char 'a')))           "a*?";
  );
  run_test_suite "test_perl"

