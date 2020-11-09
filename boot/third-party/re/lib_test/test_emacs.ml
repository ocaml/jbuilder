open Re
open Re.Emacs
open Fort_unit

let eq_re r s = expect_equal_app ~msg:s id r re s
;;

(* 
 * Tests based on description of emacs regular expressions given at
 *   http://www.gnu.org/manual/elisp-manual-20-2.5/html_chapter/elisp_34.html
 *)

let _ =

  expect_pass "ordinary characters" (fun () ->
    eq_re (char 'a')                      "a";
  );

  expect_pass "concatenation" (fun () ->
    eq_re (seq [char 'a'; char 'b'])      "ab";
  );

  expect_pass "escaping special characters" (fun () ->
    eq_re (char '.')                      "\\.";
    eq_re (char '*')                      "\\*";
    eq_re (char '+')                      "\\+";
    eq_re (char '?')                      "\\?";
    eq_re (char '[')                      "\\[";
    eq_re (char ']')                      "\\]";
    eq_re (char '^')                      "\\^";
    eq_re (char '$')                      "\\$";
    eq_re (char '\\')                     "\\\\";
  );

  expect_pass "special characters" (fun () ->
    eq_re notnl                           ".";
    eq_re (rep (char 'a'))                "a*";
    eq_re (rep1 (char 'a'))               "a+";
    eq_re (opt (char 'a'))                "a?";
    eq_re (alt [char 'b'; char 'a'])      "[ab]";
    eq_re (rg 'a' 'z')                    "[a-z]";
    eq_re (alt [char '.'; char '%'; char '$'; rg 'a' 'z'])
                                          "[a-z$%.]";
    eq_re (alt [char 'a'; char ']'])      "[]a]";
    eq_re (alt [char ']'; char '-'])      "[]-]";
    eq_re (alt [char '^'; char 'a'])      "[a^]";
    eq_re (compl [rg 'a' 'z'])            "[^a-z]";
    eq_re (compl [char '$'; rg 'a' 'z'])  "[^a-z$]";
    eq_re bol                             "^";
    eq_re eol                             "$";
  );

  expect_pass "historical compatibility (not supported)" (fun () ->
    expect_equal_app
      (fun () -> raise Parse_error) ()
      re "*ab";
    expect_equal_app
      (fun () -> raise Parse_error) ()
      re "+ab";
    expect_equal_app
      (fun () -> raise Parse_error) ()
      re "?ab";
  );

  expect_pass "alternative" (fun () ->
    eq_re (alt [char 'a'; char 'b'])      "a\\|b";
    eq_re (alt [seq [char 'a'; char 'a']; seq [char 'b'; char 'b']])
                                          "aa\\|bb";
  );

  expect_pass "grouping" (fun () ->
    eq_re (group (char 'a'))              "\\(a\\)";
    eq_re (seq [group (alt [char 'a'; char 'b']); char 'c'])
                                          "\\(a\\|b\\)c"
  );

  expect_pass "backreferences" (fun () ->
    expect_equal_app
      (fun () -> raise Not_supported) ()
      re "\\0"
  );

  expect_pass "word-constituent" (fun () ->
    eq_re (alt [alnum; char '_'])         "\\w";
    eq_re (compl [alnum; char '_'])       "\\W";
  );

  (* syntax codes... ? *)

  expect_pass "contexts" (fun () ->
    eq_re bos                             "\\`";
    eq_re eos                             "\\'";
    eq_re start                           "\\=";
    eq_re (alt [bow; eow])                "\\b";
    eq_re not_boundary                    "\\B";
    eq_re bow                             "\\<";
    eq_re eow                             "\\>";
  );
  run_test_suite "test_emacs"
