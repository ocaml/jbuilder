open Re.Glob
open Fort_unit

let re_match ?pos ?len re s =
  Re.execp ?pos ?len (Re.compile re) s
;;

let re_mismatch ?pos ?len re s = not (re_match ?pos ?len re s)

let _ =
  assert (re_match    (glob "foo*")   "foobar" );
  assert (re_mismatch (glob "fo?bar") "fobar"  );
  assert (re_match    (glob "fo?bar") "foobar" );
  assert (re_mismatch (glob "fo?bar") "foo0bar");
  assert (re_match    (glob "?oobar") "foobar" );
  assert (re_match    (glob "*bar")   "foobar" );
  assert (re_mismatch (glob "\\*bar") "foobar" );
  assert (re_match    (glob "\\*bar") "*bar"   );

  assert (re_match    (glob "[ab]foo")  "afoo"  );
  assert (re_match    (glob "[ab]foo")  "bfoo"  );
  assert (re_mismatch (glob "[ab]foo")  "cfoo"  );
  assert (re_mismatch (glob "c[ab]foo") "cabfoo");

  assert (re_match    (glob ".foo"   ) ".foo"  );
  assert (re_mismatch (glob ".foo"   ) "afoo"  );
  assert (re_match    (glob "*[.]foo") "a.foo" );
  assert (re_match    (glob "*[.]foo") "ba.foo");
  assert (re_mismatch (glob "*.foo"  ) ".foo"  );
  assert (re_mismatch (glob "*[.]foo") ".foo"  );

  assert (re_match    (glob ~anchored:true "*/foo") "/foo");
  assert (re_match    (glob ~anchored:true "foo/*") "foo/");

  assert (re_mismatch (glob                "/[^f]") "/foo");
  assert (re_match    (glob                "/[^f]") "/bar");
  assert (re_mismatch (glob ~anchored:true "/[^f]") "/bar");

  assert (re_mismatch (glob ~anchored:true "*") ".bar");

  assert (re_match    (glob "foo[.]bar") "foo.bar");
  assert (re_mismatch (glob "[.]foo"   ) ".foo"   );
  assert (re_mismatch (glob "foo[/]bar") "foo/bar");

  assert (re_match    (glob ~anchored:true "*bar") "foobar");

  assert (re_match    (glob                "foo") "foobar");
  assert (re_match    (glob                "bar") "foobar");
  assert (re_mismatch (glob ~anchored:true "foo") "foobar");
  assert (re_mismatch (glob ~anchored:true "bar") "foobar");

  assert (re_mismatch (glob "{foo,bar}bar") "foobar"      );
  assert (re_match    (glob "{foo,bar}bar") "{foo,bar}bar");
  assert (re_mismatch (glob "foo?bar"     ) "foo/bar"     );

  let pathname = true in
  let period = true in
  assert (re_mismatch (glob ~pathname ~period  "?oobar") ".oobar");
  assert (re_mismatch (glob ~pathname ~period  "?oobar") "/oobar");
  assert (re_mismatch (glob ~pathname ~period  "f?obar") "f/obar");
  assert (re_match    (glob ~pathname ~period  "f?obar") "f.obar");
  assert (re_match    (glob ~pathname ~period  "f*.bar") "f.bar");
  assert (re_match    (glob ~pathname ~period  "f?.bar") "fo.bar");
  assert (re_match    (glob ~pathname ~period  "/.bar")  "/.bar");
  assert (re_mismatch (glob ~pathname ~period  "*.bar")  ".bar");
  assert (re_mismatch (glob ~pathname ~period  "?")      ".");
  assert (re_mismatch (glob ~pathname ~period  "/*bar")  "/.bar");

  assert (re_mismatch (glob                     "?oobar") ".oobar");
  assert (re_mismatch (glob                     "?oobar") "/oobar");

  let pathname = true in
  let period = false in
  assert (re_mismatch (glob ~pathname ~period  "?oobar") "/oobar");
  assert (re_match    (glob ~pathname ~period  "?oobar") ".oobar");
  assert (re_mismatch (glob ~pathname ~period  "f?obar") "f/obar");
  assert (re_match    (glob ~pathname ~period  "f?obar") "f.obar");

  let pathname = false in
  let period = false in
  assert (re_match    (glob ~pathname ~period  "?oobar") ".oobar");
  assert (re_match    (glob ~pathname ~period  "?oobar") "/oobar");

  assert (re_match    (glob ~expand_braces:true "{foo,far}bar") "foobar"      );
  assert (re_match    (glob ~expand_braces:true "{foo,far}bar") "farbar"      );
  assert (re_mismatch (glob ~expand_braces:true "{foo,far}bar") "{foo,far}bar");

  (* Double asterisk *)
  let anchored = true in
  assert (re_match    (glob ~anchored "**") "foobar");
  assert (re_match    (glob ~anchored "**") "foo/bar");
  assert (re_match    (glob ~anchored "**/bar") "foo/bar");
  assert (re_match    (glob ~anchored "**/bar") "foo/far/bar");
  assert (re_mismatch (glob ~anchored "foo/**") "foo");
  assert (re_match    (glob ~anchored "foo/**") "foo/bar");
  assert (re_match    (glob ~anchored "foo/**") "foo/far/bar");
  assert (re_match    (glob ~anchored "foo/**/bar") "foo/far/bar");
  assert (re_match    (glob ~anchored "foo/**/bar") "foo/far/oof/bar");
  assert (re_match    (glob ~anchored "foo/**bar") "foo/far/oofbar");
  assert (re_match    (glob ~anchored "foo/**bar") "foo/bar");
  assert (re_match    (glob ~anchored "/**") "//foo");
  assert (re_match    (glob ~anchored "**") "foo//bar");

  (* Interaction with [~period] *)
  let period = true in
  assert (re_mismatch (glob ~anchored ~period "**") ".foobar");
  assert (re_mismatch (glob ~anchored ~period "**") ".foo/bar");
  assert (re_mismatch (glob ~anchored ~period "foo/**") "foo/.bar");
  assert (re_mismatch (glob ~anchored ~period "**") "foo/.bar/bat");
  assert (re_mismatch (glob ~anchored ~period "foo/**/bat") "foo/.bar/bat");
  assert (re_mismatch (glob ~anchored ~period "/**/bat") "/foo/.bar/bat");
  assert (re_mismatch (glob ~anchored ~period "/**/bat") "/.bar/bat");
  assert (re_mismatch (glob ~anchored ~period "/**bat") "/bar/.bat");
  assert (re_match    (glob ~anchored ~period ".**") ".foobar");
  assert (re_match    (glob ~anchored ~period ".**") ".foo/bar");
  assert (re_match    (glob ~anchored ~period "foo/.**") "foo/.bar");

  let period = false in
  assert (re_match    (glob ~anchored ~period "**") ".foobar");
  assert (re_match    (glob ~anchored ~period "**") ".foo/bar");
  assert (re_match    (glob ~anchored ~period "foo/**") "foo/.bar");
  assert (re_match    (glob ~anchored ~period "**") "foo/.bar/bat");
  assert (re_match    (glob ~anchored ~period "foo/**/bat") "foo/.bar/bat");
  assert (re_match    (glob ~anchored ~period "/**/bat") "/foo/.bar/bat");
  assert (re_match    (glob ~anchored ~period "/**/bat") "/.bar/bat");
  assert (re_match    (glob ~anchored ~period "/**bat") "/bar/.bat");

  run_test_suite "test_re";
