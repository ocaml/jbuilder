open Build_path_prefix_map

(* The test vectors are exactly those of Ximin Luo's specification:
     https://reproducible-builds.org/specs/build-path-prefix-map/#test-vectors
*)

let test_valid_map encoded_map =
  match decode_map encoded_map with
  | Error _ -> assert false
  | Ok map ->
    (fun input output ->
       String.equal (rewrite map input) output)

let () =
  print_endline "0.basic";
  let encoded_map =
    {|ERROR=/a/zzz:lol=/a:ERROR=/b/1234:foo=/b:bar=/a/yyy|} in
  let ok = test_valid_map encoded_map in
  assert (ok "/a/d" "lol/d");
  assert (ok "/b/1234" "foo/1234");
  assert (ok "/a/yyy/xxx" "bar/xxx");
  ()

let () =
  print_endline "0.ordering";
  let encoded_map =
    {|ERROR=/a/b%+yyy:lol=/a:ERROR=/b/1234:foo=/b|}
    ^ {|:libbar-3-bison++_41%.10.5-3~rc1pre3+dfsg1.1-3nmu1+b4=/a/b%+yyy|} in
  let ok = test_valid_map encoded_map in
  assert (ok "/a/d" "lol/d");
  assert (ok "/b/1234" "foo/1234");
  assert (ok "/a/b=yyy/xxx"
            "libbar-3-bison++_41:10.5-3~rc1pre3+dfsg1.1-3nmu1+b4/xxx");
  ()

let () =
  print_endline "pecsplit.0.allbytes-ok";
  let encoded_map =
    "=\x01\x02\x03\x04\x05\x06\x07\x08 \x0b\x0c \x0e\x0f\x10"
    ^ "\x11\x12\x13\x14\x15\x16\x17\x18\x19\x1a\x1b\x1c\x1d"
    ^ "\x1e\x1f !\"#$&\'()*+,-./0123456789;<>?@ABCDEFGHIJKLMNOPQRSTUVWXYZ"
    ^ "[\\]^_`abcdefghijklmnopqrstuvwxyz{|}~\x7f\x80\x81\x82\x83\x84\x85\x86"
    ^ "\x87\x88\x89\x8a\x8b\x8c\x8d\x8e\x8f\x90\x91\x92\x93\x94\x95\x96\x97"
    ^ "\x98\x99\x9a\x9b\x9c\x9d\x9e\x9f\xa0\xa1\xa2\xa3\xa4\xa5\xa6\xa7\xa8"
    ^ "\xa9\xaa\xab\xac\xad\xae\xaf\xb0\xb1\xb2\xb3\xb4\xb5\xb6\xb7\xb8\xb9"
    ^ "\xba\xbb\xbc\xbd\xbe\xbf\xc0\xc1\xc2\xc3\xc4\xc5\xc6\xc7\xc8\xc9"
    ^ "\xca\xcb\xcc\xcd\xce\xcf\xd0\xd1\xd2\xd3\xd4\xd5\xd6\xd7\xd8\xd9"
    ^ "\xda\xdb\xdc\xdd\xde\xdf\xe0\xe1\xe2\xe3\xe4\xe5\xe6\xe7\xe8\xe9"
    ^ "\xea\xeb\xec\xed\xee\xef\xf0\xf1\xf2\xf3\xf4\xf5\xf6\xf7\xf8\xf9"
    ^ "\xfa\xfb\xfc\xfd\xfe" in
  let ok = test_valid_map encoded_map in
  assert (ok "/a/d" "/a/d");
  assert (ok "/b/1234" "/b/1234");
  assert (ok "/a/b=yyy/xxx" "/a/b=yyy/xxx");
  ()

let () =
  print_endline "pecsplit.0.empty-ok";
  let encoded_map =
    ":ERROR=/a/b%+yyy:lol=/a::foo=/b"
    ^ ":libbar-3-bison++_41%.10.5-3~rc1pre3+dfsg1.1-3nmu1+b4=/a/b%+yyy:" in
  let ok = test_valid_map encoded_map in
  assert (ok "/a/d" "lol/d");
  assert (ok "/b/1234" "foo/1234");
  assert (ok "/a/b=yyy/xxx"
            "libbar-3-bison++_41:10.5-3~rc1pre3+dfsg1.1-3nmu1+b4/xxx");
  ()


let () =
  print_endline "pecsplit.0.non-utf8";
  let encoded_map =
    "result\xf1=/a/b%+yyy:lol%#%#=/a:foo%#%#=/b%#:result\xf1=/a/b%+yyy"
    ^ ":sec%.reteh=/a/b%+yyy\xf1" in
  let ok = test_valid_map encoded_map in
  assert (ok "/a/d" "lol%%/d");
  assert (ok "/b/1234" "/b/1234");
  assert (ok "/b%/1234" "foo%%/1234");
  assert (ok "/a/b=yyy\xf1/xxx" "sec:reteh/xxx");
  assert (ok "/a/b=yyy/xxx" "result\xf1/xxx");
  ()

let invalid_encoded_map encoded_map =
  match decode_map encoded_map with
  | Error _ -> true
  | Ok _ -> false

let () =
  print_endline "pecsplit.1.long-pc-1";
  assert (invalid_encoded_map
            "%%#ERROR=/a/zzz:lol=/a:ERROR=/b/1234:foo=/b:bar=/a/yyy")

let () =
  print_endline "pecsplit.1.long-pc-2";
  assert (invalid_encoded_map
            "ERROR%%#=/a/zzz:lol=/a:ERROR=/b/1234:foo=/b:bar=/a/yyy")

let () =
  print_endline "pecsplit.1.long-pc-3";
  assert (invalid_encoded_map
            "ERROR=/a/zzz:lol=%%#/a:ERROR=/b/1234:foo=/b:bar=/a/yyy")

let () =
  print_endline "pecsplit.1.long-pc-4";
  assert (invalid_encoded_map
            "ERROR=/a/zzz:lol=/a:ERROR=/b/1234:foo=/b%%#:bar=/a/yyy")

let () =
  print_endline "pecsplit.1.long-pc-5";
  assert (invalid_encoded_map
            "ERROR=/a/zzz:lol=/a:ERROR=/b/1234:foo=/b:bar=/a/yyy%%#")

let () =
  print_endline "pecsplit.1.many-=-not-ok";
  assert (invalid_encoded_map
            "ERROR=/a/b=yyy:lol=/a:foo=/b:bar=/a/byyy")

let () =
  print_endline "pecsplit.1.plain-pc-1";
  assert (invalid_encoded_map
            "%sERROR=/a/zzz:lol=/a:ERROR=/b/1234:foo=/b:bar=/a/yyy")

let () =
  print_endline "pecsplit.1.plain-pc-2";
  assert (invalid_encoded_map
            "ERROR%s=/a/zzz:lol=/a:ERROR=/b/1234:foo=/b:bar=/a/yyy")

let () =
  print_endline "pecsplit.1.plain-pc-3";
  assert (invalid_encoded_map
            "ERROR=/a/zzz:lol=%s/a:ERROR=/b/1234:foo=/b:bar=/a/yyy")

let () =
  print_endline "pecsplit.1.plain-pc-4";
  assert (invalid_encoded_map
            "ERROR=/a/zzz:lol=/a:ERROR=/b/1234:foo=/b%s:bar=/a/yyy")

let () =
  print_endline "pecsplit.1.plain-pc-5";
  assert (invalid_encoded_map
            "ERROR=/a/zzz:lol=/a:ERROR=/b/1234:foo=/b:bar=/a/yyy%s")

let () =
  print_endline "pecsplit.1.short-pc-2";
  assert (invalid_encoded_map
            "ERROR%=/a/zzz:lol=/a:ERROR=/b/1234:foo=/b:bar=/a/yyy")

let () =
  print_endline "pecsplit.1.short-pc-4";
  assert (invalid_encoded_map
            "ERROR=/a/zzz:lol=/a:ERROR=/b/1234:foo=/b%:bar=/a/yyy")

let () =
  print_endline "pecsplit.1.short-pc-5";
  assert (invalid_encoded_map
            "ERROR=/a/zzz:lol=/a:ERROR=/b/1234:foo=/b:bar=/a/yyy%")

let () =
  print_endline "pecsplit.1.zero-=-not-ok";
  assert (invalid_encoded_map
            "a/byyyERROR:lol=/a:foo=/b:bar=/a/byyy ")


let () =
  print_endline "everything ok."
