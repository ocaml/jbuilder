open Stdune
open Dune_rules
module Re = Dune_re

let () =
  Path.set_root (Path.External.cwd ());
  Path.Build.set_build_dir (Path.Build.Kind.of_string "_build")

let fail fmt =
  Printf.ksprintf
    (fun msg ->
      prerr_endline msg;
      exit 1)
    fmt

(* {1 encoding/decoding tests} Test that encoding and decoding round trip *)

let () =
  for n = 0 to 3 do
    let test ?min_len subst =
      let subst' =
        subst
        |> Artifact_substitution.encode ?min_len
        |> Artifact_substitution.decode
      in
      if subst' <> Some subst then
        fail
          "encode and decode don't round trip!\n\
           subst:                     %s\n\
           subst |> encode:           %S\n\
           subst |> encode |> decode: %s"
          (Dyn.to_string (Artifact_substitution.to_dyn subst))
          (Artifact_substitution.encode subst ?min_len)
          (match subst' with
          | None -> "-"
          | Some x -> Dyn.to_string (Artifact_substitution.to_dyn x))
    in
    let test s =
      let value = Artifact_substitution.Repeat (n, s) in
      test value;
      let len = String.length (Artifact_substitution.encode value) in
      for i = -2 to 2 do
        test value ~min_len:(len + i)
      done
    in
    test "";
    test "x";
    test "xyz";
    test (String.make 100 'x')
  done

(* {1 copy tests} *)

(* {2 Test harness}

   The test harness implements a slower but much simpler version of the
   substution algorithm and compare the result for various inputs between the
   simpler implementation and the real one. *)

let simple_subst =
  let re =
    Re.compile
      (Re.seq
         [ Re.str "%%DUNE_PLACEHOLDER:"
         ; Re.group (Re.rep1 Re.digit)
         ; Re.char ':'
         ])
  in
  fun s ->
    let slen = String.length s in
    let extract_placeholder pos =
      let open Option.O in
      (* Look at the beginning manually otherwise it's too slow *)
      if
        pos + 3 >= slen
        || s.[pos] <> '%'
        || s.[pos + 1] <> '%'
        || s.[pos + 2] <> 'D'
      then
        None
      else
        let* groups = Re.exec_opt re s ~pos in
        let* len =
          let len = Re.Group.get groups 1 in
          match int_of_string len with
          | n -> Some n
          | exception _ -> None
        in
        if pos + len > slen then
          None
        else
          let* p = Artifact_substitution.decode (String.sub s ~pos ~len) in
          Some (len, p)
    in
    let buf = Buffer.create slen in
    let rec loop pos =
      if pos = slen then
        Buffer.contents buf
      else
        match extract_placeholder pos with
        | None ->
          Buffer.add_char buf s.[pos];
          loop (pos + 1)
        | Some (len, subst) ->
          Buffer.add_string buf
            (Artifact_substitution.encode_replacement ~len
               ~repl:
                 (match subst with
                 | Repeat (n, s) ->
                   Array.make n s |> Array.to_list |> String.concat ~sep:""
                 | _ -> failwith "substitution value not supported"));
          loop (pos + len)
    in
    loop 0

(* Replace long sequences of the same character by the character followed by a
   number between "\\{" and "}" *)
let compress_string s =
  let buf = Buffer.create (String.length s * 2) in
  let chain_length = ref 0 in
  let last_char = ref '\000' in
  let commit_chain () =
    let s = Char.escaped !last_char in
    if !chain_length > 5 then
      Printf.bprintf buf "%s\\{%d}" s !chain_length
    else
      for _i = 1 to !chain_length do
        Buffer.add_string buf s
      done
  in
  for i = 0 to String.length s - 1 do
    let c = s.[i] in
    if c = !last_char then
      incr chain_length
    else (
      commit_chain ();
      last_char := c;
      chain_length := 1
    )
  done;
  commit_chain ();
  Buffer.contents buf

let test input =
  let expected = simple_subst input in
  let buf = Buffer.create (String.length expected) in
  Fiber.run
    ~iter:(fun () -> assert false)
    (let ofs = ref 0 in
     let input buf pos len =
       let to_copy = min len (String.length input - !ofs) in
       Bytes.blit_string ~src:input ~dst:buf ~src_pos:!ofs ~dst_pos:pos
         ~len:to_copy;
       ofs := !ofs + to_copy;
       to_copy
     in
     let output = Buffer.add_subbytes buf in
     Artifact_substitution.copy ~conf:Artifact_substitution.conf_dummy
       ~input_file:(Path.of_string "<memory>")
       ~input ~output);
  let result = Buffer.contents buf in
  if result <> expected then
    fail
      "Got invalid result!\n\
       Input:    \"%s\"\n\
       Expected: \"%s\"\n\
       Result:   \"%s\"" (compress_string input) (compress_string expected)
      (compress_string result)

(* {2 Test cases} *)

let () =
  test "";
  test "lkdjflskfjlksdf"

let () =
  test
    (String.concat ~sep:""
       [ "foo "; Artifact_substitution.encode (Repeat (2, "xyz")); " bar" ])

let () =
  let s = Artifact_substitution.encode (Repeat (2, "xyz")) in
  let testf fmt = Printf.ksprintf test fmt in
  testf "%s" s;
  testf "%%%%%s%%%%" s;
  for i = 0 to String.length s do
    testf "%s%s" (String.sub s ~pos:0 ~len:i) s;
    testf "%s%s" s (String.sub s ~pos:0 ~len:i);
    testf "%s%s%s" (String.sub s ~pos:0 ~len:i) (String.sub s ~pos:0 ~len:i) s;
    testf "%*s%s" (65536 - i) "" s;
    testf "%*s%s%s" (65536 - i) "" (String.sub s ~pos:0 ~len:i) s
  done
