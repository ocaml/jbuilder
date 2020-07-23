open Import

(* Artifact substitutions works as follow: the substitution is encoded as a
   string of the form:

   {v %%DUNE_PLACEHOLDER:<len>:<symbolic-value>%% v}

   Where: - [<len>] is the full length of the encoded string -
   [<symbolic-value>] is an encoding of a [Value.t]

   For instance:

   "%%DUNE_PLACEHOLDER:46:vcs-describe:7:src/foo%%"

   The placeholder is padded with '%' characters to make it of length exactly
   [<len>].

   The [copy] functions recognises such strings and substitute them accordingly.
   The maximum allowed length of such a string is 65536.

   When the placeholder is substituted, the replacement is encoded as follow:

   "=<len>:<replacement><space-padding>"

   For instance:

   "=3:1.0 "

   Replacements that are too long are truncated. *)

type t =
  | Vcs_describe of Path.Source.t
  | Custom of string * Path.Build.t
  | Repeat of int * string

let to_dyn = function
  | Vcs_describe p -> Dyn.Variant ("Vcs_describe", [ Path.Source.to_dyn p ])
  | Custom (name, dir) ->
    Dyn.Variant ("Custom", [ String name; Path.Build.to_dyn dir ])
  | Repeat (n, s) -> Dyn.Variant ("Repeat", [ Int n; String s ])

let eval t ~get_vcs =
  match t with
  | Repeat (n, s) ->
    Fiber.return (Array.make n s |> Array.to_list |> String.concat ~sep:"")
  | Custom (name, dir) ->
    let f = Path.Build.relative dir (Custom_build_info.output_file name) in
    let s, _ = Build.(contents (Path.build f) |> exec) in
    Fiber.return s
  | Vcs_describe p -> (
    match get_vcs p with
    | None -> Fiber.return ""
    | Some vcs -> Vcs.describe vcs )

let encode_replacement ~len ~repl:s =
  let repl = sprintf "=%u:%s" (String.length s) s in
  match Int.compare (String.length repl) len with
  | Lt -> repl ^ String.make (len - String.length repl) ' '
  | Eq -> repl
  | Gt -> String.sub repl ~pos:0 ~len

let prefix = "%%DUNE_PLACEHOLDER:"

let prefix_len = String.length prefix

let max_len = 65536

let encode ?(min_len = 0) t =
  let suffix =
    sprintf ":%s%%%%"
      ( match t with
      | Vcs_describe p ->
        let s = Path.Source.to_string p in
        sprintf "vcs-describe:%d:%s" (String.length s) s
      | Custom (name, dir) ->
        let s = Path.Build.to_string dir in
        sprintf "custom:%s:%d:%s" name (String.length s) s
      | Repeat (n, s) -> sprintf "repeat:%d:%d:%s" n (String.length s) s )
  in
  let len =
    let len0 = prefix_len + String.length suffix in
    if len0 + 1 < 10 then
      len0 + 1
    else if len0 + 2 < 100 then
      len0 + 2
    else if len0 + 3 < 1000 then
      len0 + 3
    else if len0 + 4 < 10000 then
      len0 + 4
    else
      len0 + 5
  in
  let len = max min_len len in
  if len > max_len then
    Code_error.raise "Artifact_substitution.encode: too long"
      [ ("t", to_dyn t); ("min_len", Int min_len); ("len", Int len) ];
  let s = sprintf "%s%u%s" prefix len suffix in
  s ^ String.make (len - String.length s) '%'

(* This function is not called very often, so the focus is on readibility rather
   than speed. *)
let decode s =
  let fail () = raise_notrace Exit in
  let parse_int s =
    match Int.of_string s with
    | None -> fail ()
    | Some i -> i
  in
  let len = String.length s in
  match
    if
      len > max_len || len < 4
      || s.[0] <> '%'
      || s.[1] <> '%'
      || s.[len - 2] <> '%'
      || s.[len - 1] <> '%'
    then
      fail ();
    let dune_placeholder, len', rest =
      match String.split (String.sub s ~pos:2 ~len:(len - 4)) ~on:':' with
      | dune_placeholder :: len' :: rest -> (dune_placeholder, len', rest)
      | _ -> fail ()
    in
    if dune_placeholder <> "DUNE_PLACEHOLDER" then fail ();
    if parse_int len' <> len then fail ();
    let read_string_payload = function
      | [] -> fail ()
      | len :: rest ->
        let len = parse_int len in
        let s = String.concat rest ~sep:":" in
        for i = len to String.length s - 1 do
          if s.[i] <> '%' then fail ()
        done;
        String.sub s ~pos:0 ~len
    in
    match rest with
    | "vcs-describe" :: rest ->
      let path = Path.Source.of_string (read_string_payload rest) in
      Vcs_describe path
    | "custom" :: name :: rest -> (
      let s = read_string_payload rest in
      match String.drop_prefix ~prefix:"_build/" s with
      | None -> fail ()
      | Some s ->
        let path = Path.Build.of_string s in
        Custom (name, path) )
    | "repeat" :: repeat :: rest ->
      Repeat (parse_int repeat, read_string_payload rest)
    | _ -> fail ()
  with
  | exception Exit -> None
  | t -> Option.some_if (encode t ~min_len:len = s) t

(* Scan a buffer for "%%DUNE_PLACEHOLDER:<len>:" *)
module Scanner = struct
  (* The following module implement a scanner for special placeholders strings.
     The scanner needs to be fast as possible as it will scan every single byte
     of the input so this module is carefully written with performances in mind.

     The logic is implemented as a DFA where each OCaml function represent a
     state of the automaton.

     In the future, we expect to have a C version of the scanner so that it can
     run concurrently with OCaml code in a separate thread. This will speed up
     the promotion of large binaries from the build directory to the source
     tree.

     {1 Notations}

     In this module, [buf] is the buffer containing the data read from the
     input. [end_of_data] is the position in [buf] of the end of data read from
     the input. [pos] is the current reading position in [buf].
     [placeholder_start] represents the position in [buf] where the current
     potential placeholder starts in [buf]. *)

  (* The following type represents the possible state of the DFA. *)
  type state =
    | Scan0
    (* Initial state and state when we are not in a potential placeholder *)
    | Scan1
    (* State after seeing one '%' *)
    | Scan2
    (* State after seeing at least two '%' *)
    | Scan_prefix of int
    (* [Scan_prefix placeholer_start] is the state after seeing [pos -
       placeholer_start] characters from [prefix] *)
    | Scan_length of int * int
    (* [Scan_length (placeholer_start, acc)] is the state after seeing all of
       [prefix] and the beginning of the length field. [acc] is the length
       accumulated so far. *)
    | Scan_placeholder of int * int

  (* [Scan_placeholder (placeholer_start, len)] is the state after seeing all of
     [prefix] and the length field, i.e. just after the second ':' of the
     placeholder *)

  (* The [run] function at the end of this module is the main function that
     consume a buffer and return the new DFA state. If the beginning of
     placeholder is found before reaching the end of the buffer, [run]
     immediately returns [Scan_placeholder (placeholder_start, len)].

     The following functions represent the transition functions for each state
     of the DFA. [run] is called only at the beginning and immediately chain the
     execution to the right transition function.

     Each transition function is written as follow:

     {[ let state ~buf ~pos ~end_of_data ... = if pos < end_of_data then let c =
     Bytes.unsafe_get buf pos in let pos = pos + 1 in match c with ... else
     State (...) ]}

     i.e. it either inspect the next character and chain the execution to the
     next transition function or immediately return the DFA state if the end of
     buffer is reached. Reaching the end of buffer is handled in the [else]
     branch for performance reason: the code generated by OCaml tend to make
     following the [then] branch slightly more efficient and reaching the end of
     buffer is the less likely case. *)

  let rec scan0 ~buf ~pos ~end_of_data =
    if pos < end_of_data then
      let c = Bytes.unsafe_get buf pos in
      let pos = pos + 1 in
      match c with
      | '%' -> scan1 ~buf ~pos ~end_of_data
      | _ -> scan0 ~buf ~pos ~end_of_data
    else
      Scan0

  and scan1 ~buf ~pos ~end_of_data =
    if pos < end_of_data then
      let c = Bytes.unsafe_get buf pos in
      let pos = pos + 1 in
      match c with
      | '%' -> scan2 ~buf ~pos ~end_of_data
      | _ -> scan0 ~buf ~pos ~end_of_data
    else
      Scan1

  and scan2 ~buf ~pos ~end_of_data =
    if pos < end_of_data then
      let c = Bytes.unsafe_get buf pos in
      let pos = pos + 1 in
      match c with
      | '%' -> scan2 ~buf ~pos ~end_of_data
      | 'D' -> scan_prefix ~buf ~pos ~end_of_data ~placeholder_start:(pos - 3)
      | _ -> scan0 ~buf ~pos ~end_of_data
    else
      Scan2

  and scan_prefix ~buf ~pos ~end_of_data ~placeholder_start =
    if pos < end_of_data then
      let pos_in_prefix = pos - placeholder_start in
      let c = Bytes.unsafe_get buf pos in
      let pos = pos + 1 in
      match c with
      | '%' -> scan1 ~buf ~pos ~end_of_data
      | c ->
        if c = prefix.[pos_in_prefix] then
          if pos_in_prefix = prefix_len - 1 then
            scan_length ~buf ~pos ~end_of_data ~placeholder_start ~acc:0
          else
            scan_prefix ~buf ~pos ~end_of_data ~placeholder_start
        else
          scan0 ~buf ~pos ~end_of_data
    else
      Scan_prefix placeholder_start

  and scan_length ~buf ~pos ~end_of_data ~placeholder_start ~acc =
    if pos < end_of_data then
      let c = Bytes.unsafe_get buf pos in
      let pos = pos + 1 in
      match c with
      | '%' -> scan1 ~buf ~pos ~end_of_data
      | '0' .. '9' as c ->
        let n = Char.code c - Char.code '0' in
        let acc = (acc * 10) + n in
        if acc = 0 || acc > max_len then
          (* We don't allow leading zeros in length fields and a length of [0]
             is not possible, so [acc = 0] here correspond to an invalid
             placeholder *)
          scan0 ~buf ~pos ~end_of_data
        else
          scan_length ~buf ~pos ~end_of_data ~placeholder_start ~acc
      | ':' ->
        if acc < pos - placeholder_start then
          (* If the length is too small, then this is surely not a valid
             placeholder *)
          scan0 ~buf ~pos ~end_of_data
        else
          Scan_placeholder (placeholder_start, acc)
      | _ -> scan0 ~buf ~pos ~end_of_data
    else
      Scan_length (placeholder_start, acc)

  let run state ~buf ~pos ~end_of_data =
    match state with
    | Scan0 -> scan0 ~buf ~pos ~end_of_data
    | Scan1 -> scan1 ~buf ~pos ~end_of_data
    | Scan2 -> scan2 ~buf ~pos ~end_of_data
    | Scan_prefix placeholder_start ->
      scan_prefix ~buf ~pos ~end_of_data ~placeholder_start
    | Scan_length (placeholder_start, acc) ->
      scan_length ~buf ~pos ~end_of_data ~placeholder_start ~acc
    | Scan_placeholder _ -> state
end

let buf_len = max_len

let buf = Bytes.create buf_len

(** The copy algorithm works as follow:

    {v
       read some data from the input
                |
                |
                v
        feed data to [Scanner.run]<-----------------------------------------\
                |                                                           |
                |                                                           |
                v                                                           |
   commit all the data we are sure are not                                  |
     part of a placeholder to the output                                    |
                |                                                           |
                |                                                           |
                v                                                           |
 was the beginning of a placeholder found by [Scanner.run]?                 |
          (i.e. "%%DUNE_PLACEHOLDER:<len>:")                                |
 and if yes, is the whole placeholder currently in [buf]?                   |
        |                                         |                         |
        | YES                                     | NO                      |
        v                                         v                         |
 extract the placeholder                   read more data from the input    |
 and try to parse it with                         |                         |
 [Artifact_substitution.decode]                   \-------------------------|
 |                            |                                             |
 | SUCCESS                    |                                             |
 v                            |                                             |
output the replacement        |                                             |
 |                            | FAILURE                                     |
 |                            v                                             |
 |                    consider that this                                    |
 |                    wasn't a placeholder                                  |
 |                            |                                             |
 |                            \---------------------------------------------|
 |                                                                          |
 \--------------------------------------------------------------------------/
    v} *)
let copy ~get_vcs ~input_file ~input ~output =
  let open Fiber.O in
  let rec loop scanner_state ~beginning_of_data ~pos ~end_of_data =
    let scanner_state = Scanner.run scanner_state ~buf ~pos ~end_of_data in
    let placeholder_start =
      match scanner_state with
      | Scan0 -> end_of_data
      | Scan1 -> end_of_data - 1
      | Scan2 -> end_of_data - 2
      | Scan_prefix placeholder_start
      | Scan_length (placeholder_start, _)
      | Scan_placeholder (placeholder_start, _) ->
        placeholder_start
    in
    (* All the data before [placeholder_start] can be sent to the output
       immediately since we know for sure that they are not part of a
       placeholder *)
    if placeholder_start > beginning_of_data then
      output buf beginning_of_data (placeholder_start - beginning_of_data);
    let leftover = end_of_data - placeholder_start in
    match scanner_state with
    | Scan_placeholder (placeholder_start, len) when len <= leftover -> (
      let placeholder = Bytes.sub_string buf ~pos:placeholder_start ~len in
      match decode placeholder with
      | Some t ->
        let* s = eval t ~get_vcs in
        ( if !Clflags.debug_artifact_substitution then
          let open Pp.O in
          Console.print
            [ Pp.textf "Found placeholder in %s:"
                (Path.to_string_maybe_quoted input_file)
            ; Pp.enumerate ~f:Fun.id
                [ Pp.text "placeholder: " ++ Dyn.pp (to_dyn t)
                ; Pp.text "evaluates to: " ++ Dyn.pp (String s)
                ]
            ] );
        let s = encode_replacement ~len ~repl:s in
        output (Bytes.unsafe_of_string s) 0 len;
        let pos = placeholder_start + len in
        loop Scan0 ~beginning_of_data:pos ~pos ~end_of_data
      | None ->
        (* Restart just after [prefix] since we know for sure that a placeholder
           cannot start before that. *)
        loop Scan0 ~beginning_of_data:placeholder_start
          ~pos:(placeholder_start + prefix_len)
          ~end_of_data )
    | scanner_state -> (
      (* We reached the end of the buffer: move the leftover data back to the
         beginning of [buf] and refill the buffer *)
      if leftover > 0 then
        Bytes.blit ~src:buf ~dst:buf ~src_pos:placeholder_start ~dst_pos:0
          ~len:leftover;
      (* Reset [placeholder_start] to [0] since we moved back the leftover data
         to the beginning of [buf] *)
      let scanner_state : Scanner.state =
        match scanner_state with
        | Scan0
        | Scan1
        | Scan2 ->
          scanner_state
        | Scan_prefix _ -> Scan_prefix 0
        | Scan_length (_, acc) -> Scan_length (0, acc)
        | Scan_placeholder (_, len) -> Scan_placeholder (0, len)
      in
      match input buf leftover (buf_len - leftover) with
      | 0 -> (
        match scanner_state with
        | Scan_placeholder _ ->
          (* There might still be another placeholder after this invalid one
             with a length that is too long *)
          loop Scan0 ~beginning_of_data:0 ~pos:prefix_len ~end_of_data:leftover
        | _ ->
          (* Nothing more to read; [leftover] is definitely not the beginning of
             a placeholder, send it and end the copy *)
          output buf 0 leftover;
          Fiber.return () )
      | n ->
        loop scanner_state ~beginning_of_data:0 ~pos:leftover
          ~end_of_data:(leftover + n) )
  in
  match input buf 0 buf_len with
  | 0 -> Fiber.return ()
  | n -> loop Scan0 ~beginning_of_data:0 ~pos:0 ~end_of_data:n

let copy_file ~get_vcs ?chmod ~src ~dst () =
  let ic, oc = Io.setup_copy ?chmod ~src ~dst () in
  Fiber.finalize
    ~finally:(fun () ->
      Io.close_both (ic, oc);
      Fiber.return ())
    (fun () ->
      copy ~get_vcs ~input_file:src ~input:(input ic) ~output:(output oc))
