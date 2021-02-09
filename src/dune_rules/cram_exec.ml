open! Dune_engine
open Import

module Sanitizer : sig
  [@@@ocaml.warning "-32"]

  module Command : sig
    type t =
      { output : string
      ; build_path_prefix_map : string
      ; script : Path.t
      }
  end

  val impl_sanitizer :
    (Command.t -> string) -> in_channel -> out_channel -> unit

  val run_sanitizer :
       ?temp_dir:Path.t
    -> prog:Path.t
    -> argv:string list
    -> Command.t list
    -> string list Fiber.t
end = struct
  module Command = struct
    type t =
      { output : string
      ; build_path_prefix_map : string
      ; script : Path.t
      }

    let of_sexp script (csexp : Sexp.t) : t =
      match csexp with
      | List [ Atom build_path_prefix_map; Atom output ] ->
        { build_path_prefix_map; output; script }
      | _ -> Code_error.raise "Command.of_csexp: invalid csexp" []

    let to_sexp { output; build_path_prefix_map; script } : Sexp.t =
      List
        [ Atom build_path_prefix_map
        ; Atom output
        ; Atom (Path.to_absolute_filename script)
        ]
  end

  let run_sanitizer ?temp_dir ~prog ~argv commands =
    let temp_dir =
      match temp_dir with
      | Some d -> d
      | None -> Temp.create Dir ~prefix:"sanitizer" ~suffix:"unspecified"
    in
    let fname = Path.relative temp_dir in
    let stdout_path = fname "sanitizer.stdout" in
    let stdout_to = Process.Io.file stdout_path Process.Io.Out in
    let stdin_from =
      let path = fname "sanitizer.stdin" in
      let csexp = List.map commands ~f:Command.to_sexp in
      Io.with_file_out ~binary:true path ~f:(fun oc ->
          List.iter csexp ~f:(Csexp.to_channel oc));
      Process.Io.file path Process.Io.In
    in
    let open Fiber.O in
    let+ () = Process.run ~stdin_from ~stdout_to Strict prog argv in
    Io.with_file_in stdout_path ~f:(fun ic ->
        let rec loop acc =
          match Csexp.input_opt ic with
          | Ok None -> List.rev acc
          | Ok (Some (Sexp.Atom s)) -> loop (s :: acc)
          | Error error ->
            Code_error.raise "invalid csexp" [ ("error", String error) ]
          | Ok _ -> Code_error.raise "unexpected output" []
        in
        loop [])

  let impl_sanitizer f in_ out =
    set_binary_mode_in in_ true;
    set_binary_mode_out out true;
    let rec loop () =
      match Csexp.input_opt in_ with
      | Error error ->
        Code_error.raise "unable to parse csexp" [ ("error", String error) ]
      | Ok None -> ()
      | Ok (Some sexp) ->
        let command = Command.of_sexp (assert false) sexp in
        Csexp.to_channel out (Atom (f command));
        flush out;
        loop ()
    in
    loop ()
end

(* Translate a path for [sh]. On Windows, [sh] will come from Cygwin so if we
   are a real windows program we need to pass the path through [cygpath] *)
let translate_path_for_sh =
  if not Sys.win32 then
    fun fn ->
  Fiber.return (Path.to_absolute_filename fn)
  else
    fun fn ->
  let cygpath =
    let path = Env.path Env.initial in
    Bin.which ~path "cygpath"
  in
  match cygpath with
  | None -> User_error.raise [ Pp.text "Unable to find cygpath in PATH" ]
  | Some cygpath ->
    Process.run_capture_line Strict cygpath [ Path.to_absolute_filename fn ]

(* Quote a filename for sh, independently of whether we are on Windows or Unix.
   On Windows, we still generate a [sh] script so we need to quote using Unix
   conventions. *)
let quote_for_sh fn =
  let buf = Buffer.create (String.length fn + 2) in
  Buffer.add_char buf '\'';
  String.iter fn ~f:(function
    | '\'' -> Buffer.add_string buf "'\\''"
    | c -> Buffer.add_char buf c);
  Buffer.add_char buf '\'';
  Buffer.contents buf

let cram_stanzas lexbuf =
  let rec loop acc =
    match Cram_lexer.block lexbuf with
    | None -> List.rev acc
    | Some s -> loop (s :: acc)
  in
  loop []

let run_expect_test file ~f =
  let file_contents = Io.read_file ~binary:false file in
  let open Fiber.O in
  let+ expected =
    let lexbuf =
      Lexbuf.from_string file_contents ~fname:(Path.to_string file)
    in
    f lexbuf
  in
  let corrected_file = Path.extend_basename file ~suffix:".corrected" in
  if file_contents <> expected then
    Io.write_file ~binary:false corrected_file expected
  else if Path.exists corrected_file then
    Path.rm_rf corrected_file

let _BUILD_PATH_PREFIX_MAP = "BUILD_PATH_PREFIX_MAP"

let extend_build_path_prefix_map ~env ~cwd ~temp_dir =
  let s =
    Build_path_prefix_map.encode_map
      [ Some
          { source = Path.to_absolute_filename cwd; target = "$TESTCASE_ROOT" }
      ; Some { source = Path.to_absolute_filename temp_dir; target = "$TMPDIR" }
      ]
  in
  Env.update env ~var:_BUILD_PATH_PREFIX_MAP ~f:(function
    | None -> Some s
    | Some s' -> Some (s ^ ":" ^ s'))

let fprln oc fmt = Printf.fprintf oc (fmt ^^ "\n")

(* Produce the following shell code:

   {v cat <<"EOF_<n>" <data> EOF_<n> v}

   choosing [<n>] such that [CRAM_EOF_<n>] doesn't appear in [<data>]. *)
let cat_eof ~dest oc lines =
  let prln fmt = fprln oc fmt in
  let rec loop n =
    let sentinel =
      if n = 0 then
        "EOF"
      else
        sprintf "EOF%d" n
    in
    if List.mem sentinel ~set:lines then
      loop (n + 1)
    else
      sentinel
  in
  let sentinel = loop 0 in
  prln "cat >%s <<%S" (quote_for_sh (Path.to_absolute_filename dest)) sentinel;
  List.iter lines ~f:(prln "%s");
  prln "%s" sentinel

type block_result =
  { command : string list
  ; output_file : Path.t
  ; script : Path.t
  }

type metadata_entry =
  { exit_code : int
  ; build_path_prefix_map : string
  }

type full_block_result = block_result * metadata_entry

type sh_script =
  { script : Path.t
  ; cram_to_output : block_result Cram_lexer.block list
  ; metadata_file : Path.t option
  ; command_count : int
  }

let read_exit_codes_and_prefix_maps file =
  let s =
    match file with
    | None -> ""
    | Some file -> Io.read_file ~binary:true file
  in
  let rec loop acc = function
    | exit_code :: build_path_prefix_map :: entries ->
      let exit_code =
        match Int.of_string exit_code with
        | Some s -> s
        | None ->
          Code_error.raise "invalid metadata file"
            [ ("entries", Dyn.Encoder.string s)
            ; ("exit_code", Dyn.Encoder.string exit_code)
            ]
      in
      loop ({ exit_code; build_path_prefix_map } :: acc) entries
    | [ "" ]
    | [] ->
      List.rev acc
    | [ _ ] ->
      Code_error.raise "odd number of elements" [ ("s", Dyn.Encoder.string s) ]
  in
  loop [] (String.split ~on:'\000' s)

let read_and_attach_exit_codes (sh_script : sh_script) :
    full_block_result Cram_lexer.block list =
  let metadata_entries =
    read_exit_codes_and_prefix_maps sh_script.metadata_file
  in
  let rec loop acc entries blocks =
    match (blocks, entries) with
    | [], [] -> List.rev acc
    | (Cram_lexer.Comment _ as comment) :: blocks, _ ->
      loop (comment :: acc) entries blocks
    | Command block_result :: blocks, metadata_entry :: entries ->
      loop (Command (block_result, metadata_entry) :: acc) entries blocks
    | Cram_lexer.Command _ :: _, [] ->
      Code_error.raise "command without metadata" []
    | [], _ :: _ -> Code_error.raise "more blocks than metadata" []
  in
  loop [] metadata_entries sh_script.cram_to_output

let line_number =
  let open Re in
  seq [ set "123456789"; rep digit ]

let rewrite_paths build_path_prefix_map ~parent_script ~command_script s =
  match Build_path_prefix_map.decode_map build_path_prefix_map with
  | Error msg ->
    Code_error.raise "Cannot decode build prefix map"
      [ ("build_path_prefix_map", String build_path_prefix_map)
      ; ("msg", String msg)
      ]
  | Ok map ->
    let abs_path_re =
      let not_dir = Printf.sprintf " \n\r\t%c" Bin.path_sep in
      Re.(compile (seq [ char '/'; rep1 (diff any (set not_dir)) ]))
    in
    let error_msg =
      let open Re in
      let command_script = str (Path.to_absolute_filename command_script) in
      let parent_script = str (Path.to_absolute_filename parent_script) in
      let a =
        [ parent_script
        ; str ": "
        ; line_number
        ; str ": "
        ; command_script
        ; str ": "
        ]
        |> seq
      in
      let b = seq [ command_script; str ": line "; line_number; str ": " ] in
      [ a; b ] |> List.map ~f:(fun re -> seq [ bol; re ]) |> alt |> compile
    in
    Re.replace abs_path_re s ~f:(fun g ->
        Build_path_prefix_map.rewrite map (Re.Group.get g 0))
    |> Re.replace_string error_msg ~by:""

let sanitize ~parent_script cram_to_output :
    (block_result * metadata_entry * string) Cram_lexer.block list =
  List.map cram_to_output ~f:(fun t ->
      match t with
      | Cram_lexer.Comment t -> Cram_lexer.Comment t
      | Command
          (block_result, ({ build_path_prefix_map; exit_code = _ } as entry)) ->
        let output =
          Io.read_file ~binary:false block_result.output_file
          |> Ansi_color.strip
          |> rewrite_paths ~parent_script ~command_script:block_result.script
               build_path_prefix_map
        in
        Command (block_result, entry, output))

(* Compose user written cram stanzas to output *)
let compose_cram_output (cram_to_output : _ Cram_lexer.block list) =
  let buf = Buffer.create 256 in
  let add_line line =
    Buffer.add_string buf line;
    Buffer.add_char buf '\n'
  in
  let add_line_prefixed_with_two_space line =
    Buffer.add_string buf "  ";
    add_line line
  in
  List.iter cram_to_output ~f:(fun block ->
      match (block : _ Cram_lexer.block) with
      | Comment lines -> List.iter lines ~f:add_line
      | Command
          ( { command; output_file = _; script = _ }
          , { exit_code; build_path_prefix_map = _ }
          , output ) -> (
        List.iteri command ~f:(fun i line ->
            let line =
              sprintf "%c %s"
                (if i = 0 then
                  '$'
                else
                  '>')
                line
            in
            add_line_prefixed_with_two_space line);
        String.split_lines output
        |> List.iter ~f:add_line_prefixed_with_two_space;
        match exit_code with
        | 0 -> ()
        | n -> add_line_prefixed_with_two_space (sprintf "[%d]" n)));
  Buffer.contents buf

let create_sh_script cram_stanzas ~temp_dir : sh_script Fiber.t =
  let script = Path.relative temp_dir "main.sh" in
  let oc = Io.open_out ~binary:true script in
  let file name = Path.relative temp_dir name in
  let open Fiber.O in
  let sh_path path =
    let+ path = translate_path_for_sh path in
    quote_for_sh path
  in
  let metadata_file = file "cram.metadata" in
  let* metadata_file_sh_path = sh_path metadata_file in
  let i = ref 0 in
  let loop block =
    match (block : _ Cram_lexer.block) with
    | Comment _ as comment -> Fiber.return comment
    | Command lines ->
      incr i;
      let i = !i in
      let file ~ext = file (sprintf "%d%s" i ext) in
      (* Shell code written by the user might not be properly terminated. For
         instance the user might forgot to write [EOF] after a [cat <<EOF]. If
         we wrote this shell code directly in the main script, it would hide the
         rest of the script. So instead, we dump each user written shell phrase
         into a file and then source it in the main script. *)
      let user_shell_code_file = file ~ext:".sh" in
      let* user_shell_code_file_sh_path = sh_path user_shell_code_file in
      cat_eof oc lines ~dest:user_shell_code_file;
      (* Where we store the output of shell code written by the user *)
      let user_shell_code_output_file = file ~ext:".output" in
      let+ user_shell_code_output_file_sh_path =
        sh_path user_shell_code_output_file
      in
      fprln oc ". %s > %s 2>&1" user_shell_code_file_sh_path
        user_shell_code_output_file_sh_path;
      fprln oc {|printf "%%d\0%%s\0" $? $%s >> %s|} _BUILD_PATH_PREFIX_MAP
        metadata_file_sh_path;
      Cram_lexer.Command
        { command = lines
        ; output_file = user_shell_code_output_file
        ; script = user_shell_code_file
        }
  in
  let+ cram_to_output = Fiber.sequential_map ~f:loop cram_stanzas in
  close_out oc;
  let command_count = !i in
  let metadata_file = Option.some_if (command_count > 0) metadata_file in
  { script; cram_to_output; metadata_file; command_count }

let _display_with_bars s =
  List.iter (String.split_lines s) ~f:(Printf.eprintf "| %s\n")

let run ~env ~script lexbuf : string Fiber.t =
  let temp_dir =
    let suffix = Path.basename script in
    Temp.create Dir ~prefix:"dune.cram." ~suffix
  in
  let cram_stanzas = cram_stanzas lexbuf in
  let open Fiber.O in
  let* sh_script = create_sh_script cram_stanzas ~temp_dir in
  let cwd = Path.parent_exn script in
  let env =
    let env = Env.add env ~var:"LC_ALL" ~value:"C" in
    let temp_dir = Path.relative temp_dir "tmp" in
    let env = extend_build_path_prefix_map ~env ~cwd ~temp_dir in
    Path.mkdir_p temp_dir;
    Env.add env ~var:Env.Var.temp_dir
      ~value:(Path.to_absolute_filename temp_dir)
  in
  let open Fiber.O in
  let+ () =
    let sh =
      let path = Env.path Env.initial in
      Option.value_exn (Bin.which ~path "sh")
    in
    Process.run ~dir:cwd ~env Strict sh [ Path.to_string sh_script.script ]
  in
  let raw = read_and_attach_exit_codes sh_script in
  let sanitized = sanitize ~parent_script:sh_script.script raw in
  compose_cram_output sanitized

let run ~env ~script =
  run_expect_test script ~f:(fun lexbuf -> run ~env ~script lexbuf)

let () = Fdecl.set Action_exec.cram_run run

let linkme = ()
