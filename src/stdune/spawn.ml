external sys_exit : int -> 'a = "caml_sys_exit"

let rec file_descr_not_standard fd =
  assert (not Sys.win32);
  if (Obj.magic (fd : Unix.file_descr) : int) >= 3 then
    fd
  else
    file_descr_not_standard (Unix.dup fd)

let safe_close fd = try Unix.close fd with Unix.Unix_error _ -> ()

let perform_redirections stdin stdout stderr =
  let stdin = file_descr_not_standard stdin in
  let stdout = file_descr_not_standard stdout in
  let stderr = file_descr_not_standard stderr in
  Unix.dup2 stdin Unix.stdin;
  Unix.dup2 stdout Unix.stdout;
  Unix.dup2 stderr Unix.stderr;
  safe_close stdin;
  safe_close stdout;
  safe_close stderr

let exec ?env prog argv =
  ignore (Unix.sigprocmask SIG_SETMASK [] : int list);
  match env with
  | None -> Unix.execv prog argv
  | Some env -> Unix.execve prog argv env

let create_process ?env prog argv stdin stdout stderr =
  match env with
  | None -> Unix.create_process prog argv stdin stdout stderr
  | Some env -> Unix.create_process_env prog argv env stdin stdout stderr

(** Note that this function's behavior differs between windows and unix.

    - [Unix.create_process{,_env} prog] looks up prog in PATH
    - [Unix.execv{_,e} does not look up prog in PATH] *)
let spawn ?env ~prog ~argv ?(stdin = Unix.stdin) ?(stdout = Unix.stdout)
    ?(stderr = Unix.stderr) () =
  let argv = Array.of_list argv in
  let env = Option.map ~f:Env.to_unix env in
  Pid.of_int
    ( if Sys.win32 then
      create_process prog argv stdin stdout stderr
    else
      match Unix.fork () with
      | 0 -> (
        try
          perform_redirections stdin stdout stderr;
          exec ?env prog argv
        with _ -> sys_exit 127 )
      | pid -> pid )

let exec ?env ~prog ~argv () =
  let argv = Array.of_list argv in
  let env = Option.map ~f:Env.to_unix env in
  if Sys.win32 then
    let pid =
      create_process ?env prog argv Unix.stdin Unix.stdout Unix.stderr
    in
    match snd (Unix.waitpid [] pid) with
    | WEXITED 0 -> exit 0
    | WEXITED n -> exit n
    | WSIGNALED _ -> exit 255
    | WSTOPPED _ -> assert false
  else
    exec ?env prog argv
