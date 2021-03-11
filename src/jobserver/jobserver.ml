open Stdune

type t =
  | Fd of
      { read : Unix.file_descr
      ; write : Unix.file_descr
      }
  | Semaphore of string

external fd_of_int : int -> Unix.file_descr = "%identity"

let of_env_var e =
  let lex = Lexing.from_string e in
  let open Option.O in
  let* s = Lexer.lex lex in
  if Sys.unix then
    String.lsplit2 ~on:',' s
    |> Option.map ~f:(fun (read, write) ->
           let read = fd_of_int (Int.of_string_exn read) in
           let write = fd_of_int (Int.of_string_exn write) in
           Fd { read; write })
  else
    Some (Semaphore s)

let of_env env =
  let open Option.O in
  let* var = Env.get env "MAKEFLAGS" in
  of_env_var var

module Job = struct
  type nonrec t = t * Bytes.t (* always length 1 *)

  let release (t, job) =
    match t with
    | Semaphore _ -> ()
    | Fd { read = _; write } ->
      let (_ : int) = Unix.write write job 0 1 in
      ()
end

let wait_next t =
  ( t
  , match t with
    | Semaphore _ -> Bytes.make 1 '1'
    | Fd { read; write = _ } ->
      let b = Bytes.make 1 '\000' in
      let (_ : int) = Unix.read read b 0 1 in
      b )
