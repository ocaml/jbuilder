include Stdlib.Sys

let linux =
  match Io.String_path.read_file "/proc/sys/kernel/ostype" |> String.trim with
  | "Linux" -> true
  | _ -> false
  | exception _ -> false

let force_remove =
  if win32 then (
    fun fn ->
  try remove fn with
  | Sys_error _ ->
    (* Try to remove the "read-only" attribute, then retry. *)
    (try Unix.chmod fn 0o666 with
    | Unix.Unix_error _ -> ());
    remove fn
  ) else
    remove
