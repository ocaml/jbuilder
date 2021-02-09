let is_root t = Filename.dirname t = t

type mkdir_p =
  | Already_exists
  | Created

let rec mkdir_p ?(perms = 0o777) t_s =
  if is_root t_s then
    Already_exists
  else
    try
      Unix.mkdir t_s perms;
      Created
    with
    | Unix.Unix_error (EEXIST, _, _) -> Already_exists
    | Unix.Unix_error (ENOENT, _, _) as e ->
      let parent = Filename.dirname t_s in
      if is_root parent then
        raise e
      else
        let (_ : mkdir_p) = mkdir_p parent ~perms in
        Unix.mkdir t_s perms;
        Created

let resolve_link path =
  match Unix.readlink path with
  | exception Unix.Unix_error (EINVAL, _, _) -> Ok None
  | exception Unix.Unix_error (e, _, _) -> Error e
  | link ->
    Ok
      (Some
         (if Filename.is_relative link then
           Filename.concat (Filename.dirname path) link
         else
           link))

type follow_symlink_error =
  | Not_a_symlink
  | Max_depth_exceeded
  | Unix_error of Unix.error

let follow_symlink path =
  let rec loop n path =
    if n = 0 then
      Error Max_depth_exceeded
    else
      match resolve_link path with
      | Error e -> Error (Unix_error e)
      | Ok None -> Ok path
      | Ok (Some path) -> loop (n - 1) path
  in
  match resolve_link path with
  | Ok None -> Error Not_a_symlink
  | Ok (Some p) -> loop 20 p
  | Error e -> Error (Unix_error e)
