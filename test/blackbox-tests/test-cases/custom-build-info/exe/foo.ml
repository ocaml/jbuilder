let () = Printf.printf "version: %s\n"
  (match Build_info.V1.version () with
    | None -> "n/a"
    | Some v -> Build_info.V1.Version.to_string v)

let () = Printf.printf "custom: %s\n"
  (match Build_info.V2.custom () with
    | None -> "n/a"
    | Some v -> Build_info.V2.Custom.to_string v)
