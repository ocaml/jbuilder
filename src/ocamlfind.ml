open! Stdune
open Import

type t =
  { config : Findlib.Config.t
  ; ocamlpath : Path.t list
  ; which : string -> Path.t option
  ; toolchain : string option
  }

let ocamlpath_sep =
  if Sys.cygwin then
    (* because that's what ocamlfind expects *)
    ';'
  else
    Bin.path_sep

let path_var = Bin.parse_path ~sep:ocamlpath_sep

let ocamlpath env =
  match Env.get env "OCAMLPATH" with
  | None -> []
  | Some s -> path_var s

let set_toolchain t ~toolchain =
  match t.toolchain with
  | None ->
    { t with
      config = Findlib.Config.toolchain t.config ~toolchain
    ; toolchain = Some toolchain
    }
  | Some old_toolchain ->
    let open Sexp.Encoder in
    Exn.code_error "Ocamlfind.set_toolchain: cannot set toolchain twice"
      [ "old_toolchain", string old_toolchain
      ; "toolchain", string toolchain
      ]

let conf_path t =
  match Findlib.Config.get t.config "path" with
  | None -> t.ocamlpath
  | Some p -> t.ocamlpath @ path_var p

let tool t ~prog =
  let open Option.O in
  Findlib.Config.get t.config prog >>= fun s ->
  match Filename.analyze_program_name s with
  | In_path | Relative_to_current_dir -> t.which s
  | Absolute -> Some (Path.of_filename_relative_to_initial_cwd s)

let ocamlfind_config_path ~env ~which =
  (match Env.get env "OCAMLFIND_CONF" with
   | Some v -> Some (Fiber.return v)
   | None ->
     Option.map (which "ocamlfind") ~f:(fun ocamlfind ->
       Process.run_capture_line ~env Strict ocamlfind ["printconf"; "conf"]))
  |> Option.map ~f:(Fiber.map ~f:Path.of_filename_relative_to_initial_cwd)

let discover_from_env ~env ~ocamlpath ~which =
  Option.map (ocamlfind_config_path ~env ~which) ~f:(fun config ->
    let open Fiber.O in
    config >>| fun config ->
    { config = Findlib.Config.load config
    ; ocamlpath
    ; which
    ; toolchain = None
    })

let extra_env t = Findlib.Config.env t.config
