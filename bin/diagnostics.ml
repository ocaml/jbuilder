open! Stdune
open Import

let on_notification _ _ = Fiber.return ()

let format_diagnostic (err : Dune_rpc_private.Diagnostic.t) : User_message.t =
  let prefix =
    Option.map err.severity ~f:(fun sev ->
        let severity, prefix =
          match sev with
          | Dune_rpc_private.Diagnostic.Error ->
            (User_message.Style.Error, "Error:")
          | Warning -> (Warning, "Warning:")
        in
        Pp.tag severity (Pp.text prefix))
  in
  let directory =
    match err.directory with
    | None -> []
    | Some d ->
      [ Pp.tag User_message.Style.Loc (Pp.textf "(In directory %s)" d) ]
  in
  let formatted_loc =
    match err.loc with
    | None -> []
    | Some l -> [ Pp.map_tags ~f:(fun _ -> User_message.Style.Loc) (Loc.pp l) ]
  in
  User_message.make ?prefix
    (directory @ formatted_loc
    @ [ Pp.map_tags ~f:(fun _ -> User_message.Style.Details) err.message ])

let exec common =
  let open Fiber.O in
  let where = Rpc.wait_for_server common in
  let+ errors =
    Dune_rpc_impl.Run.client where
      (Dune_rpc_private.Initialize.Request.create
         ~id:(Dune_rpc_private.Id.make (Sexp.Atom "diagnostics_cmd")))
      ~on_notification
      ~f:(fun cli ->
        Dune_rpc_impl.Client.request cli
          Dune_rpc_private.Public.Request.diagnostics ())
  in
  match errors with
  | Ok errors ->
    List.iter errors ~f:(fun err -> User_message.print (format_diagnostic err))
  | Error e -> Rpc.raise_rpc_error e

let info =
  let doc = "fetch and return errors from the current build" in
  Term.info "diagnostics" ~doc

let term =
  let+ (common : Common.t) = Common.term in
  Rpc.client_term common exec

let command = (term, info)
