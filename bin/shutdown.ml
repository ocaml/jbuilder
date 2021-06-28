open! Stdune
open Import

let send_shutdown cli =
  Dune_rpc_impl.Client.notification cli
    Dune_rpc_private.Public.Notification.shutdown ()

let on_notification _ _ = Fiber.return ()

let exec common =
  let where = Rpc.wait_for_server common in
  Dune_rpc_impl.Run.client where
    (Dune_rpc_private.Initialize.Request.create
       ~id:(Dune_rpc_private.Id.make (Sexp.Atom "shutdown_cmd")))
    ~on_notification ~f:send_shutdown

let info =
  let doc = "cancel and shutdown any builds in the current workspace" in
  Term.info "shutdown" ~doc

let term =
  let+ (common : Common.t) = Common.term in
  Rpc.client_term common exec

let command = (term, info)
