open! Stdune
open! Fiber.O
module Dune_rpc = Dune_rpc_private
open Dune_rpc
open Dune_rpc_server
module Scheduler = Test_scheduler

let () = Printexc.record_backtrace false

let print pp = Format.printf "%a@." Pp.to_fmt pp

let print_dyn dyn = print (Dyn.pp dyn)

module Chan = struct
  module Mvar = Fiber.Mvar

  type t =
    { (* Read end. Populated by writing by [snd] *)
      in_ : Sexp.t Fiber.Stream.In.t * Sexp.t Fiber.Stream.Out.t
    ; (* Write end. Can be read via [fst] *)
      out : Sexp.t Fiber.Stream.In.t * Sexp.t Fiber.Stream.Out.t
    }

  let create () = { in_ = Fiber.Stream.pipe (); out = Fiber.Stream.pipe () }

  let write t s =
    match s with
    | None -> Fiber.Stream.Out.write (snd t.out) None
    | Some s ->
      Fiber.sequential_iter s ~f:(fun s ->
          Fiber.Stream.Out.write (snd t.out) (Some s))

  let read t = Fiber.Stream.In.read (fst t.in_)

  let connect c1 c2 =
    Fiber.fork_and_join_unit
      (fun () -> Fiber.Stream.connect (fst c1.out) (snd c2.in_))
      (fun () -> Fiber.Stream.connect (fst c2.out) (snd c1.in_))
end

module Drpc = struct
  module Client =
    Dune_rpc.Client.Make
      (struct
        include Fiber

        let parallel_iter t ~f =
          let stream = Fiber.Stream.In.create t in
          Fiber.Stream.In.parallel_iter stream ~f
      end)
      (Chan)

  module Server = Dune_rpc_server.Make (Chan)
end

open Drpc

let on_init _ _ = Fiber.return ()

let setup_client_server () =
  let client_chan = Chan.create () in
  let server_chan = Chan.create () in
  let sessions = Fiber.Stream.In.of_list [ server_chan ] in
  let connect () = Chan.connect client_chan server_chan in
  (client_chan, sessions, connect)

let test ~client ~handler ~init () =
  let run =
    let client_chan, sessions, connect = setup_client_server () in
    let client () =
      Drpc.Client.connect client_chan init ~f:(fun c ->
          let* () = client c in
          Chan.write client_chan None)
    in
    let server () =
      let+ () =
        Drpc.Server.serve sessions None (Dune_rpc_server.make handler)
      in
      printfn "server: finished."
    in
    Fiber.parallel_iter [ connect; client; server ] ~f:(fun f -> f ())
  in
  Scheduler.run (Scheduler.create ()) run

let init ?(id = Id.make (Csexp.Atom "test-client")) ?(version = (1, 1)) () =
  { Initialize.Request.version; id }

let%expect_test "initialize scheduler with rpc" =
  let handler = Handler.create ~on_init ~version:(2, 0) () in
  let init = init () in
  test ~init
    ~client:(fun _ ->
      printfn "client: connected. now terminating";
      Fiber.return ())
    ~handler ();
  [%expect {|
    client: connected. now terminating
    server: finished. |}]

let%expect_test "invalid client version" =
  let handler = Handler.create ~on_init ~version:(2, 0) () in
  let init = init ~version:(2, 5) () in
  test ~init ~client:(fun _ -> assert false) ~handler ();
  [%expect.unreachable]
  [@@expect.uncaught_exn
    {|
  ( "Response.E\
   \n  { payload = Some [ [ \"supported versions until\"; [ \"2\"; \"0\" ] ] ]\
   \n  ; message = \"Unsupported version\"\
   \n  ; kind = Version_error\
   \n  }")
  Trailing output
  ---------------
  server: finished. |}]

let%expect_test "call private method" =
  let decl = Decl.request ~method_:"double" Conv.int Conv.int in
  let handler =
    let rpc = Handler.create ~on_init ~version:(1, 1) () in
    let () =
      let cb =
        Handler.callback Handler.private_ (fun x ->
            if x = 0 then
              raise
                (Response.Error.E
                   (Response.Error.create ~kind:Invalid_request
                      ~message:"0 not allowed" ()))
            else
              Fiber.return (x + x))
      in
      Handler.request rpc cb decl
    in
    rpc
  in
  let client client =
    printfn "client: sending request";
    let* resp = Client.request client decl 5 in
    (match resp with
    | Error _ -> assert false
    | Ok s -> printfn "client: result %d" s);
    printfn "client: sending invalid request";
    let* resp = Client.request client decl 0 in
    (match resp with
    | Error e -> printfn "client: error %s" e.message
    | Ok _ -> assert false);
    Fiber.return ()
  in
  let init =
    { Initialize.Request.version = (1, 1); id = Id.make (Atom "test-client") }
  in
  test ~init ~client ~handler ();
  [%expect
    {|
    client: sending request
    client: result 10
    client: sending invalid request
    client: error 0 not allowed
    server: finished. |}]

let%expect_test "versioning public methods" =
  let decl = Decl.request ~method_:"double" Conv.int Conv.int in
  let handler =
    let rpc = Handler.create ~on_init ~version:(2, 0) () in
    let () =
      let cb =
        Handler.callback
          (Handler.public ~since:(1, 5) ~until:(2, 0) ())
          (fun x -> Fiber.return (x + x))
      in
      Handler.request rpc cb decl
    in
    rpc
  in
  let client client =
    printfn "client: sending request";
    let* resp = Client.request client decl 0 in
    (match resp with
    | Error e -> printfn "client: error %s" e.message
    | Ok _ -> assert false);
    Fiber.return ()
  in
  let init =
    { Initialize.Request.version = (1, 1); id = Id.make (Atom "test-client") }
  in
  test ~init ~client ~handler ();
  [%expect
    {|
    client: sending request
    client: error no method matching this client version
    server: finished. |}]

let%expect_test "versioning public methods" =
  let decl =
    let input =
      let open Conv in
      record
        (both
           (field "x" (required int))
           (field "y" (optional (version int ~since:(2, 0)))))
    in
    Decl.request ~method_:"add" input Conv.int
  in
  let handler =
    let rpc = Handler.create ~on_init ~version:(2, 0) () in
    let () =
      let cb =
        Handler.callback
          (Handler.public ~since:(1, 5) ())
          (fun (x, y) -> Fiber.return (x + Option.value y ~default:x))
      in
      Handler.request rpc cb decl
    in
    rpc
  in
  let client client =
    printfn "client: sending request";
    let* resp = Client.request client decl (10, None) in
    (match resp with
    | Error _ -> assert false
    | Ok x -> printfn "client: %d" x);
    let+ resp = Client.request client decl (10, Some 20) in
    match resp with
    | Error e -> printfn "client: error %s" e.message
    | Ok _ -> ()
  in
  let init =
    { Initialize.Request.version = (1, 9); id = Id.make (Atom "test-client") }
  in
  test ~init ~client ~handler ();
  [%expect
    {|
    client: sending request
    client: 20
    client: error invalid version
    server: finished. |}]

let%test_module "subscirption" =
  (module struct
    let sub_decl =
      { Dune_rpc_private.Sub.init = Conv.int
      ; diff = Conv.string
      ; name = "pulse"
      }

    let version = (3, 0)

    let init = init ~version ()

    let rpc () = Handler.create ~on_init ~version ()

    let server f =
      let rpc = rpc () in
      let () =
        let on_subscribe _ = Fiber.return 42 in
        let subscription _ _ sub = f sub in
        Handler.subscription rpc sub_decl ~on_subscribe ~subscription
      in
      rpc

    let run_client client on_next =
      printfn "client: making subscription";
      let+ () =
        Client.subscribe client sub_decl
          ~on_init:(fun init sub ->
            printfn "client: initial value %d" init;
            Fiber.return sub)
          ~on_next
      in
      printfn "subscription terminated"

    let%expect_test "subscription - client cancel" =
      let client client =
        let count = ref 0 in
        run_client client (fun v active_sub ->
            if !count = 2 then
              Client.Subscription.cancel active_sub
            else (
              printfn "client: update %s received" v;
              incr count;
              Fiber.return ()
            ))
      in
      let handler =
        server (fun sub ->
            let* () = Subscription.update sub "first" in
            let* () = Subscription.update sub "second" in
            let* () = Subscription.update sub "third" in
            Subscription.finished sub)
      in
      test ~init ~client ~handler ();
      [%expect
        {|
    client: making subscription
    client: initial value 42
    client: update first received
    client: update second received
    subscription terminated
    server: finished. |}]

    let%expect_test "subscription - server cancel" =
      let client client =
        run_client client (fun v _ ->
            printfn "client: update %s received" v;
            Fiber.return ())
      in
      let handler =
        server (fun sub ->
            let* () = Subscription.update sub "first" in
            let* () = Subscription.update sub "second" in
            Subscription.finish sub)
      in
      test ~init ~client ~handler ();
      [%expect
        {|
    client: making subscription
    client: initial value 42
    client: update first received
    client: update second received
    subscription terminated
    server: finished. |}]
  end)
