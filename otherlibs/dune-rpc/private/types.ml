open Import

module Id = struct
  module T = struct
    type t = Sexp.t

    let equal = Poly.equal

    let compare = Poly.compare

    let to_dyn s = Sexp.to_dyn s
  end

  include T

  let make s = s

  let to_sexp t = t

  let sexp = Conv.sexp

  let gen f = Conv.field "id" (f sexp)

  let required_field = gen Conv.required

  let optional_field = gen Conv.optional

  let hash = Poly.hash

  module C = Comparable.Make (T)
  module Set = C.Set
  module Map = C.Map
end

module Version = struct
  type t = int * int

  let latest = (3, 0)

  let sexp : t Conv.value =
    let open Conv in
    pair int int
end

module Call = struct
  type t =
    { method_ : string
    ; params : Sexp.t
    }

  let to_dyn { method_; params } =
    let open Dyn.Encoder in
    record [ ("method_", String method_); ("params", Sexp.to_dyn params) ]

  let create ?(params = Sexp.List []) ~method_ () = { method_; params }

  let fields =
    let open Conv in
    let to_ (method_, params) = { method_; params } in
    let from { method_; params } = (method_, params) in
    let method_ = field "method" (required string) in
    let params = field "params" (required sexp) in
    iso (both method_ params) to_ from
end

module Request = struct
  type t = Id.t * Call.t
end

module Response = struct
  module Error = struct
    type kind =
      | Invalid_request
      | Code_error
      | Version_error

    let dyn_of_kind =
      let open Dyn.Encoder in
      function
      | Invalid_request -> constr "Invalid_request" []
      | Code_error -> constr "Code_error" []
      | Version_error -> constr "Version_error" []

    type t =
      { payload : Sexp.t option
      ; message : string
      ; kind : kind
      }

    let payload t = t.payload

    let kind t = t.kind

    let message t = t.message

    exception E of t

    let create ?payload ~kind ~message () = { payload; message; kind }

    let of_conv (error : Conv.error) =
      let make_payload = function
        | [] -> None
        | payload -> Some (Sexp.record payload)
      in
      match error with
      | Parse_error { payload; message } ->
        { message; payload = make_payload payload; kind = Invalid_request }
      | Version_error { payload; message; since = _; until = _ } ->
        { message; payload = make_payload payload; kind = Version_error }

    let sexp =
      let open Conv in
      let id = field "payload" (optional sexp) in
      let message = field "message" (required string) in
      let kind =
        field "kind"
          (required
             (enum
                [ ("Invalid_request", Invalid_request)
                ; ("Code_error", Code_error)
                ; ("Version_error", Version_error)
                ]))
      in
      record
        (iso (three id message kind)
           (fun (payload, message, kind) -> { payload; message; kind })
           (fun { payload; message; kind } -> (payload, message, kind)))

    let to_dyn { payload; message; kind } =
      let open Dyn.Encoder in
      record
        [ ("payload", option Sexp.to_dyn payload)
        ; ("message", string message)
        ; ("kind", dyn_of_kind kind)
        ]

    let () =
      Printexc.register_printer (function
        | E e ->
          Some (Dyn.to_string (Dyn.Encoder.constr "Response.E" [ to_dyn e ]))
        | _ -> None)
  end

  type t = (Sexp.t, Error.t) result

  let result =
    let open Conv in
    let ok = constr "ok" sexp (fun x -> Ok x) in
    let error = constr "error" Error.sexp (fun x -> Error x) in
    sum [ econstr ok; econstr error ] (function
      | Ok s -> case s ok
      | Error e -> case e error)

  let fields =
    let open Conv in
    let id = Id.required_field in
    let payload = field "result" (required result) in
    both id payload

  let to_dyn = Result.to_dyn Sexp.to_dyn Error.to_dyn
end

module Initialize = struct
  module Request = struct
    type t =
      { version : int * int
      ; id : Id.t
      }

    let version t = t.version

    let id t = t.id

    let create ~id = { version = Version.latest; id }

    let sexp =
      let open Conv in
      let version = field "version" (required Version.sexp) in
      let id = Id.required_field in
      let to_ (version, id) = { version; id } in
      let from { version; id } = (version, id) in
      record (iso (both version id) to_ from)

    let of_call { Call.method_; params } ~version =
      match method_ with
      | "initialize" ->
        Conv.of_sexp sexp ~version params
        |> Result.map_error ~f:Response.Error.of_conv
      | _ ->
        let message = "initialize request expected" in
        Error (Response.Error.create ~message ~kind:Invalid_request ())

    let to_call t =
      let params = Conv.to_sexp sexp t in
      { Call.method_ = "initialize"; params }
  end

  module Response = struct
    type t = unit

    let sexp = Conv.unit

    let create () = ()

    let to_response () = Conv.to_sexp sexp ()
  end
end

module Persistent = struct
  module Out = struct
    type t =
      | Packet of Sexp.t
      | Close_connection

    let sexp =
      let open Conv in
      let packet = constr "packet" sexp (fun p -> Packet p) in
      let close_connection =
        constr "close_connection" unit (fun () -> Close_connection)
      in
      sum [ econstr packet; econstr close_connection ] (function
        | Packet p -> case p packet
        | Close_connection -> case () close_connection)
  end

  module In = struct
    type t =
      | New_connection
      | Packet of Csexp.t
      | Close_connection

    let to_dyn =
      let open Dyn.Encoder in
      function
      | New_connection -> constr "New_connection" []
      | Close_connection -> constr "Close_connection" []
      | Packet sexp -> constr "Packet" [ Sexp.to_dyn sexp ]

    let sexp =
      let open Conv in
      let new_connection =
        constr "new_connection" unit (fun () -> New_connection)
      in
      let packet = constr "packet" sexp (fun p -> Packet p) in
      let close_connection =
        constr "close_connection" unit (fun () -> Close_connection)
      in
      sum [ econstr new_connection; econstr packet; econstr close_connection ]
        (function
        | New_connection -> case () new_connection
        | Packet p -> case p packet
        | Close_connection -> case () close_connection)
  end
end

module Packet = struct
  module Query = struct
    type t =
      | Request of Request.t
      | Notification of Call.t

    let sexp =
      let open Conv in
      let to_ (id, payload) =
        match id with
        | None -> Notification payload
        | Some id -> Request (id, payload)
      in
      let from = function
        | Request (id, payload) -> (Some id, payload)
        | Notification payload -> (None, payload)
      in
      record
        (iso
           (let id = Id.optional_field in
            both id Call.fields)
           to_ from)
  end

  module Reply = struct
    type t =
      | Response of (Id.t * Response.t)
      | Notification of Call.t

    let sexp =
      let f = function
        | Either.Left (id, resp) -> Response (id, resp)
        | Right y -> Notification y
      in
      let t = function
        | Response (id, resp) -> Either.Left (id, resp)
        | Notification r -> Right r
      in
      let open Conv in
      record (iso (either Response.fields Call.fields) f t)
  end
end

module Decl = struct
  type ('req, 'resp) request =
    { method_ : string
    ; req : 'req Conv.value
    ; resp : 'resp Conv.value
    }

  type 'req notification =
    { method_ : string
    ; req : 'req Conv.value
    }

  let notification ~method_ req = { method_; req }

  let request ~method_ req resp = { method_; req; resp }
end
