module Annotations = struct
  module Entry = struct
    type 'a t = 'a Univ_map.Key.t

    module type S = sig
      type payload

      val entry : payload t
    end

    module Make (Payload : sig
      type t

      val name : string

      val to_dyn : t -> Dyn.t
    end) =
    struct
      type payload = Payload.t

      let entry = Univ_map.Key.create ~name:Payload.name Payload.to_dyn
    end
  end

  type t = Univ_map.t

  let none = Univ_map.empty

  let singleton = Univ_map.singleton

  let annotate = Univ_map.set

  let lookup = Univ_map.find

  let is_empty = Univ_map.is_empty

  module Has_embedded_location = Entry.Make (struct
    type t = unit

    let name = "embedded_location"

    let to_dyn = Unit.to_dyn
  end)
end

exception E of User_message.t * Annotations.t

let prefix =
  Pp.seq (Pp.tag User_message.Style.Error (Pp.verbatim "Error")) (Pp.char ':')

let make ?loc ?hints paragraphs =
  User_message.make ?loc ?hints paragraphs ~prefix

let raise ?loc ?hints ?(annots = Annotations.none) paragraphs =
  raise (E (make ?loc ?hints paragraphs, annots))

let is_loc_none loc =
  match loc with
  | None -> true
  | Some loc -> loc = Loc0.none

let has_embed_location annots =
  Option.is_some
    (Annotations.lookup annots Annotations.Has_embedded_location.entry)

let has_location (msg : User_message.t) annots =
  (not (is_loc_none msg.loc)) || has_embed_location annots

let () =
  Printexc.register_printer (function
    | E (t, annots) ->
      if Annotations.is_empty annots then
        Some (Format.asprintf "%a@?" Pp.to_fmt (User_message.pp t))
      else
        let open Pp.O in
        let pp =
          User_message.pp t ++ Pp.vbox (Dyn.pp (Univ_map.to_dyn annots))
        in
        Some (Format.asprintf "%a" Pp.to_fmt pp)
    | _ -> None)
