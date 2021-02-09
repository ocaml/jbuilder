open! Dune_engine
open Stdune
open Dune_lang.Decoder

type t = ..

type sub_system = t = ..

module type S = sig
  type t

  type sub_system += T of t

  val name : Sub_system_name.t

  val loc : t -> Loc.t

  val syntax : Dune_lang.Syntax.t

  val decode : t Dune_lang.Decoder.t

  val encode : t -> Dune_lang.Syntax.Version.t * Dune_lang.t list
end

(* This mutable table is safe under the assumption that subsystems are
   registered at the top level, which is currently true. *)
let all = Sub_system_name.Table.create ~default_value:None

(* For parsing config files in the workspace *)
let record_parser = ref return

module Register (M : S) : sig end = struct
  open M

  let () =
    match Sub_system_name.Table.get all name with
    | Some _ ->
      Code_error.raise "Sub_system_info.register: already registered"
        [ ("name", Dyn.Encoder.string (Sub_system_name.to_string name)) ]
    | None -> (
      Sub_system_name.Table.set all ~key:name ~data:(Some (module M : S));
      let p = !record_parser in
      let name_s = Sub_system_name.to_string name in
      record_parser :=
        fun acc ->
          field_o name_s decode >>= function
          | None -> p acc
          | Some x ->
            let acc = Sub_system_name.Map.set acc name (T x) in
            p acc)
end

let record_parser () = !record_parser Sub_system_name.Map.empty

let get name = Option.value_exn (Sub_system_name.Table.get all name)
