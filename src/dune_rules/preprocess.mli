open! Dune_engine
open Stdune

module Pps : sig
  type 'a t =
    { loc : Loc.t
    ; pps : 'a list
    ; flags : String_with_vars.t list
    ; staged : bool
    }

  val compare_no_locs : ('a -> 'a -> Ordering.t) -> 'a t -> 'a t -> Ordering.t
end

type 'a t =
  | No_preprocessing
  | Action of Loc.t * Action_dune_lang.t
  | Pps of 'a Pps.t
  | Camlp5 of 'a Pps.t
  | Future_syntax of Loc.t

val map : 'a t -> f:('a -> 'b) -> 'b t

module Without_instrumentation : sig
  type t = Loc.t * Lib_name.t

  val compare_no_locs : t -> t -> Ordering.t
end

module With_instrumentation : sig
  type t =
    | Ordinary of Without_instrumentation.t
    | Instrumentation_backend of (Loc.t * Lib_name.t) * Dep_conf.t list
end

val decode : Without_instrumentation.t t Dune_lang.Decoder.t

module Without_future_syntax : sig
  type 'a t =
    | No_preprocessing
    | Action of Loc.t * Action_dune_lang.t
    | Pps of 'a Pps.t
    | Camlp5 of 'a Pps.t
end

val loc : _ t -> Loc.t option

module Pp_flag_consumer : sig
  type t =
    | Compiler
    | Merlin
end

val remove_future_syntax :
     'a t
  -> for_:Pp_flag_consumer.t
  -> Ocaml_version.t
  -> 'a Without_future_syntax.t

module Per_module : sig
  type 'a preprocess = 'a t

  type 'a t = 'a preprocess Module_name.Per_item.t

  val decode : Without_instrumentation.t t Dune_lang.Decoder.t

  val no_preprocessing : unit -> 'a t

  val default : unit -> 'a t

  (** [find module_name] find the preprocessing specification for a given module *)
  val find : Module_name.t -> 'a t -> 'a preprocess

  val pps : Without_instrumentation.t t -> Without_instrumentation.t list

  (** Preprocessing specification used by all modules or [No_preprocessing] *)
  val single_preprocess : 'a t -> 'a preprocess

  val add_instrumentation :
       With_instrumentation.t t
    -> loc:Loc.t
    -> flags:String_with_vars.t list
    -> deps:Dep_conf.t list
    -> Loc.t * Lib_name.t
    -> With_instrumentation.t t

  val without_instrumentation :
    With_instrumentation.t t -> Without_instrumentation.t t

  val with_instrumentation :
       With_instrumentation.t t
    -> instrumentation_backend:
         (Loc.t * Lib_name.t -> Without_instrumentation.t option)
    -> Without_instrumentation.t t

  val instrumentation_deps :
       With_instrumentation.t t
    -> instrumentation_backend:
         (Loc.t * Lib_name.t -> Without_instrumentation.t option)
    -> Dep_conf.t list
end
with type 'a preprocess := 'a t
