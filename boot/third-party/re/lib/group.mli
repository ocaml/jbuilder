(* Result of a successful match. *)
type t =
  { s : string
  (* Input string. Matched strings are substrings of s *)

  ; marks : Automata.mark_infos
  (* Mapping from group indices to positions in gpos. group i has positions 2*i
     - 1, 2*i + 1 in gpos. If the group wasn't matched, then its corresponding
     values in marks will be -1,-1 *)

  ; pmarks : Pmark.Set.t
  (* Marks positions. i.e. those marks created with Re.marks *)

  ; gpos : int array
  (* Group positions. Adjacent elements are (start, stop) of group match.
     indexed by the values in marks. So group i in an re would be the substring:

     start = t.gpos.(marks.(2*i)) - 1
     stop = t.gpos.(marks.(2*i + 1)) - 1 *)

  ; gcount : int
  (* Number of groups the regular expression contains. Matched or not *)
  }

(** Information about groups in a match. *)

val get : t -> int -> string
(** Raise [Not_found] if the group did not match *)

val offset : t -> int -> int * int
(** Raise [Not_found] if the group did not match *)

val start : t -> int -> int
(** Return the start of the match. Raise [Not_found] if the group did not match. *)

val stop : t -> int -> int
(** Return the end of the match. Raise [Not_found] if the group did not match. *)

val all : t -> string array
(** Return the empty string for each group which did not match *)

val all_offset : t -> (int * int) array
(** Return [(-1,-1)] for each group which did not match *)

val test : t -> int -> bool
(** Test whether a group matched *)

val nb_groups : t -> int
(** Returns the total number of groups defined - matched or not.
    This function is experimental. *)

val pp : Format.formatter -> t -> unit
