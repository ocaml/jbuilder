(* Result of a successful match. *)
type t =
  { s : string
  ; marks : Automata.mark_infos
  ; pmarks : Pmark.Set.t
  ; gpos : int array
  ; gcount : int
  }

let offset t i =
  if 2 * i + 1 >= Array.length t.marks then raise Not_found;
  let m1 = t.marks.(2 * i) in
  if m1 = -1 then raise Not_found;
  let p1 = t.gpos.(m1) - 1 in
  let p2 = t.gpos.(t.marks.(2 * i + 1)) - 1 in
  (p1, p2)

let get t i =
  let (p1, p2) = offset t i in
  String.sub t.s p1 (p2 - p1)

let start subs i = fst (offset subs i)

let stop subs i = snd (offset subs i)

let test t i =
  if 2 * i >= Array.length t.marks then
    false
  else
    let idx = t.marks.(2 * i) in
    idx <> -1

let dummy_offset = (-1, -1)

let all_offset t =
  let res = Array.make t.gcount dummy_offset in
  for i = 0 to Array.length t.marks / 2 - 1 do
    let m1 = t.marks.(2 * i) in
    if m1 <> -1 then begin
      let p1 = t.gpos.(m1) in
      let p2 = t.gpos.(t.marks.(2 * i + 1)) in
      res.(i) <- (p1 - 1, p2 - 1)
    end
  done;
  res

let dummy_string = ""

let all t =
  let res = Array.make t.gcount dummy_string in
  for i = 0 to Array.length t.marks / 2 - 1 do
    let m1 = t.marks.(2 * i) in
    if m1 <> -1 then begin
      let p1 = t.gpos.(m1) in
      let p2 = t.gpos.(t.marks.(2 * i + 1)) in
      res.(i) <- String.sub t.s (p1 - 1) (p2 - p1)
    end
  done;
  res

let pp fmt t =
  let matches =
    let offsets = all_offset t in
    let strs = all t in
    Array.to_list (
      Array.init (Array.length strs) (fun i -> strs.(i), offsets.(i))
    ) in
  let open Fmt in
  let pp_match fmt (str, (start, stop)) =
    fprintf fmt "@[(%s (%d %d))@]" str start stop in
  sexp fmt "Group" (list pp_match) matches

let nb_groups t = t.gcount
