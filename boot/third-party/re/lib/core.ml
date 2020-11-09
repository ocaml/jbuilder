(*
   RE - A regular expression library

   Copyright (C) 2001 Jerome Vouillon
   email: Jerome.Vouillon@pps.jussieu.fr

   This library is free software; you can redistribute it and/or
   modify it under the terms of the GNU Lesser General Public
   License as published by the Free Software Foundation, with
   linking exception; either version 2.1 of the License, or (at
   your option) any later version.

   This library is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   Lesser General Public License for more details.

   You should have received a copy of the GNU Lesser General Public
   License along with this library; if not, write to the Free Software
   Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA
*)

let rec iter n f v = if n = 0 then v else iter (n - 1) f (f v)

(****)

let unknown = -2
let break = -3

type match_info =
  | Match of Group.t
  | Failed
  | Running

type state =
  { idx : int;
    (* Index of the current position in the position table.
       Not yet computed transitions point to a dummy state where
       [idx] is set to [unknown];
       If [idx] is set to [break] for states that either always
       succeed or always fail. *)
    real_idx : int;
    (* The real index, in case [idx] is set to [break] *)
    next : state array;
    (* Transition table, indexed by color *)
    mutable final :
      (Category.t *
       (Automata.idx * Automata.status)) list;
    (* Mapping from the category of the next character to
       - the index where the next position should be saved
       - possibly, the list of marks (and the corresponding indices)
         corresponding to the best match *)
    desc : Automata.State.t
    (* Description of this state of the automata *) }

(* Automata (compiled regular expression) *)
type re =
  { initial : Automata.expr;
    (* The whole regular expression *)
    mutable initial_states : (Category.t * state) list;
    (* Initial states, indexed by initial category *)
    colors : Bytes.t;
    (* Color table *)
    color_repr : Bytes.t;
    (* Table from colors to one character of this color *)
    ncolor : int;
    (* Number of colors. *)
    lnl : int;
    (* Color of the last newline. -1 if unnecessary *)
    tbl : Automata.working_area;
    (* Temporary table used to compute the first available index
       when computing a new state *)
    states : state Automata.State.Table.t;
    (* States of the deterministic automata *)
    group_count : int
    (* Number of groups in the regular expression *) }

let pp_re ch re = Automata.pp ch re.initial

let print_re = pp_re

(* Information used during matching *)
type info =
  { re : re;
    (* The automata *)
    colors : Bytes.t;
    (* Color table ([x.colors = x.re.colors])
       Shortcut used for performance reasons *)
    mutable positions : int array;
    (* Array of mark positions
       The mark are off by one for performance reasons *)
    pos : int;
    (* Position where the match is started *)
    last : int
    (* Position where the match should stop *) }


(****)

let category re ~color =
  if color = -1 then
    Category.inexistant
    (* Special category for the last newline *)
  else if color = re.lnl then
    Category.(lastnewline ++ newline ++ not_letter)
  else
    Category.from_char (Bytes.get re.color_repr color)

(****)

let dummy_next = [||]

let unknown_state =
  { idx = unknown; real_idx = 0;
    next = dummy_next; final = [];
    desc = Automata.State.dummy }

let mk_state ncol desc =
  let break_state =
    match Automata.status desc with
    | Automata.Running -> false
    | Automata.Failed
    | Automata.Match _ -> true
  in
  { idx = if break_state then break else desc.Automata.State.idx;
    real_idx = desc.Automata.State.idx;
    next = if break_state then dummy_next else Array.make ncol unknown_state;
    final = [];
    desc }

let find_state re desc =
  try
    Automata.State.Table.find re.states desc
  with Not_found ->
    let st = mk_state re.ncolor desc in
    Automata.State.Table.add re.states desc st;
    st

(**** Match with marks ****)

let delta info cat ~color st =
  let desc = Automata.delta info.re.tbl cat color st.desc in
  let len = Array.length info.positions in
  if desc.Automata.State.idx = len && len > 0 then begin
    let pos = info.positions in
    info.positions <- Array.make (2 * len) 0;
    Array.blit pos 0 info.positions 0 len
  end;
  desc

let validate info (s:string) ~pos st =
  let color = Char.code (Bytes.get info.colors (Char.code s.[pos])) in
  let cat = category info.re ~color in
  let desc' = delta info cat ~color st in
  let st' = find_state info.re desc' in
  st.next.(color) <- st'

(*
let rec loop info s pos st =
  if pos < info.last then
    let st' = st.next.(Char.code info.cols.[Char.code s.[pos]]) in
    let idx = st'.idx in
    if idx >= 0 then begin
      info.positions.(idx) <- pos;
      loop info s (pos + 1) st'
    end else if idx = break then begin
      info.positions.(st'.real_idx) <- pos;
      st'
    end else begin (* Unknown *)
      validate info s pos st;
      loop info s pos st
    end
  else
    st
*)

let rec loop info (s:string) ~pos st =
  if pos < info.last then
    let st' = st.next.(Char.code (Bytes.get info.colors (Char.code s.[pos]))) in
    loop2 info s ~pos st st'
  else
    st

and loop2 info s ~pos st st' =
  if st'.idx >= 0 then begin
    let pos = pos + 1 in
    if pos < info.last then begin
      (* It is important to place these reads before the write *)
      (* But then, we don't have enough registers left to store the
         right position.  So, we store the position plus one. *)
      let st'' =
        st'.next.(Char.code (Bytes.get info.colors (Char.code s.[pos]))) in
      info.positions.(st'.idx) <- pos;
      loop2 info s ~pos st' st''
    end else begin
      info.positions.(st'.idx) <- pos;
      st'
    end
  end else if st'.idx = break then begin
    info.positions.(st'.real_idx) <- pos + 1;
    st'
  end else begin (* Unknown *)
    validate info s ~pos st;
    loop info s ~pos st
  end

let rec loop_no_mark info s ~pos ~last st =
  if pos < last then
    let st' = st.next.(Char.code (Bytes.get info.colors (Char.code s.[pos]))) in
    if st'.idx >= 0 then
      loop_no_mark info s ~pos:(pos + 1) ~last st'
    else if st'.idx = break then
      st'
    else begin (* Unknown *)
      validate info s ~pos st;
      loop_no_mark info s ~pos ~last st
    end
  else
    st

let final info st cat =
  try
    List.assq cat st.final
  with Not_found ->
    let st' = delta info cat ~color:(-1) st in
    let res = (st'.Automata.State.idx, Automata.status st') in
    st.final <- (cat, res) :: st.final;
    res

let find_initial_state re cat =
  try
    List.assq cat re.initial_states
  with Not_found ->
    let st = find_state re (Automata.State.create cat re.initial) in
    re.initial_states <- (cat, st) :: re.initial_states;
    st

let get_color re (s:string) pos =
  if pos < 0 then
    -1
  else
    let slen = String.length s in
    if pos >= slen then
      -1
    else if pos = slen - 1 && re.lnl <> -1 && s.[pos] = '\n' then
      (* Special case for the last newline *)
      re.lnl
    else
      Char.code (Bytes.get re.colors (Char.code s.[pos]))

let rec handle_last_newline info ~pos st ~groups =
  let st' = st.next.(info.re.lnl) in
  if st'.idx >= 0 then begin
    if groups then info.positions.(st'.idx) <- pos + 1;
    st'
  end else if st'.idx = break then begin
    if groups then info.positions.(st'.real_idx) <- pos + 1;
    st'
  end else begin (* Unknown *)
    let color = info.re.lnl in
    let real_c = Char.code (Bytes.get info.colors (Char.code '\n')) in
    let cat = category info.re ~color in
    let desc' = delta info cat ~color:real_c st in
    let st' = find_state info.re desc' in
    st.next.(color) <- st';
    handle_last_newline info ~pos st ~groups
  end

let rec scan_str info (s:string) initial_state ~groups =
  let pos = info.pos in
  let last = info.last in
  if (last = String.length s
      && info.re.lnl <> -1
      && last > pos
      && String.get s (last - 1) = '\n')
  then begin
    let info = { info with last = last - 1 } in
    let st = scan_str info s initial_state ~groups in
    if st.idx = break then
      st
    else
      handle_last_newline info ~pos:(last - 1) st ~groups
  end else if groups then
    loop info s ~pos initial_state
  else
    loop_no_mark info s ~pos ~last initial_state

let match_str ~groups ~partial re s ~pos ~len =
  let slen = String.length s in
  let last = if len = -1 then slen else pos + len in
  let info =
    { re ; colors = re.colors; pos ; last
    ; positions =
        if groups then begin
          let n = Automata.index_count re.tbl + 1 in
          if n <= 10 then
            [|0;0;0;0;0;0;0;0;0;0|]
          else
            Array.make n 0
        end else
          [||] }
  in
  let initial_cat =
    if pos = 0 then
      Category.(search_boundary ++ inexistant)
    else
      Category.(search_boundary
                ++ category re ~color:(get_color re s (pos - 1)))
  in
  let initial_state = find_initial_state re initial_cat in
  let st = scan_str info s initial_state ~groups in
  let res =
    if st.idx = break || partial then
      Automata.status st.desc
    else
      let final_cat =
        if last = slen then
          Category.(search_boundary ++ inexistant)
        else
          Category.(search_boundary ++ category re ~color:(get_color re s last))
      in
      let (idx, res) = final info st final_cat in
      if groups then info.positions.(idx) <- last + 1;
      res
  in
  match res with
    Automata.Match (marks, pmarks) ->
    Match { s ; marks; pmarks ; gpos = info.positions; gcount = re.group_count}
  | Automata.Failed -> Failed
  | Automata.Running -> Running

let mk_re ~initial ~colors ~color_repr ~ncolor ~lnl ~group_count =
  { initial ;
    initial_states = [];
    colors;
    color_repr;
    ncolor;
    lnl;
    tbl = Automata.create_working_area ();
    states = Automata.State.Table.create 97;
    group_count }

(**** Character sets ****)

let cseq c c' = Cset.seq (Char.code c) (Char.code c')
let cadd c s = Cset.add (Char.code c) s

let trans_set cache cm s =
  match Cset.one_char s with
  | Some i -> Cset.csingle (Bytes.get cm i)
  | None ->
    let v = (Cset.hash_rec s, s) in
    try
      Cset.CSetMap.find v !cache
    with Not_found ->
      let l =
        Cset.fold_right
          s
          ~f:(fun (i, j) l -> Cset.union (cseq (Bytes.get cm i)
                                            (Bytes.get cm j)) l)
          ~init:Cset.empty
      in
      cache := Cset.CSetMap.add v l !cache;
      l

(****)

type regexp =
    Set of Cset.t
  | Sequence of regexp list
  | Alternative of regexp list
  | Repeat of regexp * int * int option
  | Beg_of_line | End_of_line
  | Beg_of_word | End_of_word | Not_bound
  | Beg_of_str | End_of_str
  | Last_end_of_line | Start | Stop
  | Sem of Automata.sem * regexp
  | Sem_greedy of Automata.rep_kind * regexp
  | Group of regexp | No_group of regexp | Nest of regexp
  | Case of regexp | No_case of regexp
  | Intersection of regexp list
  | Complement of regexp list
  | Difference of regexp * regexp
  | Pmark of Pmark.t * regexp

module View = struct
  type t = regexp =
      Set of Cset.t
    | Sequence of regexp list
    | Alternative of regexp list
    | Repeat of regexp * int * int option
    | Beg_of_line | End_of_line
    | Beg_of_word | End_of_word | Not_bound
    | Beg_of_str | End_of_str
    | Last_end_of_line | Start | Stop
    | Sem of Automata.sem * regexp
    | Sem_greedy of Automata.rep_kind * regexp
    | Group of regexp | No_group of regexp | Nest of regexp
    | Case of regexp | No_case of regexp
    | Intersection of regexp list
    | Complement of regexp list
    | Difference of regexp * regexp
    | Pmark of Pmark.t * regexp

  let view t = t
end

let rec pp fmt t =
  let open Fmt in
  let var s re = sexp fmt s pp re in
  let seq s rel = sexp fmt s (list pp) rel in
  match t with
  | Set s ->  sexp fmt "Set" Cset.pp s
  | Sequence sq -> seq "Sequence" sq
  | Alternative alt -> seq "Alternative" alt
  | Repeat (re, start, stop) ->
    let pp' fmt () = fprintf fmt "%a@ %d%a" pp re   start   optint stop in
    sexp fmt "Repeat" pp' ()
  | Beg_of_line      -> str fmt "Beg_of_line"
  | End_of_line      -> str fmt "End_of_line"
  | Beg_of_word      -> str fmt "Beg_of_word"
  | End_of_word      -> str fmt "End_of_word"
  | Not_bound        -> str fmt "Not_bound"
  | Beg_of_str       -> str fmt "Beg_of_str"
  | End_of_str       -> str fmt "End_of_str"
  | Last_end_of_line -> str fmt "Last_end_of_line"
  | Start            -> str fmt "Start"
  | Stop             -> str fmt "Stop"
  | Sem (sem, re)    ->
    sexp fmt "Sem" (pair Automata.pp_sem pp) (sem, re)
  | Sem_greedy (k, re) ->
    sexp fmt "Sem_greedy" (pair Automata.pp_rep_kind pp) (k, re)
  | Group c        -> var "Group" c
  | No_group c     -> var "No_group" c
  | Nest c         -> var "Nest" c
  | Case c         -> var "Case" c
  | No_case c      -> var "No_case" c
  | Intersection c -> seq "Intersection" c
  | Complement c   -> seq "Complement" c
  | Difference (a, b) -> sexp fmt "Difference" (pair pp pp) (a, b)
  | Pmark (m, r)      -> sexp fmt "Pmark" (pair Pmark.pp pp) (m, r)

let rec is_charset = function
  | Set _ ->
    true
  | Alternative l | Intersection l | Complement l ->
    List.for_all is_charset l
  | Difference (r, r') ->
    is_charset r && is_charset r'
  | Sem (_, r) | Sem_greedy (_, r)
  | No_group r | Case r | No_case r ->
    is_charset r
  | Sequence _ | Repeat _ | Beg_of_line | End_of_line
  | Beg_of_word | End_of_word | Beg_of_str | End_of_str
  | Not_bound | Last_end_of_line | Start | Stop
  | Group _ | Nest _ | Pmark (_,_)->
    false

(*XXX Use a better algorithm allowing non-contiguous regions? *)

let cupper =
  Cset.union (cseq 'A' 'Z')
    (Cset.union (cseq '\192' '\214') (cseq '\216' '\222'))
let clower = Cset.offset 32 cupper
let calpha =
  List.fold_right cadd ['\170'; '\181'; '\186'; '\223'; '\255']
    (Cset.union clower cupper)
let cdigit = cseq '0' '9'
let calnum = Cset.union calpha cdigit
let cword = cadd '_' calnum

let colorize c regexp =
  let lnl = ref false in
  let rec colorize regexp =
    match regexp with
      Set s                     -> Color_map.split s c
    | Sequence l                -> List.iter colorize l
    | Alternative l             -> List.iter colorize l
    | Repeat (r, _, _)          -> colorize r
    | Beg_of_line | End_of_line -> Color_map.split (Cset.csingle '\n') c
    | Beg_of_word | End_of_word
    | Not_bound                 -> Color_map.split cword c
    | Beg_of_str | End_of_str
    | Start | Stop              -> ()
    | Last_end_of_line          -> lnl := true
    | Sem (_, r)
    | Sem_greedy (_, r)
    | Group r | No_group r
    | Nest r | Pmark (_,r)     -> colorize r
    | Case _ | No_case _
    | Intersection _
    | Complement _
    | Difference _              -> assert false
  in
  colorize regexp;
  !lnl

(**** Compilation ****)

let rec equal x1 x2 =
  match x1, x2 with
    Set s1, Set s2 ->
    s1 = s2
  | Sequence l1, Sequence l2 ->
    eq_list l1 l2
  | Alternative l1, Alternative l2 ->
    eq_list l1 l2
  | Repeat (x1', i1, j1), Repeat (x2', i2, j2) ->
    i1 = i2 && j1 = j2 && equal x1' x2'
  | Beg_of_line, Beg_of_line
  | End_of_line, End_of_line
  | Beg_of_word, Beg_of_word
  | End_of_word, End_of_word
  | Not_bound, Not_bound
  | Beg_of_str, Beg_of_str
  | End_of_str, End_of_str
  | Last_end_of_line, Last_end_of_line
  | Start, Start
  | Stop, Stop ->
    true
  | Sem (sem1, x1'), Sem (sem2, x2') ->
    sem1 = sem2 && equal x1' x2'
  | Sem_greedy (k1, x1'), Sem_greedy (k2, x2') ->
    k1 = k2 && equal x1' x2'
  | Group _, Group _ -> (* Do not merge groups! *)
    false
  | No_group x1', No_group x2' ->
    equal x1' x2'
  | Nest x1', Nest x2' ->
    equal x1' x2'
  | Case x1', Case x2' ->
    equal x1' x2'
  | No_case x1', No_case x2' ->
    equal x1' x2'
  | Intersection l1, Intersection l2 ->
    eq_list l1 l2
  | Complement l1, Complement l2 ->
    eq_list l1 l2
  | Difference (x1', x1''), Difference (x2', x2'') ->
    equal x1' x2' && equal x1'' x2''
  | Pmark (m1, r1), Pmark (m2, r2) ->
    Pmark.equal m1 m2 && equal r1 r2
  | _ ->
    false

and eq_list l1 l2 =
  match l1, l2 with
    [], [] ->
    true
  | x1 :: r1, x2 :: r2 ->
    equal x1 x2 && eq_list r1 r2
  | _ ->
    false

let sequence = function
  | [x] -> x
  | l   -> Sequence l

let rec merge_sequences = function
  | [] ->
    []
  | Alternative l' :: r ->
    merge_sequences (l' @ r)
  | Sequence (x :: y) :: r ->
    begin match merge_sequences r with
        Sequence (x' :: y') :: r' when equal x x' ->
        Sequence [x; Alternative [sequence y; sequence y']] :: r'
      | r' ->
        Sequence (x :: y) :: r'
    end
  | x :: r ->
    x :: merge_sequences r

module A = Automata

let enforce_kind ids kind kind' cr =
  match kind, kind' with
    `First, `First -> cr
  | `First, k       -> A.seq ids k cr (A.eps ids)
  |  _               -> cr

(* XXX should probably compute a category mask *)
let rec translate ids kind ign_group ign_case greedy pos cache c = function
  | Set s ->
    (A.cst ids (trans_set cache c s), kind)
  | Sequence l ->
    (trans_seq ids kind ign_group ign_case greedy pos cache c l, kind)
  | Alternative l ->
    begin match merge_sequences l with
        [r'] ->
        let (cr, kind') =
          translate ids kind ign_group ign_case greedy pos cache c r' in
        (enforce_kind ids kind kind' cr, kind)
      | merged_sequences ->
        (A.alt ids
           (List.map
              (fun r' ->
                 let (cr, kind') =
                   translate ids kind ign_group ign_case greedy
                     pos cache c r' in
                 enforce_kind ids kind kind' cr)
              merged_sequences),
         kind)
    end
  | Repeat (r', i, j) ->
    let (cr, kind') =
      translate ids kind ign_group ign_case greedy pos cache c r' in
    let rem =
      match j with
        None ->
        A.rep ids greedy kind' cr
      | Some j ->
        let f =
          match greedy with
            `Greedy ->
            fun rem ->
              A.alt ids
                [A.seq ids kind' (A.rename ids cr) rem; A.eps ids]
          | `Non_greedy ->
            fun rem ->
              A.alt ids
                [A.eps ids; A.seq ids kind' (A.rename ids cr) rem]
        in
        iter (j - i) f (A.eps ids)
    in
    (iter i (fun rem -> A.seq ids kind' (A.rename ids cr) rem) rem, kind)
  | Beg_of_line ->
    (A.after ids Category.(inexistant ++ newline), kind)
  | End_of_line ->
    (A.before ids Category.(inexistant ++ newline), kind)
  | Beg_of_word ->
    (A.seq ids `First
       (A.after ids Category.(inexistant ++ not_letter))
       (A.before ids Category.(inexistant ++ letter)),
     kind)
  | End_of_word ->
    (A.seq ids `First
       (A.after ids Category.(inexistant ++ letter))
       (A.before ids Category.(inexistant ++ not_letter)),
     kind)
  | Not_bound ->
    (A.alt ids [A.seq ids `First
                  (A.after ids Category.letter)
                  (A.before ids Category.letter);
                A.seq ids `First
                  (A.after ids Category.letter)
                  (A.before ids Category.letter)],
     kind)
  | Beg_of_str ->
    (A.after ids Category.inexistant, kind)
  | End_of_str ->
    (A.before ids Category.inexistant, kind)
  | Last_end_of_line ->
    (A.before ids Category.(inexistant ++ lastnewline), kind)
  | Start ->
    (A.after ids Category.search_boundary, kind)
  | Stop ->
    (A.before ids Category.search_boundary, kind)
  | Sem (kind', r') ->
    let (cr, kind'') =
      translate ids kind' ign_group ign_case greedy pos cache c r' in
    (enforce_kind ids kind' kind'' cr,
     kind')
  | Sem_greedy (greedy', r') ->
    translate ids kind ign_group ign_case greedy' pos cache c r'
  | Group r' ->
    if ign_group then
      translate ids kind ign_group ign_case greedy pos cache c r'
    else
      let p = !pos in
      pos := !pos + 2;
      let (cr, kind') =
        translate ids kind ign_group ign_case greedy pos cache c r' in
      (A.seq ids `First (A.mark ids p) (
          A.seq ids `First cr (A.mark ids (p + 1))),
       kind')
  | No_group r' ->
    translate ids kind true ign_case greedy pos cache c r'
  | Nest r' ->
    let b = !pos in
    let (cr, kind') =
      translate ids kind ign_group ign_case greedy pos cache c r'
    in
    let e = !pos - 1 in
    if e < b then
      (cr, kind')
    else
      (A.seq ids `First (A.erase ids b e) cr, kind')
  | Difference _ | Complement _ | Intersection _ | No_case _ | Case _ ->
    assert false
  | Pmark (i, r') ->
    let (cr, kind') =
      translate ids kind ign_group ign_case greedy pos cache c r' in
    (A.seq ids `First (A.pmark ids i) cr, kind')

and trans_seq ids kind ign_group ign_case greedy pos cache c = function
  | [] ->
    A.eps ids
  | [r] ->
    let (cr', kind') =
      translate ids kind ign_group ign_case greedy pos cache c r in
    enforce_kind ids kind kind' cr'
  | r :: rem ->
    let (cr', kind') =
      translate ids kind ign_group ign_case greedy pos cache c r in
    let cr'' =
      trans_seq ids kind ign_group ign_case greedy pos cache c rem in
    if A.is_eps cr'' then
      cr'
    else if A.is_eps cr' then
      cr''
    else
      A.seq ids kind' cr' cr''

(**** Case ****)

let case_insens s =
  Cset.union s (Cset.union (Cset.offset 32 (Cset.inter s cupper))
                  (Cset.offset (-32) (Cset.inter s clower)))

let as_set = function
  | Set s -> s
  | _     -> assert false

(* XXX Should split alternatives into (1) charsets and (2) more
   complex regular expressions; alternative should therefore probably
   be flatten here *)
let rec handle_case ign_case = function
  | Set s ->
    Set (if ign_case then case_insens s else s)
  | Sequence l ->
    Sequence (List.map (handle_case ign_case) l)
  | Alternative l ->
    let l' = List.map (handle_case ign_case) l in
    if is_charset (Alternative l') then
      Set (List.fold_left (fun s r -> Cset.union s (as_set r)) Cset.empty l')
    else
      Alternative l'
  | Repeat (r, i, j) ->
    Repeat (handle_case ign_case r, i, j)
  | Beg_of_line | End_of_line | Beg_of_word | End_of_word | Not_bound
  | Beg_of_str | End_of_str | Last_end_of_line | Start | Stop as r ->
    r
  | Sem (k, r) ->
    let r' = handle_case ign_case r in
    if is_charset r' then r' else Sem (k, r')
  | Sem_greedy (k, r) ->
    let r' = handle_case ign_case r in
    if is_charset r' then r' else Sem_greedy (k, r')
  | Group r ->
    Group (handle_case ign_case r)
  | No_group r ->
    let r' = handle_case ign_case r in
    if is_charset r' then r' else No_group r'
  | Nest r ->
    let r' = handle_case ign_case r in
    if is_charset r' then r' else Nest r'
  | Case r ->
    handle_case false r
  | No_case r ->
    handle_case true r
  | Intersection l ->
    let l' = List.map (fun r -> handle_case ign_case r) l in
    Set (List.fold_left (fun s r -> Cset.inter s (as_set r)) Cset.cany l')
  | Complement l ->
    let l' = List.map (fun r -> handle_case ign_case r) l in
    Set (Cset.diff Cset.cany
           (List.fold_left (fun s r -> Cset.union s (as_set r))
              Cset.empty l'))
  | Difference (r, r') ->
    Set (Cset.inter (as_set (handle_case ign_case r))
           (Cset.diff Cset.cany (as_set (handle_case ign_case r'))))
  | Pmark (i,r) -> Pmark (i,handle_case ign_case r)

(****)

let compile_1 regexp =
  let regexp = handle_case false regexp in
  let c = Color_map.make () in
  let need_lnl = colorize c regexp in
  let (colors, color_repr, ncolor) = Color_map.flatten c in
  let lnl = if need_lnl then ncolor else -1 in
  let ncolor = if need_lnl then ncolor + 1 else ncolor in
  let ids = A.create_ids () in
  let pos = ref 0 in
  let (r, kind) =
    translate ids
      `First false false `Greedy pos (ref Cset.CSetMap.empty) colors regexp in
  let r = enforce_kind ids `First kind r in
  (*Format.eprintf "<%d %d>@." !ids ncol;*)
  mk_re ~initial:r ~colors ~color_repr ~ncolor ~lnl ~group_count:(!pos / 2)

(****)

let rec anchored = function
  | Sequence l ->
    List.exists anchored l
  | Alternative l ->
    List.for_all anchored l
  | Repeat (r, i, _) ->
    i > 0 && anchored r
  | Set _ | Beg_of_line | End_of_line | Beg_of_word | End_of_word
  | Not_bound | End_of_str | Last_end_of_line | Stop
  | Intersection _ | Complement _ | Difference _ ->
    false
  | Beg_of_str | Start ->
    true
  | Sem (_, r) | Sem_greedy (_, r) | Group r | No_group r | Nest r
  | Case r | No_case r | Pmark (_, r) ->
    anchored r

(****)

type t = regexp

let str s =
  let l = ref [] in
  for i = String.length s - 1 downto 0 do
    l := Set (Cset.csingle s.[i]) :: !l
  done;
  Sequence !l
let char c = Set (Cset.csingle c)

let alt = function
  | [r] -> r
  | l   -> Alternative l
let seq = function
  | [r] -> r
  | l   -> Sequence l

let empty = alt []
let epsilon = seq []
let repn r i j =
  if i < 0 then invalid_arg "Re.repn";
  begin match j with
    | Some j when j < i -> invalid_arg "Re.repn"
    | _ -> ()
  end;
  Repeat (r, i, j)
let rep r = repn r 0 None
let rep1 r = repn r 1 None
let opt r = repn r 0 (Some 1)
let bol = Beg_of_line
let eol = End_of_line
let bow = Beg_of_word
let eow = End_of_word
let word r = seq [bow; r; eow]
let not_boundary = Not_bound
let bos = Beg_of_str
let eos = End_of_str
let whole_string r = seq [bos; r; eos]
let leol = Last_end_of_line
let start = Start
let stop = Stop
let longest r = Sem (`Longest, r)
let shortest r = Sem (`Shortest, r)
let first r = Sem (`First, r)
let greedy r = Sem_greedy (`Greedy, r)
let non_greedy r = Sem_greedy (`Non_greedy, r)
let group r = Group r
let no_group r = No_group r
let nest r = Nest r
let mark r = let i = Pmark.gen () in (i,Pmark (i,r))

let set str =
  let s = ref Cset.empty in
  for i = 0 to String.length str - 1 do
    s := Cset.union (Cset.csingle str.[i]) !s
  done;
  Set !s

let rg c c' = Set (cseq c c')

let inter l =
  let r = Intersection l in
  if is_charset r then
    r
  else
    invalid_arg "Re.inter"

let compl l =
  let r = Complement l in
  if is_charset r then
    r
  else
    invalid_arg "Re.compl"

let diff r r' =
  let r'' = Difference (r, r') in
  if is_charset r'' then
    r''
  else
    invalid_arg "Re.diff"

let any = Set Cset.cany
let notnl = Set (Cset.diff Cset.cany (Cset.csingle '\n'))

let lower = alt [rg 'a' 'z'; char '\181'; rg '\223' '\246'; rg '\248' '\255']
let upper = alt [rg 'A' 'Z'; rg '\192' '\214'; rg '\216' '\222']
let alpha = alt [lower; upper; char '\170'; char '\186']
let digit = rg '0' '9'
let alnum = alt [alpha; digit]
let wordc = alt [alnum; char '_']
let ascii = rg '\000' '\127'
let blank = set "\t "
let cntrl = alt [rg '\000' '\031'; rg '\127' '\159']
let graph = alt [rg '\033' '\126'; rg '\160' '\255']
let print = alt [rg '\032' '\126'; rg '\160' '\255']
let punct =
  alt [rg '\033' '\047'; rg '\058' '\064'; rg '\091' '\096';
       rg '\123' '\126'; rg '\160' '\169'; rg '\171' '\180';
       rg '\182' '\185'; rg '\187' '\191'; char '\215'; char '\247']
let space = alt [char ' '; rg '\009' '\013']
let xdigit = alt [digit; rg 'a' 'f'; rg 'A' 'F']

let case r = Case r
let no_case r = No_case r

(****)

let compile r =
  compile_1 (
    if anchored r then
      group r
    else
      seq [shortest (rep any); group r]
  )

let exec_internal name ?(pos=0) ?(len = -1) ~partial ~groups re s =
  if pos < 0 || len < -1 || pos + len > String.length s then
    invalid_arg name;
  match_str ~groups ~partial re s ~pos ~len

let exec ?pos ?len re s =
  match exec_internal "Re.exec" ?pos ?len ~groups:true ~partial:false re s with
    Match substr -> substr
  | _            -> raise Not_found

let exec_opt ?pos ?len re s =
  match exec_internal "Re.exec_opt" ?pos ?len ~groups:true ~partial:false
          re s with
    Match substr -> Some substr
  | _            -> None

let execp ?pos ?len re s =
  match exec_internal ~groups:false ~partial:false "Re.execp" ?pos ?len re s with
    Match _substr -> true
  | _             -> false

let exec_partial ?pos ?len re s =
  match exec_internal ~groups:false ~partial:true "Re.exec_partial"
          ?pos ?len re s with
    Match _ -> `Full
  | Running -> `Partial
  | Failed  -> `Mismatch

module Mark = struct

  type t = Pmark.t

  let test (g : Group.t) p =
    Pmark.Set.mem p g.pmarks

  let all (g : Group.t) = g.pmarks

  module Set = Pmark.Set

  let equal = Pmark.equal

  let compare = Pmark.compare

end

type split_token =
  [ `Text of string
  | `Delim of Group.t
  ]

module Rseq = struct
  let all ?(pos=0) ?len re s : _ Seq.t =
    if pos < 0 then invalid_arg "Re.all";
    (* index of the first position we do not consider.
       !pos < limit is an invariant *)
    let limit = match len with
      | None -> String.length s
      | Some l ->
        if l<0 || pos+l > String.length s then invalid_arg "Re.all";
        pos+l
    in
    (* iterate on matches. When a match is found, search for the next
       one just after its end *)
    let rec aux pos () =
      if pos >= limit
      then Seq.Nil (* no more matches *)
      else
        match match_str ~groups:true ~partial:false re s
                ~pos ~len:(limit - pos) with
        | Match substr ->
          let p1, p2 = Group.offset substr 0 in
          let pos = if p1=p2 then p2+1 else p2 in
          Seq.Cons (substr, aux pos)
        | Running
        | Failed -> Seq.Nil
    in
    aux pos

  let matches ?pos ?len re s : _ Seq.t =
    all ?pos ?len re s
    |> Seq.map (fun sub -> Group.get sub 0)

  let split_full ?(pos=0) ?len re s : _ Seq.t =
    if pos < 0 then invalid_arg "Re.split";
    let limit = match len with
      | None -> String.length s
      | Some l ->
        if l<0 || pos+l > String.length s then invalid_arg "Re.split";
        pos+l
    in
    (* i: start of delimited string
       pos: first position after last match of [re]
       limit: first index we ignore (!pos < limit is an invariant) *)
    let pos0 = pos in
    let rec aux state i pos () = match state with
      | `Idle when pos >= limit ->
        if i < limit then (
          let sub = String.sub s i (limit - i) in
          Seq.Cons (`Text sub, aux state (i+1) pos)
        ) else Seq.Nil
      | `Idle ->
        begin match match_str ~groups:true ~partial:false re s ~pos
                      ~len:(limit - pos) with
        | Match substr ->
          let p1, p2 = Group.offset substr 0 in
          let pos = if p1=p2 then p2+1 else p2 in
          let old_i = i in
          let i = p2 in
          if p1 > pos0 then (
            (* string does not start by a delimiter *)
            let text = String.sub s old_i (p1 - old_i) in
            let state = `Yield (`Delim substr) in
            Seq.Cons (`Text text, aux state i pos)
          ) else Seq.Cons (`Delim substr, aux state i pos)
        | Running -> Seq.Nil
        | Failed ->
          if i < limit
          then (
            let text = String.sub s i (limit - i) in
            (* yield last string *)
            Seq.Cons (`Text text, aux state limit pos)
          ) else
            Seq.Nil
        end
      | `Yield x ->
        Seq.Cons (x, aux `Idle i pos)
    in
    aux `Idle pos pos

  let split ?pos ?len re s : _ Seq.t =
    let seq = split_full ?pos ?len re s in
    let rec filter seq () = match seq ()  with
      | Seq.Nil -> Seq.Nil
      | Seq.Cons (`Delim _, tl) -> filter tl ()
      | Seq.Cons (`Text s,tl) -> Seq.Cons (s, filter tl)
    in filter seq
end

module Rlist = struct
  let list_of_seq (s:'a Seq.t) : 'a list =
    Seq.fold_left (fun l x -> x :: l) [] s |> List.rev

  let all ?pos ?len re s = Rseq.all ?pos ?len re s |> list_of_seq

  let matches ?pos ?len re s = Rseq.matches ?pos ?len re s |> list_of_seq

  let split_full ?pos ?len re s = Rseq.split_full ?pos ?len re s |> list_of_seq

  let split ?pos ?len re s = Rseq.split ?pos ?len re s |> list_of_seq
end

module Gen = struct
  type 'a gen = unit -> 'a option
  let gen_of_seq (s:'a Seq.t) : 'a gen =
    let r = ref s in
    fun () -> match !r () with
      | Seq.Nil -> None
      | Seq.Cons (x, tl) ->
        r := tl;
        Some x

  let split ?pos ?len re s : _ gen =
    Rseq.split ?pos ?len re s |> gen_of_seq

  let split_full ?pos ?len re s : _ gen =
    Rseq.split_full ?pos ?len re s |> gen_of_seq

  let all ?pos ?len re s = Rseq.all ?pos ?len re s |> gen_of_seq

  let matches ?pos ?len re s = Rseq.matches ?pos ?len re s |> gen_of_seq
end

let replace ?(pos=0) ?len ?(all=true) re ~f s =
  if pos < 0 then invalid_arg "Re.replace";
  let limit = match len with
    | None -> String.length s
    | Some l ->
      if l<0 || pos+l > String.length s then invalid_arg "Re.replace";
      pos+l
  in
  (* buffer into which we write the result *)
  let buf = Buffer.create (String.length s) in
  (* iterate on matched substrings. *)
  let rec iter pos =
    if pos < limit
    then
      match match_str ~groups:true ~partial:false re s ~pos ~len:(limit-pos) with
      | Match substr ->
        let p1, p2 = Group.offset substr 0 in
        (* add string between previous match and current match *)
        Buffer.add_substring buf s pos (p1-pos);
        (* what should we replace the matched group with? *)
        let replacing = f substr in
        Buffer.add_string buf replacing;
        if all then
          (* if we matched a non-char e.g. ^ we must manually advance by 1 *)
          iter (
            if p1=p2 then (
              (* a non char could be past the end of string. e.g. $ *)
              if p2 < limit then Buffer.add_char buf s.[p2];
              p2+1
            ) else
              p2)
        else
          Buffer.add_substring buf s p2 (limit-p2)
      | Running -> ()
      | Failed ->
        Buffer.add_substring buf s pos (limit-pos)
  in
  iter pos;
  Buffer.contents buf

let replace_string ?pos ?len ?all re ~by s =
  replace ?pos ?len ?all re s ~f:(fun _ -> by)

let witness t =
  let rec witness = function
    | Set c -> String.make 1 (Char.chr (Cset.pick c))
    | Sequence xs -> String.concat "" (List.map witness xs)
    | Alternative (x :: _) -> witness x
    | Alternative [] -> assert false
    | Repeat (r, from, _to) ->
      let w = witness r in
      let b = Buffer.create (String.length w * from) in
      for _i=1 to from do
        Buffer.add_string b w
      done;
      Buffer.contents b
    | No_case r -> witness r
    | Intersection _
    | Complement _
    | Difference (_, _) -> assert false
    | Group r
    | No_group r
    | Nest r
    | Sem (_, r)
    | Pmark (_, r)
    | Case r
    | Sem_greedy (_, r) -> witness r
    | Beg_of_line
    | End_of_line
    | Beg_of_word
    | End_of_word
    | Not_bound
    | Beg_of_str
    | Last_end_of_line
    | Start
    | Stop
    | End_of_str -> "" in
  witness (handle_case false t)

module Seq = Rseq
module List = Rlist
module Group = Group

(** {2 Deprecated functions} *)

type 'a gen        = 'a Gen.gen
let all_gen        = Gen.all
let matches_gen    = Gen.matches
let split_gen      = Gen.split
let split_full_gen = Gen.split_full


type substrings = Group.t

let get = Group.get
let get_ofs = Group.offset
let get_all = Group.all
let get_all_ofs = Group.all_offset
let test = Group.test

type markid = Mark.t

let marked = Mark.test
let mark_set = Mark.all

(**********************************)

(*
Information about the previous character:
- does not exists
- is a letter
- is not a letter
- is a newline
- is last newline

Beginning of word:
- previous is not a letter or does not exist
- current is a letter or does not exist

End of word:
- previous is a letter or does not exist
- current is not a letter or does not exist

Beginning of line:
- previous is a newline or does not exist

Beginning of buffer:
- previous does not exist

End of buffer
- current does not exist

End of line
- current is a newline or does not exist
*)

(*
Rep: e = T,e | ()
  - semantics of the comma (shortest/longest/first)
  - semantics of the union (greedy/non-greedy)

Bounded repetition
  a{0,3} = (a,(a,a?)?)?
*)

type groups = Group.t

include Rlist
