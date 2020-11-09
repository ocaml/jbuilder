type state =
  { idx : int;
    next : state array }

type info = { i_cols : string; last : int }

let unknown = {idx = -1; next = [||]}
let st1 = { idx = 0; next = Array.make 256 unknown }
let st2 = { idx = 0; next = Array.make 256 st1 }
let cols = String.create 256

let _ =
  for i = 0 to 255 do st1.next.(i) <- st2; cols.[i] <- Char.chr i done

(* 1.33
let rec loop s pos last st =
  if pos < last then begin
    ignore s.[pos];
    loop s (pos + 1) last st.next.(127)
  end

let exec s = loop s 0 (String.length s) st1
*)

(* 1.67
let rec loop s pos last st =
  if pos < last then begin
    ignore s.[pos];
    loop s (pos + 1) last st.next.(127)
  end

let exec s = loop s 0 (String.length s) st1
*)

(* 1.76
let rec loop s pos last st =
  if pos < last then begin
    let c = s.[pos] in
    let st' = st.next.(Char.code c) in
    loop s (pos + 1) last st'
  end

let exec s = loop s 0 (String.length s) st1
*)

(* 1.81
let rec loop cols s pos last st =
  if pos < last then begin
    let c' = cols.[Char.code s.[pos]] in
    let st' = st.next.(Char.code c') in
    loop cols s (pos + 1) last st'
  end

let exec s = loop cols s 0 (String.length s) st1
*)

(* 1.84
let rec loop info s pos last st =
  if pos < last then begin
    let c' = info.i_cols.[Char.code s.[pos]] in
    let st' = st.next.(Char.code c') in
    loop info s (pos + 1) last st'
  end

let exec s =
  loop {i_cols = cols; last = String.length s} s 0 (String.length s) st1
*)

(* 1.95
let rec loop info s pos last st =
  if pos < info.last then begin
    let c' = info.i_cols.[Char.code s.[pos]] in
    let st' = st.next.(Char.code c') in
    loop info s (pos + 1) last st'
  end

let exec s =
  loop {i_cols = cols; last = String.length s} s 0 (String.length s) st1
*)

(* 1.85
let rec loop info s pos cols st =
  if pos < info.last then begin
    let c' = cols.[Char.code s.[pos]] in
    let st' = st.next.(Char.code c') in
    loop info s (pos + 1) cols st'
  end

let exec s = loop {i_cols = cols; last = String.length s} s 0 cols st1
*)

let rec loop info s pos cols st =
  if pos < info.last then begin
    let c1 = cols.[Char.code s.[pos]] in
    let st1 = st.next.(Char.code c1) in
    let pos = pos + 1 in
    let c2 = cols.[Char.code s.[pos]] in
    let st2 = st1.next.(Char.code c2) in
    let pos = pos + 1 in
    let c3 = cols.[Char.code s.[pos]] in
    let st3 = st2.next.(Char.code c3) in
    let pos = pos + 1 in
    let c4 = cols.[Char.code s.[pos]] in
    let st4 = st3.next.(Char.code c4) in
    loop info s (pos + 1) cols st4
  end

let exec s = loop {i_cols = cols; last = String.length s} s 0 cols st1

(* 2.20
let rec loop info s pos last st idx =
  if idx >= 0 then begin
    if pos < last then begin
      let c' = info.i_cols.[Char.code s.[pos]] in
      let st' = st.next.(Char.code c') in
  let idx = st'.idx in
      loop info s (pos + 1) last st' idx
    end
  end else
    ()

let exec s =
  loop {i_cols = cols; last = String.length s} s 0 (String.length s) st1 0
*)

(*
let rec loop info s pos cols st st' =
  if pos < info.last then begin
    let c' = cols.[Char.code s.[pos]] in
    let idx = st.idx in
    if idx >= 0 then begin
      let st' = st.next.(Char.code c') in
      loop info s (pos + 1) cols st' st
    end else if idx = -1 then
      ()
    else (* Unknown *)
      validate info s pos st'
  end

and validate info s pos st' = validate info s pos st'

let exec s = loop {i_cols = cols; last = String.length s} s 0 cols st1 st1
*)

let _ =
  let s = String.make (1024*1024) 'a' in
  s.[1024*1024-1] <- 'b';
  for _i = 0 to 99 do
    ignore (exec s)
  done
