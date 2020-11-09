#!/usr/bin/env ocaml
#use "topfind";;
#directory "_build/lib";;
#load "re.cma";;
#load "re_posix.cma";;
let re = Re.compile (Re_posix.re "a*(ab)");;
let s = "aaaaabaaa aaabaaabaaaabaa axbaaba";;

let rec iter_gen f g = match g() with
  | None -> ()
  | Some x -> f x; iter_gen f g
;;

Re.Easy.iter_gen re s
  |> iter_gen
    (fun s ->
      let i,j = Re.get_ofs s 0 in
      Printf.printf "%s at %d,%d\n" (Re.get s 0) i j
    );;

