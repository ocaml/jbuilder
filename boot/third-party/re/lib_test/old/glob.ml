(** This is a little command line tool to test the library.
@author Christian Lindig <lindig@gmail.com>
*)

module R = Re
module G = Re.Glob

exception Error of string
let error fmt   = Printf.kprintf (fun msg -> raise (Error msg)) fmt
let eprintf     = Printf.eprintf
let printf      = Printf.printf

let (@@) f x    = f x

let glob pattern str =
    let rx = R.compile @@ R.whole_string @@ G.glob ~expand_braces:true pattern in
    (* let () = R.print_re Format.std_formatter rx in *)
    if R.execp rx str
    then printf "%s matches:       %s\n" pattern str
    else printf "%s doesn't match: %s\n" pattern str

let main () =
    let argv = Array.to_list Sys.argv in
    let this = List.hd argv in
    let args = List.tl argv in
        match args with
        | [] | [_]   -> error "usage: %s pattern string .." this
        | p :: strs  -> List.iter (glob p) strs

let () =
    try
        main (); exit 0
    with
    | Error(msg)    -> eprintf "Error: %s\n" msg; exit 1
    | _             -> eprintf "unknown exception raised\n"; exit 1

