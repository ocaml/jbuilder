
open Re

let x = compile (seq [char 'a'; opt (char 'a'); char 'b'])
let _ =
  let s = String.make (1024*1024) 'a' in
  s.[1024*1024-1] <- 'b';
  for _i = 0 to 99 do
    ignore (exec x s)
  done
