
let x = Re_pcre.regexp "aa?b"
let _ =
  let s = String.make (1024*1024) 'a' in
  s.[1024*1024-1] <- 'b';
  for _i = 0 to 99 do
    ignore (Re_pcre.exec ~rex:x s)
  done
