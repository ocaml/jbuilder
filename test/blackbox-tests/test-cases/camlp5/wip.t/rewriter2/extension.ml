open Pcaml

EXTEND
  expr: BEFORE "expr1"
  [ [ "sum";
      e =
      FOLD0 (fun e1 e2 -> <:expr< $e2$ + $e1$ >>) <:expr< 0 >>
        expr LEVEL "expr1" SEP ";";
      "end" -> e ] ] ;
END
