open! Dune_engine
open! Stdune

module Ast = struct
  type ('k, 'v) t =
    { on : String_with_vars.t
    ; clauses : ('k * 'v) list
    }

  let decode k v =
    let open Dune_lang.Decoder in
    enter
      (let* loc, () = located (keyword "case") in
       let* on = String_with_vars.decode in
       let clause =
         enter
           (let* elt = k in
            let* () = keyword "->" in
            let+ v = v in
            (elt, v))
       in
       let+ clauses = repeat clause in
       (match clauses with
       | [] ->
         User_error.raise ~loc
           [ Pp.text "case expression must have at least one clause" ]
       | _ :: _ -> ());
       { on; clauses })
end

type 'a t = (Predicate_lang.Glob.t, 'a) Ast.t

let decode f = Ast.decode Predicate_lang.Glob.decode f

let eval (t : _ t) ~f =
  let elem = f t.on in
  List.find_map t.clauses ~f:(fun (keys, v) ->
      Option.some_if
        (Predicate_lang.Glob.exec keys ~standard:Predicate_lang.empty elem)
        v)
