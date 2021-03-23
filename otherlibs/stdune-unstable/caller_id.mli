(** Who called me? *)

(** [get ~skip] returns the first element of the call stack that is not in
    [skip]. For instance,

    {[ get ~skip:[ __FILE__ ] ]}

    will return the first call site outside of the current file. *)
val get : skip:string list -> Loc.t option
