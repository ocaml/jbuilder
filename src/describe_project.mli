type format = Text | JSON | Sexp

val describe :
  Dune_project.t -> Dune_load.Dune_file.t list -> format:format -> unit
