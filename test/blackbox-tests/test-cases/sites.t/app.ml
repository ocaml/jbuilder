(* load all the available plugins *)
let () = Sites.Plugins.Plugins.load_all ()

let () = print_endline "Main app starts..."
(* Execute the code registered by the plugins *)
let () = Queue.iter (fun f -> f ()) Registration.todo
