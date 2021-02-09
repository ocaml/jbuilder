(* Benchmark the scheduler *)

open Stdune
open Dune_engine

let setup =
  lazy
    (Path.set_root (Path.External.cwd ());
     Path.Build.set_build_dir (Path.Build.Kind.of_string "_build"))

let prog = Option.value_exn (Bin.which ~path:(Env.path Env.initial) "true")

let run () = Process.run ~env:Env.initial Strict prog []

let go ~jobs fiber =
  Scheduler.go fiber ~config:{ Config.default with concurrency = Fixed jobs }

let%bench_fun "single" =
  Lazy.force setup;
  fun () -> go run ~jobs:1

let l = List.init 100 ~f:ignore

let%bench_fun ("many"[@indexed jobs = [ 1; 2; 4; 8 ]]) =
  Lazy.force setup;
  fun () -> go ~jobs (fun () -> Fiber.parallel_iter l ~f:run)
