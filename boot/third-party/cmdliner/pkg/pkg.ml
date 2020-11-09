#!/usr/bin/env ocaml
#use "topfind"
#require "topkg"
open Topkg

let test t = Pkg.flatten [ Pkg.test ~run:false t; Pkg.doc (t ^ ".ml")]

let distrib =
  let exclude_paths () = Ok [".git";".gitignore";".gitattributes";"_build"] in
  Pkg.distrib ~exclude_paths ()

let opams =
  [Pkg.opam_file "cmdliner.opam"]

let () =
  Pkg.describe ~distrib "cmdliner" ~opams @@ fun c ->
  Ok [ Pkg.mllib ~api:["Cmdliner"] "src/cmdliner.mllib";
       test "test/chorus";
       test "test/cp_ex";
       test "test/darcs_ex";
       test "test/revolt";
       test "test/rm_ex";
       test "test/tail_ex";
       Pkg.test ~run:false "test/test_man";
       Pkg.test ~run:false "test/test_man_utf8";
       Pkg.test ~run:false "test/test_pos";
       Pkg.test ~run:false "test/test_pos_rev";
       Pkg.test ~run:false "test/test_pos_all";
       Pkg.test ~run:false "test/test_pos_left";
       Pkg.test ~run:false "test/test_pos_req";
       Pkg.test ~run:false "test/test_opt_req";
       Pkg.test ~run:false "test/test_term_dups";
       Pkg.test ~run:false "test/test_with_used_args"; ]
