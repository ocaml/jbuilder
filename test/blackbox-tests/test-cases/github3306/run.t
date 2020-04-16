  $ echo "(lang dune 2.0)" > dune-project
  $ cat >dune <<EOF
  > (executable
  > (name main))
  > (rule
  >  (targets lib.ml)
  >  (deps ../libraries/lib.ml)
  >  (action
  >  (chdir
  >   %{workspace_root}
  >   (copy# %{deps} %{targets}))))
  > EOF
  $ echo "let () = Lib.run ()" > main.ml
  $ dune exec ./main.exe
  Error: exception { exn =
      ("[gen_rules] did not specify rules for the context",
      { context_name = "libraries"; dir = "libraries" })
  ; backtrace =
      [ { ocaml =
            "Raised at file \"src/stdune/code_error.ml\", line 9, characters
  30-62\n\
             Called from file \"src/dune/build_system.ml\", line 892,
  characters 10-195\n\
             Called from file \"src/stdune/exn_with_backtrace.ml\", line 9,
  characters 8-12\n\
             "
        ; memo = ("load-dir", In_build_dir "libraries")
        }
      ]
  ; outer_call_stack =
      [ ("build-file", In_build_dir "libraries/lib.ml")
      ; ("execute-rule",
        { id = 0
        ; loc =
            { pos_fname = "dune"
            ; start = { pos_lnum = 3; pos_bol = 25; pos_cnum = 25 }
            ; stop = { pos_lnum = 9; pos_bol = 114; pos_cnum = 145 }
            }
        })
      ; ("build-file", In_build_dir "default/lib.ml")
      ; ("execute-rule",
        { id = 8
        ; loc =
            { pos_fname = ".main.eobjs/byte/_unknown_"
            ; start = { pos_lnum = 1; pos_bol = 0; pos_cnum = 0 }
            ; stop = { pos_lnum = 1; pos_bol = 0; pos_cnum = 0 }
            }
        })
      ; ("build-file",
        In_build_dir "default/.main.eobjs/byte/dune__exe__Lib.cmi")
      ; ("execute-rule",
        { id = 9
        ; loc =
            { pos_fname = ".main.eobjs/native/_unknown_"
            ; start = { pos_lnum = 1; pos_bol = 0; pos_cnum = 0 }
            ; stop = { pos_lnum = 1; pos_bol = 0; pos_cnum = 0 }
            }
        })
      ; ("build-file",
        In_build_dir "default/.main.eobjs/native/dune__exe__Lib.cmx")
      ; ("execute-rule",
        { id = 12
        ; loc =
            { pos_fname = "dune"
            ; start = { pos_lnum = 2; pos_bol = 12; pos_cnum = 18 }
            ; stop = { pos_lnum = 2; pos_bol = 12; pos_cnum = 22 }
            }
        })
      ; ("build-file", In_build_dir "default/main.exe")
      ]
  }
  Raised at file "src/stdune/code_error.ml", line 9, characters 30-62
  Called from file "src/dune/build_system.ml", line 892, characters 10-195
  Called from file "src/stdune/exn_with_backtrace.ml", line 9, characters 8-12
  Re-raised at file "src/stdune/exn.ml", line 37, characters 27-56
  Called from file "src/dune/build_system.ml", line 1089, characters 15-28
  Called from file "src/dune/build_system.ml", line 1108, characters 16-55
  Called from file "src/dune/build_system.ml", line 1657, characters 8-33
  Called from file "src/fiber/fiber.ml", line 114, characters 10-15
  Re-raised at file "src/stdune/exn.ml", line 37, characters 27-56
  Called from file "src/fiber/fiber.ml", line 85, characters 10-17
  Re-raised at file "src/stdune/exn.ml", line 37, characters 27-56
  Called from file "src/fiber/fiber.ml", line 85, characters 10-17
  Re-raised at file "src/stdune/exn.ml", line 37, characters 27-56
  Called from file "src/fiber/fiber.ml", line 85, characters 10-17
  Re-raised at file "src/stdune/exn.ml", line 37, characters 27-56
  Called from file "src/fiber/fiber.ml", line 85, characters 10-17
  Re-raised at file "src/stdune/exn.ml", line 37, characters 27-56
  Called from file "src/fiber/fiber.ml", line 85, characters 10-17
  Re-raised at file "src/stdune/exn.ml", line 37, characters 27-56
  Called from file "src/fiber/fiber.ml", line 85, characters 10-17
  
  I must not crash.  Uncertainty is the mind-killer. Exceptions are the
  little-death that brings total obliteration.  I will fully express my cases. 
  Execution will pass over me and through me.  And when it has gone past, I
  will unwind the stack along its path.  Where the cases are handled there will
  be nothing.  Only I will remain.
  [1]
