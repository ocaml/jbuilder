The describe-project command outputs a human-readable text description of a
project in the current workspace.

  $ dune describe-project
  Project name: project_name
  Public library: lib1
  Dependencies: lib1-dep1, lib1-dep2
  Synopsis: What lib1 does

It can output data in JSON format:

  $ dune describe-project --format=json
  { "name": "project_name",
    "libs":
    [ { "name": "lib1",
        "deps": [ "lib1-dep1",
                  "lib1-dep2"
                  ],
        "synopsis": "What lib1 does"
        }
      ]
    }

And as a S-expression:

  $ dune describe-project --format=sexp
  ((name "project_name")
   (libs
     (((name "lib1")
       (deps ("lib1-dep1" "lib1-dep2"))
       (synopsis "What lib1 does")))))
