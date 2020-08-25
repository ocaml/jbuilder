Test generation of opam files, as well as handling of versions

Simple test
-----------

The `dune build` should generate the opam file

  $ mkdir test1

  $ cat >test1/dune-project <<EOF
  > (lang dune 1.10)
  > (version 1.0.0)
  > (name cohttp)
  > (source (github mirage/ocaml-cohttp))
  > (license ISC)
  > (authors "Anil Madhavapeddy" "Rudi Grinberg")
  > ;
  > (generate_opam_files true)
  > ;
  > (package
  >   (name cohttp)
  >   (synopsis "An OCaml library for HTTP clients and servers")
  >   (description "A longer description")
  >   (depends
  >     (alcotest :with-test)
  >     (dune (and :build (> 1.5)))
  >     (foo (and :dev (> 1.5) (< 2.0)))
  >     (uri (>= 1.9.0))
  >     (uri (< 2.0.0))
  >     (fieldslib (> v0.12))
  >     (fieldslib (< v0.13))))
  > ;
  > (package
  >   (name cohttp-async)
  >   (synopsis "HTTP client and server for the Async library")
  >   (description "A _really_ long description")
  >   (depends
  >     (cohttp (>= 1.0.2))
  >     (conduit-async (>= 1.0.3))
  >     (async (>= v0.10.0))
  >     (async (< v0.12))))
  > EOF

  $ dune build @install --root test1
  Entering directory 'test1'

  $ cat test1/cohttp.opam
  # This file is generated by dune, edit dune-project instead
  opam-version: "2.0"
  build: [
    ["dune" "subst"] {pinned}
    ["dune" "build" "-p" name "-j" jobs]
    ["dune" "runtest" "-p" name "-j" jobs] {with-test}
    ["dune" "build" "-p" name "@doc"] {with-doc}
  ]
  authors: ["Anil Madhavapeddy" "Rudi Grinberg"]
  bug-reports: "https://github.com/mirage/ocaml-cohttp/issues"
  homepage: "https://github.com/mirage/ocaml-cohttp"
  license: "ISC"
  version: "1.0.0"
  dev-repo: "git+https://github.com/mirage/ocaml-cohttp.git"
  synopsis: "An OCaml library for HTTP clients and servers"
  description: "A longer description"
  depends: [
    "alcotest" {with-test}
    "dune" {build & > "1.5"}
    "foo" {dev & > "1.5" & < "2.0"}
    "uri" {>= "1.9.0"}
    "uri" {< "2.0.0"}
    "fieldslib" {> "v0.12"}
    "fieldslib" {< "v0.13"}
  ]

  $ cat test1/cohttp-async.opam
  # This file is generated by dune, edit dune-project instead
  opam-version: "2.0"
  build: [
    ["dune" "subst"] {pinned}
    ["dune" "build" "-p" name "-j" jobs]
    ["dune" "runtest" "-p" name "-j" jobs] {with-test}
    ["dune" "build" "-p" name "@doc"] {with-doc}
  ]
  authors: ["Anil Madhavapeddy" "Rudi Grinberg"]
  bug-reports: "https://github.com/mirage/ocaml-cohttp/issues"
  homepage: "https://github.com/mirage/ocaml-cohttp"
  license: "ISC"
  version: "1.0.0"
  dev-repo: "git+https://github.com/mirage/ocaml-cohttp.git"
  synopsis: "HTTP client and server for the Async library"
  description: "A _really_ long description"
  depends: [
    "cohttp" {>= "1.0.2"}
    "conduit-async" {>= "1.0.3"}
    "async" {>= "v0.10.0"}
    "async" {< "v0.12"}
  ]

Fatal error with opam file that is not listed in the dune-project file:

  $ echo "cannot parse me" > test1/foo.opam
  $ dune build @install --root test1
  Entering directory 'test1'
  File "foo.opam", line 1, characters 0-0:
  Error: This opam file doesn't have a corresponding (package ...) stanza in
  the dune-project_file. Since you have at least one other (package ...) stanza
  in your dune-project file, you must a (package ...) stanza for each opam
  package in your project.
  [1]

Package information fields can be overridden per-package:

  $ mkdir test2
  $ cat >test2/dune-project <<EOF
  > (lang dune 2.5)
  > (name foo)
  > (version 1.0.0)
  > (source (github mirage/ocaml-cohttp))
  > (license ISC)
  > (authors "Anil Madhavapeddy" "Rudi Grinberg")
  > (homepage https://my.home.page)
  > ;
  > (generate_opam_files true)
  > ;
  > (package
  >  (name foo)
  >  (version 1.0.1)
  >  (source (github mirage/foo))
  >  (license MIT)
  >  (authors "Foo" "Bar"))
  > EOF

  $ dune build @install --root test2
  Entering directory 'test2'

  $ cat test2/foo.opam
  # This file is generated by dune, edit dune-project instead
  opam-version: "2.0"
  version: "1.0.1"
  authors: ["Foo" "Bar"]
  license: "MIT"
  homepage: "https://my.home.page"
  bug-reports: "https://github.com/mirage/foo/issues"
  depends: [
    "dune" {>= "2.5"}
  ]
  build: [
    ["dune" "subst"] {pinned}
    [
      "dune"
      "build"
      "-p"
      name
      "-j"
      jobs
      "@install"
      "@runtest" {with-test}
      "@doc" {with-doc}
    ]
  ]
  dev-repo: "git+https://github.com/mirage/foo.git"

Version generated in opam and META files
----------------------------------------

After calling `dune subst`, dune should embed the version inside the
generated META and opam files.

### With opam files and no package stanzas

  $ mkdir version

  $ cat > version/dune-project <<EOF
  > (lang dune 1.10)
  > (name foo)
  > EOF

  $ cat > version/foo.opam <<EOF
  > EOF

  $ cat > version/dune <<EOF
  > (library (public_name foo))
  > EOF

  $ (cd version
  >  git init -q
  >  git add .
  >  git commit -qm _
  >  git tag -a 1.0 -m 1.0
  >  dune subst)

  $ dune build --root version foo.opam META.foo
  Entering directory 'version'

  $ grep ^version version/foo.opam
  version: "1.0"

  $ grep ^version version/_build/default/META.foo
  version = "1.0"

### With package stanzas and generating the opam files

  $ rm -rf version
  $ mkdir version

  $ cat > version/dune-project <<EOF
  > (lang dune 1.10)
  > (name foo)
  > (generate_opam_files true)
  > (package (name foo))
  > EOF

  $ cat > version/foo.opam <<EOF
  > EOF

  $ cat > version/dune <<EOF
  > (library (public_name foo))
  > EOF

  $ (cd version
  >  git init -q
  >  git add .
  >  git commit -qm _
  >  git tag -a 1.0 -m 1.0
  >  dune subst)

  $ dune build --root version foo.opam META.foo
  Entering directory 'version'

  $ grep ^version version/foo.opam
  version: "1.0"

  $ grep ^version version/_build/default/META.foo
  version = "1.0"

Generation of opam files with lang dune >= 1.11
-----------------------------------------------

  $ mkdir gen-v1.11
  $ cat > gen-v1.11/dune-project <<EOF
  > (lang dune 1.11)
  > (name test)
  > (generate_opam_files true)
  > (package (name test))
  > EOF

  $ dune build @install --root gen-v1.11
  Entering directory 'gen-v1.11'
  $ cat gen-v1.11/test.opam
  # This file is generated by dune, edit dune-project instead
  opam-version: "2.0"
  depends: [
    "dune" {>= "1.11"}
  ]
  build: [
    ["dune" "subst"] {pinned}
    [
      "dune"
      "build"
      "-p"
      name
      "-j"
      jobs
      "@install"
      "@runtest" {with-test}
      "@doc" {with-doc}
    ]
  ]

Templates
---------

  $ mkdir template

  $ cat > template/dune-project <<EOF
  > (lang dune 2.0)
  > (name foo)
  > (generate_opam_files true)
  > (package (name foo) (depends bar))
  > EOF

Test various fields in the template file. Fields coming from the
template are always put at the end. Fields generated by Dune are
sorted in a way that pleases "opam lint".

  $ cat > template/foo.opam.template <<EOF
  > x-foo: "blah"
  > EOF
  $ dune build @install --root template
  Entering directory 'template'
  $ tail -n 1 template/foo.opam
  x-foo: "blah"

  $ cat > template/foo.opam.template <<EOF
  > libraries: [ "blah" ]
  > EOF
  $ dune build @install --root template
  Entering directory 'template'
  $ tail -n 1 template/foo.opam
  libraries: [ "blah" ]

  $ cat > template/foo.opam.template <<EOF
  > depends: [ "overridden" ]
  > EOF
  $ dune build @install --root template
  Entering directory 'template'
  $ tail -n 1 template/foo.opam
  depends: [ "overridden" ]

Using binary operators for dependencies
---------------------------------------

  $ mkdir binops

Not supported before 2.1:

  $ cat > binops/dune-project <<EOF
  > (lang dune 2.0)
  > (name foo)
  > (generate_opam_files true)
  > (package
  >  (name foo)
  >  (depends (conf-libX11 (<> :os win32))))
  > EOF

  $ dune build @install --root binops
  Entering directory 'binops'
  File "dune-project", line 6, characters 23-37:
  6 |  (depends (conf-libX11 (<> :os win32))))
                             ^^^^^^^^^^^^^^
  Error: Passing two arguments to <> is only available since version 2.1 of the
  dune language. Please update your dune-project file to have (lang dune 2.1).
  [1]

Supported since 2.1:

  $ cat > binops/dune-project <<EOF
  > (lang dune 2.1)
  > (name foo)
  > (generate_opam_files true)
  > (package
  >  (name foo)
  >  (depends (conf-libX11 (<> :os win32))))
  > EOF

  $ dune build @install --root binops
  Entering directory 'binops'
  $ grep conf-libX11 binops/foo.opam
    "conf-libX11" {os != "win32"}

Version constraint on dune deps
-------------------------------

  $ mkdir dune-dep
  $ cd dune-dep

Without the dune dependency declared in the dune-project file, we
generate a dune dependency with a constraint:

  $ cat > dune-project <<EOF
  > (lang dune 2.1)
  > (name foo)
  > (generate_opam_files true)
  > (package (name foo))
  > EOF

  $ dune build foo.opam
  $ grep -A2 ^depends: foo.opam
  depends: [
    "dune" {>= "2.1"}
  ]

With the dune dependency declared in the dune-project file and version
of the language < 2.6 we don't add the constraint:

  $ cat > dune-project <<EOF
  > (lang dune 2.5)
  > (name foo)
  > (generate_opam_files true)
  > (package (name foo) (depends dune))
  > EOF

  $ dune build foo.opam
  $ grep ^depends: foo.opam
  depends: ["dune"]

Same with version of the language >= 2.6, we now add the constraint:

  $ cat > dune-project <<EOF
  > (lang dune 2.6)
  > (name foo)
  > (generate_opam_files true)
  > (package (name foo) (depends dune))
  > EOF

  $ dune build foo.opam
  $ grep -A2 ^depends: foo.opam
  depends: [
    "dune" {>= "2.6"}
  ]

When the version of the language >= 2.7 we use dev instead of pinned
when calling dune subst:

  $ cat > dune-project <<EOF
  > (lang dune 2.7)
  > (name foo)
  > (generate_opam_files true)
  > (package (name foo))
  > EOF

  $ dune build foo.opam
  $ grep -A13 ^build: foo.opam
  build: [
    ["dune" "subst"] {dev}
    [
      "dune"
      "build"
      "-p"
      name
      "-j"
      jobs
      "@install"
      "@runtest" {with-test}
      "@doc" {with-doc}
    ]
  ]

When the version of the language >= 2.7, odoc is automatically added to
the doc dependencies:

  $ cat > dune-project <<EOF
  > (lang dune 2.7)
  > (name foo)
  > (generate_opam_files true)
  > (package (name foo))
  > EOF

  $ dune build foo.opam
  $ grep -A3 ^depends: foo.opam
  depends: [
    "dune" {>= "2.7"}
    "odoc" {with-doc}
  ]

  $ cat > dune-project <<EOF
  > (lang dune 2.7)
  > (name foo)
  > (generate_opam_files true)
  > (package (name foo) (depends something))
  > EOF

  $ dune build foo.opam
  $ grep -A4 ^depends: foo.opam
  depends: [
    "dune" {>= "2.7"}
    "something"
    "odoc" {with-doc}
  ]

  $ cat > dune-project <<EOF
  > (lang dune 2.7)
  > (name foo)
  > (generate_opam_files true)
  > (package (name foo) (depends odoc something))
  > EOF

  $ dune build foo.opam
  $ grep -A4 ^depends: foo.opam
  depends: [
    "dune" {>= "2.7"}
    "odoc"
    "something"
  ]

  $ cat > dune-project <<EOF
  > (lang dune 2.7)
  > (name foo)
  > (generate_opam_files true)
  > (package (name foo) (depends (odoc :with-doc) something))
  > EOF

  $ dune build foo.opam
  $ grep -A4 ^depends: foo.opam
  depends: [
    "dune" {>= "2.7"}
    "odoc" {with-doc}
    "something"
  ]

  $ cat > dune-project <<EOF
  > (lang dune 2.7)
  > (name foo)
  > (generate_opam_files true)
  > (package (name foo) (depends (odoc (and :with-doc (>= 1.5.0))) something))
  > EOF

  $ dune build foo.opam
  $ grep -A4 ^depends: foo.opam
  depends: [
    "dune" {>= "2.7"}
    "odoc" {with-doc & >= "1.5.0"}
    "something"
  ]

  $ cat > dune-project <<EOF
  > (lang dune 2.7)
  > (name foo)
  > (generate_opam_files true)
  > (package (name foo) (depends (odoc :with-test) something))
  > EOF

  $ dune build foo.opam
  $ grep -A5 ^depends: foo.opam
  depends: [
    "dune" {>= "2.7"}
    "odoc" {with-test}
    "something"
    "odoc" {with-doc}
  ]

wrong dune constraint

  $ cat > dune-project <<EOF
  > (lang dune 2.8)
  > (name foo)
  > (generate_opam_files true)
  > (package (name foo) (depends (dune (>= 2.7)) ))
  > EOF

  $ dune build foo.opam
  Warning: The supplied dune constraint 2.7 is not compatible with lang dune in dune-project 2.8
  Set dune constraint to >= 2.8
