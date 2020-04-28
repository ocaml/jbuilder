No ppx driver found

  $ dune build --root driver-tests .foo1.objs/lib.cma
  Entering directory 'driver-tests'
  File "dune", line 6, characters 13-18:
  6 |  (preprocess (pps)))
                   ^^^^^
  Error: You must specify at least one ppx rewriter.
  [1]

Too many drivers

  $ dune build --root driver-tests .foo2.objs/lib.cma
  Entering directory 'driver-tests'
  File "dune", line 13, characters 13-28:
  13 |  (preprocess (pps ppx1 ppx2)))
                    ^^^^^^^^^^^^^^^
  Error: Too many incompatible ppx drivers were found: foo.driver2 and
  foo.driver1.
  [1]

Not compatible with Dune

  $ dune build --root driver-tests .foo3.objs/lib.cma
  Entering directory 'driver-tests'
  File "dune", line 20, characters 13-28:
  20 |  (preprocess (pps ppx_other)))
                    ^^^^^^^^^^^^^^^
  Error: No ppx driver were found. It seems that ppx_other is not compatible
  with Dune. Examples of ppx rewriters that are compatible with Dune are ones
  using ocaml-migrate-parsetree, ppxlib or ppx_driver.
  [1]

Incompatible Cookies

  $ dune build --root driver-tests .foo4.objs/lib.cma
  Entering directory 'driver-tests'
  File "dune", line 27, characters 13-28:
  27 |  (preprocess (pps ppx3 ppx4)))
                    ^^^^^^^^^^^^^^^
  Error: foo.ppx3 and foo.ppx4 have inconsistent requests for cookie "germany";
  foo.ppx3 requests "spritzgeback" and foo.ppx4 requests "lebkuchen"
  [1]

Same, but with error pointing to .ppx

  $ dune build --root driver-tests .ppx/foo.ppx1+foo.ppx2/ppx.exe
  Entering directory 'driver-tests'
  Error: invalid ppx key for default/.ppx/foo.ppx1+foo.ppx2/ppx.exe
  [1]

  $ dune build --root driver-tests .ppx/foo.ppx-other/ppx.exe
  Entering directory 'driver-tests'
  Error: invalid ppx key for default/.ppx/foo.ppx-other/ppx.exe
  [1]

Test the argument syntax

  $ dune build --root driver-tests .test_ppx_args.objs/lib.cma
  Entering directory 'driver-tests'
           ppx test_ppx_args.pp.ml
  .ppx/454728df5270ab91f8a5af6b5e860eb0/ppx.exe
  -arg1
  -arg2
  -arg3=Oreo
  -foo
  bar
  Snickerdoodle
  --cookie
  france="Petit Beurre"
  --cookie
  italy="Biscotti"
  --cookie
  library-name="test_ppx_args"
  -o
  test_ppx_args.pp.ml
  --impl
  test_ppx_args.ml
  --as-ppx
  File "dune", line 101, characters 3-138:
  101 |    (pps -arg1 driver_print_args ppx_with_cookies_print_args -arg2 -arg3=%{env:AMERICA=undefined} --
  102 |     -foo bar %{env:ENGLAND=undefined})))
  Error: Rule failed to generate the following targets:
  - test_ppx_args.pp.ml
  [1]

Test that going throught the -ppx option of the compiler works

  $ dune build --root driver-tests .test_ppx_staged.objs/lib.cma
  Entering directory 'driver-tests'
      ocamldep .test_ppx_staged.objs/test_ppx_staged.ml.d
  tool name: ocamldep
  args:--as-ppx -arg1 -arg2 -arg3=Oreo -foo bar Snickerdoodle --cookie france="Petit Beurre" --cookie italy="Biscotti" --cookie library-name="test_ppx_staged"
        ocamlc .test_ppx_staged.objs/byte/test_ppx_staged.{cmi,cmo,cmt}
  tool name: ocamlc
  args:--as-ppx -arg1 -arg2 -arg3=Oreo -foo bar Snickerdoodle --cookie france="Petit Beurre" --cookie italy="Biscotti" --cookie library-name="test_ppx_staged"

Test using installed drivers

  $ dune build --root driver @install
  Entering directory 'driver'
  $ OCAMLPATH=driver/_build/install/default/lib dune build --root use-external-driver .driveruser.objs/lib.cma
  Entering directory 'use-external-driver'
           ppx driveruser.pp.ml
  .ppx/35d69311d5da258d073875db2b34f33b/ppx.exe
  -arg1
  -arg2
  -foo
  bar
  --cookie
  library-name="driveruser"
  -o
  driveruser.pp.ml
  --impl
  driveruser.ml
  --as-ppx
  File "dune", line 6, characters 13-53:
  6 |  (preprocess (pps -arg1 testdriver -arg2 -- -foo bar)))
                   ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
  Error: Rule failed to generate the following targets:
  - driveruser.pp.ml
  [1]

  $ OCAMLPATH=driver/_build/install/default/lib dune build --root replaces .driveruser.objs/lib.cma
  Entering directory 'replaces'
           ppx driveruser.pp.ml
  replacesdriver
  .ppx/886937db0da323b743b4366c6d3a795f/ppx.exe
  -arg1
  -arg2
  -foo
  bar
  --cookie
  library-name="driveruser"
  -o
  driveruser.pp.ml
  --impl
  driveruser.ml
  --as-ppx
  File "dune", line 13, characters 13-57:
  13 |  (preprocess (pps -arg1 replacesdriver -arg2 -- -foo bar)))
                    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
  Error: Rule failed to generate the following targets:
  - driveruser.pp.ml
  [1]

  $ OCAMLPATH=driver/_build/install/default/lib dune build --root driver-replaces @install
  Entering directory 'driver-replaces'
  $ OCAMLPATH=driver/_build/install/default/lib:driver-replaces/_build/install/default/lib dune build --root replaces-external .driveruser.objs/lib.cma
  Entering directory 'replaces-external'
           ppx driveruser.pp.ml
  replacesdriver
  .ppx/886937db0da323b743b4366c6d3a795f/ppx.exe
  -arg1
  -arg2
  -foo
  bar
  --cookie
  library-name="driveruser"
  -o
  driveruser.pp.ml
  --impl
  driveruser.ml
  --as-ppx
  File "dune", line 5, characters 13-57:
  5 |  (preprocess (pps -arg1 replacesdriver -arg2 -- -foo bar)))
                   ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
  Error: Rule failed to generate the following targets:
  - driveruser.pp.ml
  [1]
