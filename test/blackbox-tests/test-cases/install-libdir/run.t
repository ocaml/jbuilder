  $ opam_prefix="$(opam config var prefix)"
  $ export BUILD_PATH_PREFIX_MAP="/OPAM_PREFIX=$opam_prefix:$BUILD_PATH_PREFIX_MAP"

`dune install` should handle destination directories that don't exist

  $ dune build @install
  $ dune install --prefix install --libdir lib
  Installing install/lib/foo/META
  Installing install/lib/foo/dune-package
  Installing install/lib/foo/foo.cmi
  Installing install/lib/foo/foo.cmt
  Installing install/lib/foo/foo.cmx
  Installing install/lib/foo/foo.ml
  Installing install/lib/foo/lib$ext_lib
  Installing install/lib/foo/lib.cma
  Installing install/lib/foo/lib.cmxa
  Installing install/lib/foo/lib.cmxs
  Installing install/lib/foo/opam
  Installing install/bin/exec
  Installing install/man/a-man-page-with-no-ext
  Installing install/man/man1/a-man-page.1
  Installing install/man/man3/another-man-page.3

If prefix is passed, the default for libdir is `$prefix/lib`:

  $ dune install --prefix install --dry-run
  Installing install/lib/foo/META
  Installing install/lib/foo/dune-package
  Installing install/lib/foo/foo.cmi
  Installing install/lib/foo/foo.cmt
  Installing install/lib/foo/foo.cmx
  Installing install/lib/foo/foo.ml
  Installing install/lib/foo/lib$ext_lib
  Installing install/lib/foo/lib.cma
  Installing install/lib/foo/lib.cmxa
  Installing install/lib/foo/lib.cmxs
  Installing install/lib/foo/opam
  Installing install/bin/exec
  Installing install/man/a-man-page-with-no-ext
  Installing install/man/man1/a-man-page.1
  Installing install/man/man3/another-man-page.3
  Removing (if it exists) install/lib/foo/META
  Creating directory install/lib/foo
  Copying _build/install/default/lib/foo/META to install/lib/foo/META (executable: false)
  Removing (if it exists) install/lib/foo/dune-package
  Creating directory install/lib/foo
  Copying _build/install/default/lib/foo/dune-package to install/lib/foo/dune-package (executable: false)
  Removing (if it exists) install/lib/foo/foo.cmi
  Creating directory install/lib/foo
  Copying _build/install/default/lib/foo/foo.cmi to install/lib/foo/foo.cmi (executable: false)
  Removing (if it exists) install/lib/foo/foo.cmt
  Creating directory install/lib/foo
  Copying _build/install/default/lib/foo/foo.cmt to install/lib/foo/foo.cmt (executable: false)
  Removing (if it exists) install/lib/foo/foo.cmx
  Creating directory install/lib/foo
  Copying _build/install/default/lib/foo/foo.cmx to install/lib/foo/foo.cmx (executable: false)
  Removing (if it exists) install/lib/foo/foo.ml
  Creating directory install/lib/foo
  Copying _build/install/default/lib/foo/foo.ml to install/lib/foo/foo.ml (executable: false)
  Removing (if it exists) install/lib/foo/lib$ext_lib
  Creating directory install/lib/foo
  Copying _build/install/default/lib/foo/lib$ext_lib to install/lib/foo/lib$ext_lib (executable: false)
  Removing (if it exists) install/lib/foo/lib.cma
  Creating directory install/lib/foo
  Copying _build/install/default/lib/foo/lib.cma to install/lib/foo/lib.cma (executable: false)
  Removing (if it exists) install/lib/foo/lib.cmxa
  Creating directory install/lib/foo
  Copying _build/install/default/lib/foo/lib.cmxa to install/lib/foo/lib.cmxa (executable: false)
  Removing (if it exists) install/lib/foo/lib.cmxs
  Creating directory install/lib/foo
  Copying _build/install/default/lib/foo/lib.cmxs to install/lib/foo/lib.cmxs (executable: false)
  Removing (if it exists) install/lib/foo/opam
  Creating directory install/lib/foo
  Copying _build/install/default/lib/foo/opam to install/lib/foo/opam (executable: false)
  Removing (if it exists) install/bin/exec
  Creating directory install/bin
  Copying _build/install/default/bin/exec to install/bin/exec (executable: true)
  Removing (if it exists) install/man/a-man-page-with-no-ext
  Creating directory install/man
  Copying _build/install/default/man/a-man-page-with-no-ext to install/man/a-man-page-with-no-ext (executable: false)
  Removing (if it exists) install/man/man1/a-man-page.1
  Creating directory install/man/man1
  Copying _build/install/default/man/man1/a-man-page.1 to install/man/man1/a-man-page.1 (executable: false)
  Removing (if it exists) install/man/man3/another-man-page.3
  Creating directory install/man/man3
  Copying _build/install/default/man/man3/another-man-page.3 to install/man/man3/another-man-page.3 (executable: false)

If prefix is not passed, libdir defaults to the output of `ocamlfind printconf
destdir`:

  $ (export OCAMLFIND_DESTDIR=/OCAMLFIND_DESTDIR
  >  dune install --dry-run
  >  dune uninstall --dry-run)
  Installing /OCAMLFIND_DESTDIR/foo/META
  Installing /OCAMLFIND_DESTDIR/foo/dune-package
  Installing /OCAMLFIND_DESTDIR/foo/foo.cmi
  Installing /OCAMLFIND_DESTDIR/foo/foo.cmt
  Installing /OCAMLFIND_DESTDIR/foo/foo.cmx
  Installing /OCAMLFIND_DESTDIR/foo/foo.ml
  Installing /OCAMLFIND_DESTDIR/foo/lib$ext_lib
  Installing /OCAMLFIND_DESTDIR/foo/lib.cma
  Installing /OCAMLFIND_DESTDIR/foo/lib.cmxa
  Installing /OCAMLFIND_DESTDIR/foo/lib.cmxs
  Installing /OCAMLFIND_DESTDIR/foo/opam
  Installing /OPAM_PREFIX/bin/exec
  Installing /OPAM_PREFIX/man/a-man-page-with-no-ext
  Installing /OPAM_PREFIX/man/man1/a-man-page.1
  Installing /OPAM_PREFIX/man/man3/another-man-page.3
  Removing (if it exists) /OCAMLFIND_DESTDIR/foo/META
  Creating directory /OCAMLFIND_DESTDIR/foo
  Copying _build/install/default/lib/foo/META to /OCAMLFIND_DESTDIR/foo/META (executable: false)
  Removing (if it exists) /OCAMLFIND_DESTDIR/foo/dune-package
  Creating directory /OCAMLFIND_DESTDIR/foo
  Copying _build/install/default/lib/foo/dune-package to /OCAMLFIND_DESTDIR/foo/dune-package (executable: false)
  Removing (if it exists) /OCAMLFIND_DESTDIR/foo/foo.cmi
  Creating directory /OCAMLFIND_DESTDIR/foo
  Copying _build/install/default/lib/foo/foo.cmi to /OCAMLFIND_DESTDIR/foo/foo.cmi (executable: false)
  Removing (if it exists) /OCAMLFIND_DESTDIR/foo/foo.cmt
  Creating directory /OCAMLFIND_DESTDIR/foo
  Copying _build/install/default/lib/foo/foo.cmt to /OCAMLFIND_DESTDIR/foo/foo.cmt (executable: false)
  Removing (if it exists) /OCAMLFIND_DESTDIR/foo/foo.cmx
  Creating directory /OCAMLFIND_DESTDIR/foo
  Copying _build/install/default/lib/foo/foo.cmx to /OCAMLFIND_DESTDIR/foo/foo.cmx (executable: false)
  Removing (if it exists) /OCAMLFIND_DESTDIR/foo/foo.ml
  Creating directory /OCAMLFIND_DESTDIR/foo
  Copying _build/install/default/lib/foo/foo.ml to /OCAMLFIND_DESTDIR/foo/foo.ml (executable: false)
  Removing (if it exists) /OCAMLFIND_DESTDIR/foo/lib$ext_lib
  Creating directory /OCAMLFIND_DESTDIR/foo
  Copying _build/install/default/lib/foo/lib$ext_lib to /OCAMLFIND_DESTDIR/foo/lib$ext_lib (executable: false)
  Removing (if it exists) /OCAMLFIND_DESTDIR/foo/lib.cma
  Creating directory /OCAMLFIND_DESTDIR/foo
  Copying _build/install/default/lib/foo/lib.cma to /OCAMLFIND_DESTDIR/foo/lib.cma (executable: false)
  Removing (if it exists) /OCAMLFIND_DESTDIR/foo/lib.cmxa
  Creating directory /OCAMLFIND_DESTDIR/foo
  Copying _build/install/default/lib/foo/lib.cmxa to /OCAMLFIND_DESTDIR/foo/lib.cmxa (executable: false)
  Removing (if it exists) /OCAMLFIND_DESTDIR/foo/lib.cmxs
  Creating directory /OCAMLFIND_DESTDIR/foo
  Copying _build/install/default/lib/foo/lib.cmxs to /OCAMLFIND_DESTDIR/foo/lib.cmxs (executable: false)
  Removing (if it exists) /OCAMLFIND_DESTDIR/foo/opam
  Creating directory /OCAMLFIND_DESTDIR/foo
  Copying _build/install/default/lib/foo/opam to /OCAMLFIND_DESTDIR/foo/opam (executable: false)
  Removing (if it exists) /OPAM_PREFIX/bin/exec
  Creating directory /OPAM_PREFIX/bin
  Copying _build/install/default/bin/exec to /OPAM_PREFIX/bin/exec (executable: true)
  Removing (if it exists) /OPAM_PREFIX/man/a-man-page-with-no-ext
  Creating directory /OPAM_PREFIX/man
  Copying _build/install/default/man/a-man-page-with-no-ext to /OPAM_PREFIX/man/a-man-page-with-no-ext (executable: false)
  Removing (if it exists) /OPAM_PREFIX/man/man1/a-man-page.1
  Creating directory /OPAM_PREFIX/man/man1
  Copying _build/install/default/man/man1/a-man-page.1 to /OPAM_PREFIX/man/man1/a-man-page.1 (executable: false)
  Removing (if it exists) /OPAM_PREFIX/man/man3/another-man-page.3
  Creating directory /OPAM_PREFIX/man/man3
  Copying _build/install/default/man/man3/another-man-page.3 to /OPAM_PREFIX/man/man3/another-man-page.3 (executable: false)
  Removing (if it exists) /OCAMLFIND_DESTDIR/foo/META
  Removing (if it exists) /OCAMLFIND_DESTDIR/foo/dune-package
  Removing (if it exists) /OCAMLFIND_DESTDIR/foo/foo.cmi
  Removing (if it exists) /OCAMLFIND_DESTDIR/foo/foo.cmt
  Removing (if it exists) /OCAMLFIND_DESTDIR/foo/foo.cmx
  Removing (if it exists) /OCAMLFIND_DESTDIR/foo/foo.ml
  Removing (if it exists) /OCAMLFIND_DESTDIR/foo/lib$ext_lib
  Removing (if it exists) /OCAMLFIND_DESTDIR/foo/lib.cma
  Removing (if it exists) /OCAMLFIND_DESTDIR/foo/lib.cmxa
  Removing (if it exists) /OCAMLFIND_DESTDIR/foo/lib.cmxs
  Removing (if it exists) /OCAMLFIND_DESTDIR/foo/opam
  Removing (if it exists) /OPAM_PREFIX/bin/exec
  Removing (if it exists) /OPAM_PREFIX/man/a-man-page-with-no-ext
  Removing (if it exists) /OPAM_PREFIX/man/man1/a-man-page.1
  Removing (if it exists) /OPAM_PREFIX/man/man3/another-man-page.3
  Removing directory (if empty) /OPAM_PREFIX/man/man3
  Removing directory (if empty) /OPAM_PREFIX/man/man1
  Removing directory (if empty) /OPAM_PREFIX/man
  Removing directory (if empty) /OPAM_PREFIX/bin
  Removing directory (if empty) /OCAMLFIND_DESTDIR/foo

If only libdir is passed, binaries are installed under prefix/bin and libraries
in libdir:

  $ dune install --libdir /LIBDIR --dry-run
  > dune uninstall --libdir /LIBDIR --dry-run
  Installing /LIBDIR/foo/META
  Installing /LIBDIR/foo/dune-package
  Installing /LIBDIR/foo/foo.cmi
  Installing /LIBDIR/foo/foo.cmt
  Installing /LIBDIR/foo/foo.cmx
  Installing /LIBDIR/foo/foo.ml
  Installing /LIBDIR/foo/lib$ext_lib
  Installing /LIBDIR/foo/lib.cma
  Installing /LIBDIR/foo/lib.cmxa
  Installing /LIBDIR/foo/lib.cmxs
  Installing /LIBDIR/foo/opam
  Installing /OPAM_PREFIX/bin/exec
  Installing /OPAM_PREFIX/man/a-man-page-with-no-ext
  Installing /OPAM_PREFIX/man/man1/a-man-page.1
  Installing /OPAM_PREFIX/man/man3/another-man-page.3
  Removing (if it exists) /LIBDIR/foo/META
  Creating directory /LIBDIR/foo
  Copying _build/install/default/lib/foo/META to /LIBDIR/foo/META (executable: false)
  Removing (if it exists) /LIBDIR/foo/dune-package
  Creating directory /LIBDIR/foo
  Copying _build/install/default/lib/foo/dune-package to /LIBDIR/foo/dune-package (executable: false)
  Removing (if it exists) /LIBDIR/foo/foo.cmi
  Creating directory /LIBDIR/foo
  Copying _build/install/default/lib/foo/foo.cmi to /LIBDIR/foo/foo.cmi (executable: false)
  Removing (if it exists) /LIBDIR/foo/foo.cmt
  Creating directory /LIBDIR/foo
  Copying _build/install/default/lib/foo/foo.cmt to /LIBDIR/foo/foo.cmt (executable: false)
  Removing (if it exists) /LIBDIR/foo/foo.cmx
  Creating directory /LIBDIR/foo
  Copying _build/install/default/lib/foo/foo.cmx to /LIBDIR/foo/foo.cmx (executable: false)
  Removing (if it exists) /LIBDIR/foo/foo.ml
  Creating directory /LIBDIR/foo
  Copying _build/install/default/lib/foo/foo.ml to /LIBDIR/foo/foo.ml (executable: false)
  Removing (if it exists) /LIBDIR/foo/lib$ext_lib
  Creating directory /LIBDIR/foo
  Copying _build/install/default/lib/foo/lib$ext_lib to /LIBDIR/foo/lib$ext_lib (executable: false)
  Removing (if it exists) /LIBDIR/foo/lib.cma
  Creating directory /LIBDIR/foo
  Copying _build/install/default/lib/foo/lib.cma to /LIBDIR/foo/lib.cma (executable: false)
  Removing (if it exists) /LIBDIR/foo/lib.cmxa
  Creating directory /LIBDIR/foo
  Copying _build/install/default/lib/foo/lib.cmxa to /LIBDIR/foo/lib.cmxa (executable: false)
  Removing (if it exists) /LIBDIR/foo/lib.cmxs
  Creating directory /LIBDIR/foo
  Copying _build/install/default/lib/foo/lib.cmxs to /LIBDIR/foo/lib.cmxs (executable: false)
  Removing (if it exists) /LIBDIR/foo/opam
  Creating directory /LIBDIR/foo
  Copying _build/install/default/lib/foo/opam to /LIBDIR/foo/opam (executable: false)
  Removing (if it exists) /OPAM_PREFIX/bin/exec
  Creating directory /OPAM_PREFIX/bin
  Copying _build/install/default/bin/exec to /OPAM_PREFIX/bin/exec (executable: true)
  Removing (if it exists) /OPAM_PREFIX/man/a-man-page-with-no-ext
  Creating directory /OPAM_PREFIX/man
  Copying _build/install/default/man/a-man-page-with-no-ext to /OPAM_PREFIX/man/a-man-page-with-no-ext (executable: false)
  Removing (if it exists) /OPAM_PREFIX/man/man1/a-man-page.1
  Creating directory /OPAM_PREFIX/man/man1
  Copying _build/install/default/man/man1/a-man-page.1 to /OPAM_PREFIX/man/man1/a-man-page.1 (executable: false)
  Removing (if it exists) /OPAM_PREFIX/man/man3/another-man-page.3
  Creating directory /OPAM_PREFIX/man/man3
  Copying _build/install/default/man/man3/another-man-page.3 to /OPAM_PREFIX/man/man3/another-man-page.3 (executable: false)
  Removing (if it exists) /LIBDIR/foo/META
  Removing (if it exists) /LIBDIR/foo/dune-package
  Removing (if it exists) /LIBDIR/foo/foo.cmi
  Removing (if it exists) /LIBDIR/foo/foo.cmt
  Removing (if it exists) /LIBDIR/foo/foo.cmx
  Removing (if it exists) /LIBDIR/foo/foo.ml
  Removing (if it exists) /LIBDIR/foo/lib$ext_lib
  Removing (if it exists) /LIBDIR/foo/lib.cma
  Removing (if it exists) /LIBDIR/foo/lib.cmxa
  Removing (if it exists) /LIBDIR/foo/lib.cmxs
  Removing (if it exists) /LIBDIR/foo/opam
  Removing (if it exists) /OPAM_PREFIX/bin/exec
  Removing (if it exists) /OPAM_PREFIX/man/a-man-page-with-no-ext
  Removing (if it exists) /OPAM_PREFIX/man/man1/a-man-page.1
  Removing (if it exists) /OPAM_PREFIX/man/man3/another-man-page.3
  Removing directory (if empty) /OPAM_PREFIX/man/man3
  Removing directory (if empty) /OPAM_PREFIX/man/man1
  Removing directory (if empty) /OPAM_PREFIX/man
  Removing directory (if empty) /OPAM_PREFIX/bin
  Removing directory (if empty) /LIBDIR/foo

The DESTDIR var is supported. When set, it is prepended to the prefix.
This is the case when the prefix is implicit:

  $ DESTDIR=DESTDIR dune install --dry-run
  Installing DESTDIR/OPAM_PREFIX/lib/foo/META
  Installing DESTDIR/OPAM_PREFIX/lib/foo/dune-package
  Installing DESTDIR/OPAM_PREFIX/lib/foo/foo.cmi
  Installing DESTDIR/OPAM_PREFIX/lib/foo/foo.cmt
  Installing DESTDIR/OPAM_PREFIX/lib/foo/foo.cmx
  Installing DESTDIR/OPAM_PREFIX/lib/foo/foo.ml
  Installing DESTDIR/OPAM_PREFIX/lib/foo/lib$ext_lib
  Installing DESTDIR/OPAM_PREFIX/lib/foo/lib.cma
  Installing DESTDIR/OPAM_PREFIX/lib/foo/lib.cmxa
  Installing DESTDIR/OPAM_PREFIX/lib/foo/lib.cmxs
  Installing DESTDIR/OPAM_PREFIX/lib/foo/opam
  Installing DESTDIR/OPAM_PREFIX/bin/exec
  Installing DESTDIR/OPAM_PREFIX/man/a-man-page-with-no-ext
  Installing DESTDIR/OPAM_PREFIX/man/man1/a-man-page.1
  Installing DESTDIR/OPAM_PREFIX/man/man3/another-man-page.3
  Removing (if it exists) DESTDIR/OPAM_PREFIX/lib/foo/META
  Creating directory DESTDIR/OPAM_PREFIX/lib/foo
  Copying _build/install/default/lib/foo/META to DESTDIR/OPAM_PREFIX/lib/foo/META (executable: false)
  Removing (if it exists) DESTDIR/OPAM_PREFIX/lib/foo/dune-package
  Creating directory DESTDIR/OPAM_PREFIX/lib/foo
  Copying _build/install/default/lib/foo/dune-package to DESTDIR/OPAM_PREFIX/lib/foo/dune-package (executable: false)
  Removing (if it exists) DESTDIR/OPAM_PREFIX/lib/foo/foo.cmi
  Creating directory DESTDIR/OPAM_PREFIX/lib/foo
  Copying _build/install/default/lib/foo/foo.cmi to DESTDIR/OPAM_PREFIX/lib/foo/foo.cmi (executable: false)
  Removing (if it exists) DESTDIR/OPAM_PREFIX/lib/foo/foo.cmt
  Creating directory DESTDIR/OPAM_PREFIX/lib/foo
  Copying _build/install/default/lib/foo/foo.cmt to DESTDIR/OPAM_PREFIX/lib/foo/foo.cmt (executable: false)
  Removing (if it exists) DESTDIR/OPAM_PREFIX/lib/foo/foo.cmx
  Creating directory DESTDIR/OPAM_PREFIX/lib/foo
  Copying _build/install/default/lib/foo/foo.cmx to DESTDIR/OPAM_PREFIX/lib/foo/foo.cmx (executable: false)
  Removing (if it exists) DESTDIR/OPAM_PREFIX/lib/foo/foo.ml
  Creating directory DESTDIR/OPAM_PREFIX/lib/foo
  Copying _build/install/default/lib/foo/foo.ml to DESTDIR/OPAM_PREFIX/lib/foo/foo.ml (executable: false)
  Removing (if it exists) DESTDIR/OPAM_PREFIX/lib/foo/lib$ext_lib
  Creating directory DESTDIR/OPAM_PREFIX/lib/foo
  Copying _build/install/default/lib/foo/lib$ext_lib to DESTDIR/OPAM_PREFIX/lib/foo/lib$ext_lib (executable: false)
  Removing (if it exists) DESTDIR/OPAM_PREFIX/lib/foo/lib.cma
  Creating directory DESTDIR/OPAM_PREFIX/lib/foo
  Copying _build/install/default/lib/foo/lib.cma to DESTDIR/OPAM_PREFIX/lib/foo/lib.cma (executable: false)
  Removing (if it exists) DESTDIR/OPAM_PREFIX/lib/foo/lib.cmxa
  Creating directory DESTDIR/OPAM_PREFIX/lib/foo
  Copying _build/install/default/lib/foo/lib.cmxa to DESTDIR/OPAM_PREFIX/lib/foo/lib.cmxa (executable: false)
  Removing (if it exists) DESTDIR/OPAM_PREFIX/lib/foo/lib.cmxs
  Creating directory DESTDIR/OPAM_PREFIX/lib/foo
  Copying _build/install/default/lib/foo/lib.cmxs to DESTDIR/OPAM_PREFIX/lib/foo/lib.cmxs (executable: false)
  Removing (if it exists) DESTDIR/OPAM_PREFIX/lib/foo/opam
  Creating directory DESTDIR/OPAM_PREFIX/lib/foo
  Copying _build/install/default/lib/foo/opam to DESTDIR/OPAM_PREFIX/lib/foo/opam (executable: false)
  Removing (if it exists) DESTDIR/OPAM_PREFIX/bin/exec
  Creating directory DESTDIR/OPAM_PREFIX/bin
  Copying _build/install/default/bin/exec to DESTDIR/OPAM_PREFIX/bin/exec (executable: true)
  Removing (if it exists) DESTDIR/OPAM_PREFIX/man/a-man-page-with-no-ext
  Creating directory DESTDIR/OPAM_PREFIX/man
  Copying _build/install/default/man/a-man-page-with-no-ext to DESTDIR/OPAM_PREFIX/man/a-man-page-with-no-ext (executable: false)
  Removing (if it exists) DESTDIR/OPAM_PREFIX/man/man1/a-man-page.1
  Creating directory DESTDIR/OPAM_PREFIX/man/man1
  Copying _build/install/default/man/man1/a-man-page.1 to DESTDIR/OPAM_PREFIX/man/man1/a-man-page.1 (executable: false)
  Removing (if it exists) DESTDIR/OPAM_PREFIX/man/man3/another-man-page.3
  Creating directory DESTDIR/OPAM_PREFIX/man/man3
  Copying _build/install/default/man/man3/another-man-page.3 to DESTDIR/OPAM_PREFIX/man/man3/another-man-page.3 (executable: false)

But also when the prefix is explicit:

  $ DESTDIR=DESTDIR dune install --prefix prefix --dry-run
  Installing DESTDIR/prefix/lib/foo/META
  Installing DESTDIR/prefix/lib/foo/dune-package
  Installing DESTDIR/prefix/lib/foo/foo.cmi
  Installing DESTDIR/prefix/lib/foo/foo.cmt
  Installing DESTDIR/prefix/lib/foo/foo.cmx
  Installing DESTDIR/prefix/lib/foo/foo.ml
  Installing DESTDIR/prefix/lib/foo/lib$ext_lib
  Installing DESTDIR/prefix/lib/foo/lib.cma
  Installing DESTDIR/prefix/lib/foo/lib.cmxa
  Installing DESTDIR/prefix/lib/foo/lib.cmxs
  Installing DESTDIR/prefix/lib/foo/opam
  Installing DESTDIR/prefix/bin/exec
  Installing DESTDIR/prefix/man/a-man-page-with-no-ext
  Installing DESTDIR/prefix/man/man1/a-man-page.1
  Installing DESTDIR/prefix/man/man3/another-man-page.3
  Removing (if it exists) DESTDIR/prefix/lib/foo/META
  Creating directory DESTDIR/prefix/lib/foo
  Copying _build/install/default/lib/foo/META to DESTDIR/prefix/lib/foo/META (executable: false)
  Removing (if it exists) DESTDIR/prefix/lib/foo/dune-package
  Creating directory DESTDIR/prefix/lib/foo
  Copying _build/install/default/lib/foo/dune-package to DESTDIR/prefix/lib/foo/dune-package (executable: false)
  Removing (if it exists) DESTDIR/prefix/lib/foo/foo.cmi
  Creating directory DESTDIR/prefix/lib/foo
  Copying _build/install/default/lib/foo/foo.cmi to DESTDIR/prefix/lib/foo/foo.cmi (executable: false)
  Removing (if it exists) DESTDIR/prefix/lib/foo/foo.cmt
  Creating directory DESTDIR/prefix/lib/foo
  Copying _build/install/default/lib/foo/foo.cmt to DESTDIR/prefix/lib/foo/foo.cmt (executable: false)
  Removing (if it exists) DESTDIR/prefix/lib/foo/foo.cmx
  Creating directory DESTDIR/prefix/lib/foo
  Copying _build/install/default/lib/foo/foo.cmx to DESTDIR/prefix/lib/foo/foo.cmx (executable: false)
  Removing (if it exists) DESTDIR/prefix/lib/foo/foo.ml
  Creating directory DESTDIR/prefix/lib/foo
  Copying _build/install/default/lib/foo/foo.ml to DESTDIR/prefix/lib/foo/foo.ml (executable: false)
  Removing (if it exists) DESTDIR/prefix/lib/foo/lib$ext_lib
  Creating directory DESTDIR/prefix/lib/foo
  Copying _build/install/default/lib/foo/lib$ext_lib to DESTDIR/prefix/lib/foo/lib$ext_lib (executable: false)
  Removing (if it exists) DESTDIR/prefix/lib/foo/lib.cma
  Creating directory DESTDIR/prefix/lib/foo
  Copying _build/install/default/lib/foo/lib.cma to DESTDIR/prefix/lib/foo/lib.cma (executable: false)
  Removing (if it exists) DESTDIR/prefix/lib/foo/lib.cmxa
  Creating directory DESTDIR/prefix/lib/foo
  Copying _build/install/default/lib/foo/lib.cmxa to DESTDIR/prefix/lib/foo/lib.cmxa (executable: false)
  Removing (if it exists) DESTDIR/prefix/lib/foo/lib.cmxs
  Creating directory DESTDIR/prefix/lib/foo
  Copying _build/install/default/lib/foo/lib.cmxs to DESTDIR/prefix/lib/foo/lib.cmxs (executable: false)
  Removing (if it exists) DESTDIR/prefix/lib/foo/opam
  Creating directory DESTDIR/prefix/lib/foo
  Copying _build/install/default/lib/foo/opam to DESTDIR/prefix/lib/foo/opam (executable: false)
  Removing (if it exists) DESTDIR/prefix/bin/exec
  Creating directory DESTDIR/prefix/bin
  Copying _build/install/default/bin/exec to DESTDIR/prefix/bin/exec (executable: true)
  Removing (if it exists) DESTDIR/prefix/man/a-man-page-with-no-ext
  Creating directory DESTDIR/prefix/man
  Copying _build/install/default/man/a-man-page-with-no-ext to DESTDIR/prefix/man/a-man-page-with-no-ext (executable: false)
  Removing (if it exists) DESTDIR/prefix/man/man1/a-man-page.1
  Creating directory DESTDIR/prefix/man/man1
  Copying _build/install/default/man/man1/a-man-page.1 to DESTDIR/prefix/man/man1/a-man-page.1 (executable: false)
  Removing (if it exists) DESTDIR/prefix/man/man3/another-man-page.3
  Creating directory DESTDIR/prefix/man/man3
  Copying _build/install/default/man/man3/another-man-page.3 to DESTDIR/prefix/man/man3/another-man-page.3 (executable: false)

DESTDIR can also be passed as a command line flag.

  $ dune install --destdir DESTDIR --prefix prefix --dry-run
  Installing DESTDIR/prefix/lib/foo/META
  Installing DESTDIR/prefix/lib/foo/dune-package
  Installing DESTDIR/prefix/lib/foo/foo.cmi
  Installing DESTDIR/prefix/lib/foo/foo.cmt
  Installing DESTDIR/prefix/lib/foo/foo.cmx
  Installing DESTDIR/prefix/lib/foo/foo.ml
  Installing DESTDIR/prefix/lib/foo/lib$ext_lib
  Installing DESTDIR/prefix/lib/foo/lib.cma
  Installing DESTDIR/prefix/lib/foo/lib.cmxa
  Installing DESTDIR/prefix/lib/foo/lib.cmxs
  Installing DESTDIR/prefix/lib/foo/opam
  Installing DESTDIR/prefix/bin/exec
  Installing DESTDIR/prefix/man/a-man-page-with-no-ext
  Installing DESTDIR/prefix/man/man1/a-man-page.1
  Installing DESTDIR/prefix/man/man3/another-man-page.3
  Removing (if it exists) DESTDIR/prefix/lib/foo/META
  Creating directory DESTDIR/prefix/lib/foo
  Copying _build/install/default/lib/foo/META to DESTDIR/prefix/lib/foo/META (executable: false)
  Removing (if it exists) DESTDIR/prefix/lib/foo/dune-package
  Creating directory DESTDIR/prefix/lib/foo
  Copying _build/install/default/lib/foo/dune-package to DESTDIR/prefix/lib/foo/dune-package (executable: false)
  Removing (if it exists) DESTDIR/prefix/lib/foo/foo.cmi
  Creating directory DESTDIR/prefix/lib/foo
  Copying _build/install/default/lib/foo/foo.cmi to DESTDIR/prefix/lib/foo/foo.cmi (executable: false)
  Removing (if it exists) DESTDIR/prefix/lib/foo/foo.cmt
  Creating directory DESTDIR/prefix/lib/foo
  Copying _build/install/default/lib/foo/foo.cmt to DESTDIR/prefix/lib/foo/foo.cmt (executable: false)
  Removing (if it exists) DESTDIR/prefix/lib/foo/foo.cmx
  Creating directory DESTDIR/prefix/lib/foo
  Copying _build/install/default/lib/foo/foo.cmx to DESTDIR/prefix/lib/foo/foo.cmx (executable: false)
  Removing (if it exists) DESTDIR/prefix/lib/foo/foo.ml
  Creating directory DESTDIR/prefix/lib/foo
  Copying _build/install/default/lib/foo/foo.ml to DESTDIR/prefix/lib/foo/foo.ml (executable: false)
  Removing (if it exists) DESTDIR/prefix/lib/foo/lib$ext_lib
  Creating directory DESTDIR/prefix/lib/foo
  Copying _build/install/default/lib/foo/lib$ext_lib to DESTDIR/prefix/lib/foo/lib$ext_lib (executable: false)
  Removing (if it exists) DESTDIR/prefix/lib/foo/lib.cma
  Creating directory DESTDIR/prefix/lib/foo
  Copying _build/install/default/lib/foo/lib.cma to DESTDIR/prefix/lib/foo/lib.cma (executable: false)
  Removing (if it exists) DESTDIR/prefix/lib/foo/lib.cmxa
  Creating directory DESTDIR/prefix/lib/foo
  Copying _build/install/default/lib/foo/lib.cmxa to DESTDIR/prefix/lib/foo/lib.cmxa (executable: false)
  Removing (if it exists) DESTDIR/prefix/lib/foo/lib.cmxs
  Creating directory DESTDIR/prefix/lib/foo
  Copying _build/install/default/lib/foo/lib.cmxs to DESTDIR/prefix/lib/foo/lib.cmxs (executable: false)
  Removing (if it exists) DESTDIR/prefix/lib/foo/opam
  Creating directory DESTDIR/prefix/lib/foo
  Copying _build/install/default/lib/foo/opam to DESTDIR/prefix/lib/foo/opam (executable: false)
  Removing (if it exists) DESTDIR/prefix/bin/exec
  Creating directory DESTDIR/prefix/bin
  Copying _build/install/default/bin/exec to DESTDIR/prefix/bin/exec (executable: true)
  Removing (if it exists) DESTDIR/prefix/man/a-man-page-with-no-ext
  Creating directory DESTDIR/prefix/man
  Copying _build/install/default/man/a-man-page-with-no-ext to DESTDIR/prefix/man/a-man-page-with-no-ext (executable: false)
  Removing (if it exists) DESTDIR/prefix/man/man1/a-man-page.1
  Creating directory DESTDIR/prefix/man/man1
  Copying _build/install/default/man/man1/a-man-page.1 to DESTDIR/prefix/man/man1/a-man-page.1 (executable: false)
  Removing (if it exists) DESTDIR/prefix/man/man3/another-man-page.3
  Creating directory DESTDIR/prefix/man/man3
  Copying _build/install/default/man/man3/another-man-page.3 to DESTDIR/prefix/man/man3/another-man-page.3 (executable: false)
