(alias
 (name action-modifying-a-dependency)
 (deps (package dune) (source_tree test-cases/action-modifying-a-dependency))
 (action
  (chdir
   test-cases/action-modifying-a-dependency
   (progn (run %{exe:cram.exe} -test run.t) (diff? run.t run.t.corrected)))))

(alias
 (name aliases)
 (deps (package dune) (source_tree test-cases/aliases))
 (action
  (chdir
   test-cases/aliases
   (progn (run %{exe:cram.exe} -test run.t) (diff? run.t run.t.corrected)))))

(alias
 (name all-alias)
 (deps (package dune) (source_tree test-cases/all-alias))
 (action
  (chdir
   test-cases/all-alias
   (progn (run %{exe:cram.exe} -test run.t) (diff? run.t run.t.corrected)))))

(alias
 (name allow_approximate_merlin)
 (deps (package dune) (source_tree test-cases/allow_approximate_merlin))
 (action
  (chdir
   test-cases/allow_approximate_merlin
   (progn (run %{exe:cram.exe} -test run.t) (diff? run.t run.t.corrected)))))

(alias
 (name bad-alias-error)
 (deps (package dune) (source_tree test-cases/bad-alias-error))
 (action
  (chdir
   test-cases/bad-alias-error
   (progn (run %{exe:cram.exe} -test run.t) (diff? run.t run.t.corrected)))))

(alias
 (name block-strings)
 (deps (package dune) (source_tree test-cases/block-strings))
 (action
  (chdir
   test-cases/block-strings
   (progn (run %{exe:cram.exe} -test run.t) (diff? run.t run.t.corrected)))))

(alias
 (name byte-code-only)
 (deps (package dune) (source_tree test-cases/byte-code-only))
 (action
  (chdir
   test-cases/byte-code-only
   (progn (run %{exe:cram.exe} -test run.t) (diff? run.t run.t.corrected)))))

(alias
 (name c-stubs)
 (deps (package dune) (source_tree test-cases/c-stubs))
 (action
  (chdir
   test-cases/c-stubs
   (progn (run %{exe:cram.exe} -test run.t) (diff? run.t run.t.corrected)))))

(alias
 (name check-alias)
 (deps (package dune) (source_tree test-cases/check-alias))
 (action
  (chdir
   test-cases/check-alias
   (progn (run %{exe:cram.exe} -test run.t) (diff? run.t run.t.corrected)))))

(alias
 (name cinaps)
 (deps (package dune) (source_tree test-cases/cinaps))
 (action
  (chdir
   test-cases/cinaps
   (progn (run %{exe:cram.exe} -test run.t) (diff? run.t run.t.corrected)))))

(alias
 (name configurator)
 (deps (package dune) (source_tree test-cases/configurator))
 (action
  (chdir
   test-cases/configurator
   (progn (run %{exe:cram.exe} -test run.t) (diff? run.t run.t.corrected))))
 (enabled_if (<> %{ocaml-config:system} win)))

(alias
 (name configurator-gh1166)
 (deps (package dune) (source_tree test-cases/configurator-gh1166))
 (action
  (chdir
   test-cases/configurator-gh1166
   (progn (run %{exe:cram.exe} -test run.t) (diff? run.t run.t.corrected)))))

(alias
 (name contents-depends-on-glob)
 (deps (package dune) (source_tree test-cases/contents-depends-on-glob))
 (action
  (chdir
   test-cases/contents-depends-on-glob
   (progn (run %{exe:cram.exe} -test run.t) (diff? run.t run.t.corrected)))))

(alias
 (name copy-files-non-sub-dir-error)
 (deps (package dune) (source_tree test-cases/copy-files-non-sub-dir-error))
 (action
  (chdir
   test-cases/copy-files-non-sub-dir-error
   (progn (run %{exe:cram.exe} -test run.t) (diff? run.t run.t.corrected)))))

(alias
 (name copy_files)
 (deps (package dune) (source_tree test-cases/copy_files))
 (action
  (chdir
   test-cases/copy_files
   (progn (run %{exe:cram.exe} -test run.t) (diff? run.t run.t.corrected)))))

(alias
 (name coq)
 (deps (package dune) (source_tree test-cases/coq))
 (action
  (chdir
   test-cases/coq
   (progn (run %{exe:cram.exe} -test run.t) (diff? run.t run.t.corrected)))))

(alias
 (name cross-compilation)
 (deps (package dune) (source_tree test-cases/cross-compilation))
 (action
  (chdir
   test-cases/cross-compilation
   (progn (run %{exe:cram.exe} -test run.t) (diff? run.t run.t.corrected)))))

(alias
 (name custom-build-dir)
 (deps (package dune) (source_tree test-cases/custom-build-dir))
 (action
  (chdir
   test-cases/custom-build-dir
   (progn (run %{exe:cram.exe} -test run.t) (diff? run.t run.t.corrected)))))

(alias
 (name custom-cross-compilation)
 (deps (package dune) (source_tree test-cases/custom-cross-compilation))
 (action
  (chdir
   test-cases/custom-cross-compilation
   (progn (run %{exe:cram.exe} -test run.t) (diff? run.t run.t.corrected)))))

(alias
 (name cxx-extension)
 (deps (package dune) (source_tree test-cases/cxx-extension))
 (action
  (chdir
   test-cases/cxx-extension
   (progn (run %{exe:cram.exe} -test run.t) (diff? run.t run.t.corrected)))))

(alias
 (name default-targets)
 (deps (package dune) (source_tree test-cases/default-targets))
 (action
  (chdir
   test-cases/default-targets
   (progn (run %{exe:cram.exe} -test run.t) (diff? run.t run.t.corrected)))))

(alias
 (name dep-on-dir-that-does-not-exist)
 (deps (package dune) (source_tree test-cases/dep-on-dir-that-does-not-exist))
 (action
  (chdir
   test-cases/dep-on-dir-that-does-not-exist
   (progn (run %{exe:cram.exe} -test run.t) (diff? run.t run.t.corrected)))))

(alias
 (name dep-vars)
 (deps (package dune) (source_tree test-cases/dep-vars))
 (action
  (chdir
   test-cases/dep-vars
   (progn (run %{exe:cram.exe} -test run.t) (diff? run.t run.t.corrected)))))

(alias
 (name depend-on-the-universe)
 (deps (package dune) (source_tree test-cases/depend-on-the-universe))
 (action
  (chdir
   test-cases/depend-on-the-universe
   (progn (run %{exe:cram.exe} -test run.t) (diff? run.t run.t.corrected)))))

(alias
 (name deps-conf-vars)
 (deps (package dune) (source_tree test-cases/deps-conf-vars))
 (action
  (chdir
   test-cases/deps-conf-vars
   (progn (run %{exe:cram.exe} -test run.t) (diff? run.t run.t.corrected)))))

(alias
 (name describe-project)
 (deps (package dune) (source_tree test-cases/describe-project))
 (action
  (chdir
   test-cases/describe-project
   (progn (run %{exe:cram.exe} -test run.t) (diff? run.t run.t.corrected)))))

(alias
 (name dev-flag-1103)
 (deps (package dune) (source_tree test-cases/dev-flag-1103))
 (action
  (chdir
   test-cases/dev-flag-1103
   (progn (run %{exe:cram.exe} -test run.t) (diff? run.t run.t.corrected)))))

(alias
 (name dialects)
 (deps (package dune) (source_tree test-cases/dialects))
 (action
  (chdir
   test-cases/dialects
   (progn (run %{exe:cram.exe} -test run.t) (diff? run.t run.t.corrected)))))

(alias
 (name dir-target-dep)
 (deps (package dune) (source_tree test-cases/dir-target-dep))
 (action
  (chdir
   test-cases/dir-target-dep
   (progn (run %{exe:cram.exe} -test run.t) (diff? run.t run.t.corrected)))))

(alias
 (name double-echo)
 (deps (package dune) (source_tree test-cases/double-echo))
 (action
  (chdir
   test-cases/double-echo
   (progn (run %{exe:cram.exe} -test run.t) (diff? run.t run.t.corrected)))))

(alias
 (name dune-build-dir-exec-1101)
 (deps (package dune) (source_tree test-cases/dune-build-dir-exec-1101))
 (action
  (chdir
   test-cases/dune-build-dir-exec-1101
   (progn (run %{exe:cram.exe} -test run.t) (diff? run.t run.t.corrected)))))

(alias
 (name dune-init)
 (deps (package dune) (source_tree test-cases/dune-init))
 (action
  (chdir
   test-cases/dune-init
   (progn (run %{exe:cram.exe} -test run.t) (diff? run.t run.t.corrected)))))

(alias
 (name dune-jbuild-var-case)
 (deps (package dune) (source_tree test-cases/dune-jbuild-var-case))
 (action
  (chdir
   test-cases/dune-jbuild-var-case
   (progn (run %{exe:cram.exe} -test run.t) (diff? run.t run.t.corrected)))))

(alias
 (name dune-package)
 (deps (package dune) (source_tree test-cases/dune-package))
 (action
  (chdir
   test-cases/dune-package
   (progn (run %{exe:cram.exe} -test run.t) (diff? run.t run.t.corrected)))))

(alias
 (name dune-ppx-driver-system)
 (deps (package dune) (source_tree test-cases/dune-ppx-driver-system))
 (action
  (chdir
   test-cases/dune-ppx-driver-system
   (progn (run %{exe:cram.exe} -test run.t) (diff? run.t run.t.corrected)))))

(alias
 (name dune-project-edition)
 (deps (package dune) (source_tree test-cases/dune-project-edition))
 (action
  (chdir
   test-cases/dune-project-edition
   (progn (run %{exe:cram.exe} -test run.t) (diff? run.t run.t.corrected)))))

(alias
 (name dune-project-meta)
 (deps (package dune) (source_tree test-cases/dune-project-meta))
 (action
  (chdir
   test-cases/dune-project-meta
   (progn (run %{exe:cram.exe} -test run.t) (diff? run.t run.t.corrected)))))

(alias
 (name dune-project-no-opam)
 (deps (package dune) (source_tree test-cases/dune-project-no-opam))
 (action
  (chdir
   test-cases/dune-project-no-opam
   (progn (run %{exe:cram.exe} -test run.t) (diff? run.t run.t.corrected)))))

(alias
 (name dup-fields)
 (deps (package dune) (source_tree test-cases/dup-fields))
 (action
  (chdir
   test-cases/dup-fields
   (progn (run %{exe:cram.exe} -test run.t) (diff? run.t run.t.corrected)))))

(alias
 (name duplicate-c-cxx)
 (deps (package dune) (source_tree test-cases/duplicate-c-cxx))
 (action
  (chdir
   test-cases/duplicate-c-cxx
   (progn (run %{exe:cram.exe} -test run.t) (diff? run.t run.t.corrected)))))

(alias
 (name duplicate-c-cxx-obj)
 (deps (package dune) (source_tree test-cases/duplicate-c-cxx-obj))
 (action
  (chdir
   test-cases/duplicate-c-cxx-obj
   (progn (run %{exe:cram.exe} -test run.t) (diff? run.t run.t.corrected)))))

(alias
 (name duplicate-project-names)
 (deps (package dune) (source_tree test-cases/duplicate-project-names))
 (action
  (chdir
   test-cases/duplicate-project-names
   (progn (run %{exe:cram.exe} -test run.t) (diff? run.t run.t.corrected)))))

(alias
 (name duplicate-target-no-loc)
 (deps (package dune) (source_tree test-cases/duplicate-target-no-loc))
 (action
  (chdir
   test-cases/duplicate-target-no-loc
   (progn (run %{exe:cram.exe} -test run.t) (diff? run.t run.t.corrected)))))

(alias
 (name embed-jbuild)
 (deps (package dune) (source_tree test-cases/embed-jbuild))
 (action
  (chdir
   test-cases/embed-jbuild
   (progn (run %{exe:cram.exe} -test run.t) (diff? run.t run.t.corrected)))))

(alias
 (name enabled_if)
 (deps (package dune) (source_tree test-cases/enabled_if))
 (action
  (chdir
   test-cases/enabled_if
   (progn (run %{exe:cram.exe} -test run.t) (diff? run.t run.t.corrected)))))

(alias
 (name env)
 (deps (package dune) (source_tree test-cases/env))
 (action
  (chdir
   test-cases/env
   (progn
    (run %{exe:cram.exe} -skip-versions <4.06.0 -test run.t)
    (diff? run.t run.t.corrected)))))

(alias
 (name env-and-flags-include)
 (deps (package dune) (source_tree test-cases/env-and-flags-include))
 (action
  (chdir
   test-cases/env-and-flags-include
   (progn (run %{exe:cram.exe} -test run.t) (diff? run.t run.t.corrected)))))

(alias
 (name env-bin-pform)
 (deps (package dune) (source_tree test-cases/env-bin-pform))
 (action
  (chdir
   test-cases/env-bin-pform
   (progn (run %{exe:cram.exe} -test run.t) (diff? run.t run.t.corrected)))))

(alias
 (name env-bins)
 (deps (package dune) (source_tree test-cases/env-bins))
 (action
  (chdir
   test-cases/env-bins
   (progn (run %{exe:cram.exe} -test run.t) (diff? run.t run.t.corrected)))))

(alias
 (name env-cflags)
 (deps (package dune) (source_tree test-cases/env-cflags))
 (action
  (chdir
   test-cases/env-cflags
   (progn
    (run %{exe:cram.exe} -skip-versions <4.06.0 -test run.t)
    (diff? run.t run.t.corrected)))))

(alias
 (name env-dune-file)
 (deps (package dune) (source_tree test-cases/env-dune-file))
 (action
  (chdir
   test-cases/env-dune-file
   (progn (run %{exe:cram.exe} -test run.t) (diff? run.t run.t.corrected)))))

(alias
 (name env-tracking)
 (deps (package dune) (source_tree test-cases/env-tracking))
 (action
  (chdir
   test-cases/env-tracking
   (progn (run %{exe:cram.exe} -test run.t) (diff? run.t run.t.corrected)))))

(alias
 (name env-var-expansion)
 (deps (package dune) (source_tree test-cases/env-var-expansion))
 (action
  (chdir
   test-cases/env-var-expansion
   (progn (run %{exe:cram.exe} -test run.t) (diff? run.t run.t.corrected)))))

(alias
 (name env-variables)
 (deps (package dune) (source_tree test-cases/env-variables))
 (action
  (chdir
   test-cases/env-variables
   (progn (run %{exe:cram.exe} -test run.t) (diff? run.t run.t.corrected)))))

(alias
 (name envs-and-contexts)
 (deps (package dune) (source_tree test-cases/envs-and-contexts))
 (action
  (chdir
   test-cases/envs-and-contexts
   (progn (run %{exe:cram.exe} -test run.t) (diff? run.t run.t.corrected)))))

(alias
 (name exclude-missing-module)
 (deps (package dune) (source_tree test-cases/exclude-missing-module))
 (action
  (chdir
   test-cases/exclude-missing-module
   (progn (run %{exe:cram.exe} -test run.t) (diff? run.t run.t.corrected)))))

(alias
 (name exe-name-mangle)
 (deps (package dune) (source_tree test-cases/exe-name-mangle))
 (action
  (chdir
   test-cases/exe-name-mangle
   (progn (run %{exe:cram.exe} -test run.t) (diff? run.t run.t.corrected)))))

(alias
 (name exec-cmd)
 (deps (package dune) (source_tree test-cases/exec-cmd))
 (action
  (chdir
   test-cases/exec-cmd
   (progn (run %{exe:cram.exe} -test run.t) (diff? run.t run.t.corrected)))))

(alias
 (name exec-missing)
 (deps (package dune) (source_tree test-cases/exec-missing))
 (action
  (chdir
   test-cases/exec-missing
   (progn (run %{exe:cram.exe} -test run.t) (diff? run.t run.t.corrected)))))

(alias
 (name external-lib-deps)
 (deps (package dune) (source_tree test-cases/external-lib-deps))
 (action
  (chdir
   test-cases/external-lib-deps
   (progn (run %{exe:cram.exe} -test run.t) (diff? run.t run.t.corrected)))))

(alias
 (name extra-lang-line)
 (deps (package dune) (source_tree test-cases/extra-lang-line))
 (action
  (chdir
   test-cases/extra-lang-line
   (progn (run %{exe:cram.exe} -test run.t) (diff? run.t run.t.corrected)))))

(alias
 (name fallback-dune)
 (deps (package dune) (source_tree test-cases/fallback-dune))
 (action
  (chdir
   test-cases/fallback-dune
   (progn (run %{exe:cram.exe} -test run.t) (diff? run.t run.t.corrected)))))

(alias
 (name findlib)
 (deps (package dune) (source_tree test-cases/findlib))
 (action
  (chdir
   test-cases/findlib
   (progn (run %{exe:cram.exe} -test run.t) (diff? run.t run.t.corrected)))))

(alias
 (name findlib-dynload)
 (deps (package dune) (source_tree test-cases/findlib-dynload))
 (action
  (chdir
   test-cases/findlib-dynload
   (progn (run %{exe:cram.exe} -test run.t) (diff? run.t run.t.corrected)))))

(alias
 (name findlib-error)
 (deps (package dune) (source_tree test-cases/findlib-error))
 (action
  (chdir
   test-cases/findlib-error
   (progn (run %{exe:cram.exe} -test run.t) (diff? run.t run.t.corrected)))))

(alias
 (name force-test)
 (deps (package dune) (source_tree test-cases/force-test))
 (action
  (chdir
   test-cases/force-test
   (progn (run %{exe:cram.exe} -test run.t) (diff? run.t run.t.corrected)))))

(alias
 (name format-dune-file)
 (deps (package dune) (source_tree test-cases/format-dune-file))
 (action
  (chdir
   test-cases/format-dune-file
   (progn (run %{exe:cram.exe} -test run.t) (diff? run.t run.t.corrected)))))

(alias
 (name formatting)
 (deps (package dune) (source_tree test-cases/formatting))
 (action
  (chdir
   test-cases/formatting
   (progn (run %{exe:cram.exe} -test run.t) (diff? run.t run.t.corrected)))))

(alias
 (name gen-opam-install-file)
 (deps (package dune) (source_tree test-cases/gen-opam-install-file))
 (action
  (chdir
   test-cases/gen-opam-install-file
   (progn (run %{exe:cram.exe} -test run.t) (diff? run.t run.t.corrected)))))

(alias
 (name github1019)
 (deps (package dune) (source_tree test-cases/github1019))
 (action
  (chdir
   test-cases/github1019
   (progn (run %{exe:cram.exe} -test run.t) (diff? run.t run.t.corrected)))))

(alias
 (name github1099)
 (deps (package dune) (source_tree test-cases/github1099))
 (action
  (chdir
   test-cases/github1099
   (progn (run %{exe:cram.exe} -test run.t) (diff? run.t run.t.corrected)))))

(alias
 (name github1231)
 (deps (package dune) (source_tree test-cases/github1231))
 (action
  (chdir
   test-cases/github1231
   (progn (run %{exe:cram.exe} -test run.t) (diff? run.t run.t.corrected)))))

(alias
 (name github1306)
 (deps (package dune) (source_tree test-cases/github1306))
 (action
  (chdir
   test-cases/github1306
   (progn (run %{exe:cram.exe} -test run.t) (diff? run.t run.t.corrected)))))

(alias
 (name github1342)
 (deps (package dune) (source_tree test-cases/github1342))
 (action
  (chdir
   test-cases/github1342
   (progn (run %{exe:cram.exe} -test run.t) (diff? run.t run.t.corrected)))))

(alias
 (name github1372)
 (deps (package dune) (source_tree test-cases/github1372))
 (action
  (chdir
   test-cases/github1372
   (progn (run %{exe:cram.exe} -test run.t) (diff? run.t run.t.corrected)))))

(alias
 (name github1395)
 (deps (package dune) (source_tree test-cases/github1395))
 (action
  (chdir
   test-cases/github1395
   (progn (run %{exe:cram.exe} -test run.t) (diff? run.t run.t.corrected)))))

(alias
 (name github1426)
 (deps (package dune) (source_tree test-cases/github1426))
 (action
  (chdir
   test-cases/github1426
   (progn (run %{exe:cram.exe} -test run.t) (diff? run.t run.t.corrected)))))

(alias
 (name github1529)
 (deps (package dune) (source_tree test-cases/github1529))
 (action
  (chdir
   test-cases/github1529
   (progn (run %{exe:cram.exe} -test run.t) (diff? run.t run.t.corrected)))))

(alias
 (name github1549)
 (deps (package dune) (source_tree test-cases/github1549))
 (action
  (chdir
   test-cases/github1549
   (progn (run %{exe:cram.exe} -test run.t) (diff? run.t run.t.corrected)))))

(alias
 (name github1560)
 (deps (package dune) (source_tree test-cases/github1560))
 (action
  (chdir
   test-cases/github1560
   (progn (run %{exe:cram.exe} -test run.t) (diff? run.t run.t.corrected)))))

(alias
 (name github1616)
 (deps (package dune) (source_tree test-cases/github1616))
 (action
  (chdir
   test-cases/github1616
   (progn (run %{exe:cram.exe} -test run.t) (diff? run.t run.t.corrected)))))

(alias
 (name github1811)
 (deps (package dune) (source_tree test-cases/github1811))
 (action
  (chdir
   test-cases/github1811
   (progn (run %{exe:cram.exe} -test run.t) (diff? run.t run.t.corrected)))))

(alias
 (name github1856)
 (deps (package dune) (source_tree test-cases/github1856))
 (action
  (chdir
   test-cases/github1856
   (progn (run %{exe:cram.exe} -test run.t) (diff? run.t run.t.corrected)))))

(alias
 (name github1946)
 (deps (package dune) (source_tree test-cases/github1946))
 (action
  (chdir
   test-cases/github1946
   (progn (run %{exe:cram.exe} -test run.t) (diff? run.t run.t.corrected)))))

(alias
 (name github20)
 (deps (package dune) (source_tree test-cases/github20))
 (action
  (chdir
   test-cases/github20
   (progn (run %{exe:cram.exe} -test run.t) (diff? run.t run.t.corrected)))))

(alias
 (name github2033)
 (deps (package dune) (source_tree test-cases/github2033))
 (action
  (chdir
   test-cases/github2033
   (progn (run %{exe:cram.exe} -test run.t) (diff? run.t run.t.corrected)))))

(alias
 (name github2061)
 (deps (package dune) (source_tree test-cases/github2061))
 (action
  (chdir
   test-cases/github2061
   (progn (run %{exe:cram.exe} -test run.t) (diff? run.t run.t.corrected)))))

(alias
 (name github2123)
 (deps (package dune) (source_tree test-cases/github2123))
 (action
  (chdir
   test-cases/github2123
   (progn (run %{exe:cram.exe} -test run.t) (diff? run.t run.t.corrected)))))

(alias
 (name github2206)
 (deps (package dune) (source_tree test-cases/github2206))
 (action
  (chdir
   test-cases/github2206
   (progn (run %{exe:cram.exe} -test run.t) (diff? run.t run.t.corrected)))))

(alias
 (name github2228)
 (deps (package dune) (source_tree test-cases/github2228))
 (action
  (chdir
   test-cases/github2228
   (progn (run %{exe:cram.exe} -test run.t) (diff? run.t run.t.corrected)))))

(alias
 (name github2272)
 (deps (package dune) (source_tree test-cases/github2272))
 (action
  (chdir
   test-cases/github2272
   (progn (run %{exe:cram.exe} -test run.t) (diff? run.t run.t.corrected)))))

(alias
 (name github24)
 (deps (package dune) (source_tree test-cases/github24))
 (action
  (chdir
   test-cases/github24
   (progn (run %{exe:cram.exe} -test run.t) (diff? run.t run.t.corrected)))))

(alias
 (name github25)
 (deps (package dune) (source_tree test-cases/github25))
 (action
  (setenv
   OCAMLPATH
   ./findlib-packages
   (chdir
    test-cases/github25
    (progn (run %{exe:cram.exe} -test run.t) (diff? run.t run.t.corrected))))))

(alias
 (name github534)
 (deps (package dune) (source_tree test-cases/github534))
 (action
  (chdir
   test-cases/github534
   (progn (run %{exe:cram.exe} -test run.t) (diff? run.t run.t.corrected)))))

(alias
 (name github568)
 (deps (package dune) (source_tree test-cases/github568))
 (action
  (chdir
   test-cases/github568
   (progn (run %{exe:cram.exe} -test run.t) (diff? run.t run.t.corrected)))))

(alias
 (name github597)
 (deps (package dune) (source_tree test-cases/github597))
 (action
  (chdir
   test-cases/github597
   (progn (run %{exe:cram.exe} -test run.t) (diff? run.t run.t.corrected)))))

(alias
 (name github644)
 (deps (package dune) (source_tree test-cases/github644))
 (action
  (chdir
   test-cases/github644
   (progn (run %{exe:cram.exe} -test run.t) (diff? run.t run.t.corrected)))))

(alias
 (name github660)
 (deps (package dune) (source_tree test-cases/github660))
 (action
  (chdir
   test-cases/github660
   (progn (run %{exe:cram.exe} -test run.t) (diff? run.t run.t.corrected)))))

(alias
 (name github717-odoc-index)
 (deps (package dune) (source_tree test-cases/github717-odoc-index))
 (action
  (chdir
   test-cases/github717-odoc-index
   (progn
    (run %{exe:cram.exe} -skip-versions 4.02.3 -test run.t)
    (diff? run.t run.t.corrected)))))

(alias
 (name github734)
 (deps (package dune) (source_tree test-cases/github734))
 (action
  (chdir
   test-cases/github734
   (progn (run %{exe:cram.exe} -test run.t) (diff? run.t run.t.corrected)))))

(alias
 (name github759)
 (deps (package dune) (source_tree test-cases/github759))
 (action
  (chdir
   test-cases/github759
   (progn (run %{exe:cram.exe} -test run.t) (diff? run.t run.t.corrected)))))

(alias
 (name github761)
 (deps (package dune) (source_tree test-cases/github761))
 (action
  (chdir
   test-cases/github761
   (progn (run %{exe:cram.exe} -test run.t) (diff? run.t run.t.corrected)))))

(alias
 (name github764)
 (deps (package dune) (source_tree test-cases/github764))
 (action
  (chdir
   test-cases/github764
   (progn (run %{exe:cram.exe} -test run.t) (diff? run.t run.t.corrected))))
 (enabled_if (<> %{ocaml-config:system} win)))

(alias
 (name github784)
 (deps (package dune) (source_tree test-cases/github784))
 (action
  (chdir
   test-cases/github784
   (progn (run %{exe:cram.exe} -test run.t) (diff? run.t run.t.corrected)))))

(alias
 (name github992)
 (deps (package dune) (source_tree test-cases/github992))
 (action
  (chdir
   test-cases/github992
   (progn (run %{exe:cram.exe} -test run.t) (diff? run.t run.t.corrected)))))

(alias
 (name glob-deps)
 (deps (package dune) (source_tree test-cases/glob-deps))
 (action
  (chdir
   test-cases/glob-deps
   (progn (run %{exe:cram.exe} -test run.t) (diff? run.t run.t.corrected)))))

(alias
 (name ignored_subdirs)
 (deps (package dune) (source_tree test-cases/ignored_subdirs))
 (action
  (chdir
   test-cases/ignored_subdirs
   (progn (run %{exe:cram.exe} -test run.t) (diff? run.t run.t.corrected)))))

(alias
 (name include-loop)
 (deps (package dune) (source_tree test-cases/include-loop))
 (action
  (chdir
   test-cases/include-loop
   (progn (run %{exe:cram.exe} -test run.t) (diff? run.t run.t.corrected)))))

(alias
 (name include-qualified)
 (deps (package dune) (source_tree test-cases/include-qualified))
 (action
  (chdir
   test-cases/include-qualified
   (progn (run %{exe:cram.exe} -test run.t) (diff? run.t run.t.corrected)))))

(alias
 (name inline_tests)
 (deps (package dune) (source_tree test-cases/inline_tests))
 (action
  (chdir
   test-cases/inline_tests
   (progn (run %{exe:cram.exe} -test run.t) (diff? run.t run.t.corrected)))))

(alias
 (name install-dry-run)
 (deps (package dune) (source_tree test-cases/install-dry-run))
 (action
  (chdir
   test-cases/install-dry-run
   (progn (run %{exe:cram.exe} -test run.t) (diff? run.t run.t.corrected)))))

(alias
 (name install-libdir)
 (deps (package dune) (source_tree test-cases/install-libdir))
 (action
  (chdir
   test-cases/install-libdir
   (progn (run %{exe:cram.exe} -test run.t) (diff? run.t run.t.corrected)))))

(alias
 (name install-multiple-contexts)
 (deps (package dune) (source_tree test-cases/install-multiple-contexts))
 (action
  (chdir
   test-cases/install-multiple-contexts
   (progn (run %{exe:cram.exe} -test run.t) (diff? run.t run.t.corrected)))))

(alias
 (name install-partial-package)
 (deps (package dune) (source_tree test-cases/install-partial-package))
 (action
  (chdir
   test-cases/install-partial-package
   (progn (run %{exe:cram.exe} -test run.t) (diff? run.t run.t.corrected)))))

(alias
 (name install-rule-order)
 (deps (package dune) (source_tree test-cases/install-rule-order))
 (action
  (chdir
   test-cases/install-rule-order
   (progn (run %{exe:cram.exe} -test run.t) (diff? run.t run.t.corrected)))))

(alias
 (name install-with-var)
 (deps (package dune) (source_tree test-cases/install-with-var))
 (action
  (chdir
   test-cases/install-with-var
   (progn (run %{exe:cram.exe} -test run.t) (diff? run.t run.t.corrected)))))

(alias
 (name installable-dup-private-libs)
 (deps (package dune) (source_tree test-cases/installable-dup-private-libs))
 (action
  (chdir
   test-cases/installable-dup-private-libs
   (progn (run %{exe:cram.exe} -test run.t) (diff? run.t run.t.corrected)))))

(alias
 (name intf-only)
 (deps (package dune) (source_tree test-cases/intf-only))
 (action
  (chdir
   test-cases/intf-only
   (progn (run %{exe:cram.exe} -test run.t) (diff? run.t run.t.corrected)))))

(alias
 (name js_of_ocaml)
 (deps (package dune) (source_tree test-cases/js_of_ocaml))
 (action
  (setenv
   NODE
   %{bin:node}
   (chdir
    test-cases/js_of_ocaml
    (progn (run %{exe:cram.exe} -test run.t) (diff? run.t run.t.corrected))))))

(alias
 (name lib-available)
 (deps (package dune) (source_tree test-cases/lib-available))
 (action
  (chdir
   test-cases/lib-available
   (progn (run %{exe:cram.exe} -test run.t) (diff? run.t run.t.corrected)))))

(alias
 (name lib-errors)
 (deps (package dune) (source_tree test-cases/lib-errors))
 (action
  (chdir
   test-cases/lib-errors
   (progn (run %{exe:cram.exe} -test run.t) (diff? run.t run.t.corrected)))))

(alias
 (name link-deps)
 (deps (package dune) (source_tree test-cases/link-deps))
 (action
  (chdir
   test-cases/link-deps
   (progn (run %{exe:cram.exe} -test run.t) (diff? run.t run.t.corrected)))))

(alias
 (name lint)
 (deps (package dune) (source_tree test-cases/lint))
 (action
  (chdir
   test-cases/lint
   (progn (run %{exe:cram.exe} -test run.t) (diff? run.t run.t.corrected)))))

(alias
 (name loop)
 (deps (package dune) (source_tree test-cases/loop))
 (action
  (chdir
   test-cases/loop
   (progn (run %{exe:cram.exe} -test run.t) (diff? run.t run.t.corrected)))))

(alias
 (name macro-expand-error)
 (deps (package dune) (source_tree test-cases/macro-expand-error))
 (action
  (chdir
   test-cases/macro-expand-error
   (progn (run %{exe:cram.exe} -test run.t) (diff? run.t run.t.corrected)))))

(alias
 (name menhir)
 (deps (package dune) (source_tree test-cases/menhir))
 (action
  (chdir
   test-cases/menhir
   (progn (run %{exe:cram.exe} -test run.t) (diff? run.t run.t.corrected)))))

(alias
 (name merlin-tests)
 (deps (package dune) (source_tree test-cases/merlin-tests))
 (action
  (chdir
   test-cases/merlin-tests
   (progn (run %{exe:cram.exe} -test run.t) (diff? run.t run.t.corrected)))))

(alias
 (name meta-gen)
 (deps (package dune) (source_tree test-cases/meta-gen))
 (action
  (chdir
   test-cases/meta-gen
   (progn (run %{exe:cram.exe} -test run.t) (diff? run.t run.t.corrected)))))

(alias
 (name misc)
 (deps (package dune) (source_tree test-cases/misc))
 (action
  (chdir
   test-cases/misc
   (progn (run %{exe:cram.exe} -test run.t) (diff? run.t run.t.corrected)))))

(alias
 (name missing-loc-run)
 (deps (package dune) (source_tree test-cases/missing-loc-run))
 (action
  (chdir
   test-cases/missing-loc-run
   (progn (run %{exe:cram.exe} -test run.t) (diff? run.t run.t.corrected)))))

(alias
 (name multi-dir)
 (deps (package dune) (source_tree test-cases/multi-dir))
 (action
  (chdir
   test-cases/multi-dir
   (progn (run %{exe:cram.exe} -test run.t) (diff? run.t run.t.corrected)))))

(alias
 (name multiple-private-libs)
 (deps (package dune) (source_tree test-cases/multiple-private-libs))
 (action
  (chdir
   test-cases/multiple-private-libs
   (progn
    (run %{exe:cram.exe} -skip-versions 4.02.3 -test run.t)
    (diff? run.t run.t.corrected)))))

(alias
 (name multiple-targets)
 (deps (package dune) (source_tree test-cases/multiple-targets))
 (action
  (chdir
   test-cases/multiple-targets
   (progn (run %{exe:cram.exe} -test run.t) (diff? run.t run.t.corrected)))))

(alias
 (name name-field-validation)
 (deps (package dune) (source_tree test-cases/name-field-validation))
 (action
  (chdir
   test-cases/name-field-validation
   (progn (run %{exe:cram.exe} -test run.t) (diff? run.t run.t.corrected)))))

(alias
 (name no-installable-mode)
 (deps (package dune) (source_tree test-cases/no-installable-mode))
 (action
  (chdir
   test-cases/no-installable-mode
   (progn (run %{exe:cram.exe} -test run.t) (diff? run.t run.t.corrected)))))

(alias
 (name no-name-field)
 (deps (package dune) (source_tree test-cases/no-name-field))
 (action
  (chdir
   test-cases/no-name-field
   (progn (run %{exe:cram.exe} -test run.t) (diff? run.t run.t.corrected)))))

(alias
 (name null-dep)
 (deps (package dune) (source_tree test-cases/null-dep))
 (action
  (chdir
   test-cases/null-dep
   (progn (run %{exe:cram.exe} -test run.t) (diff? run.t run.t.corrected)))))

(alias
 (name ocaml-config-macro)
 (deps (package dune) (source_tree test-cases/ocaml-config-macro))
 (action
  (chdir
   test-cases/ocaml-config-macro
   (progn (run %{exe:cram.exe} -test run.t) (diff? run.t run.t.corrected)))))

(alias
 (name ocaml-syntax)
 (deps (package dune) (source_tree test-cases/ocaml-syntax))
 (action
  (chdir
   test-cases/ocaml-syntax
   (progn (run %{exe:cram.exe} -test run.t) (diff? run.t run.t.corrected)))))

(alias
 (name ocamldep-multi-stanzas)
 (deps (package dune) (source_tree test-cases/ocamldep-multi-stanzas))
 (action
  (chdir
   test-cases/ocamldep-multi-stanzas
   (progn (run %{exe:cram.exe} -test run.t) (diff? run.t run.t.corrected)))))

(alias
 (name ocamllex-jbuild)
 (deps (package dune) (source_tree test-cases/ocamllex-jbuild))
 (action
  (chdir
   test-cases/ocamllex-jbuild
   (progn (run %{exe:cram.exe} -test run.t) (diff? run.t run.t.corrected)))))

(alias
 (name odig-files-workspace)
 (deps (package dune) (source_tree test-cases/odig-files-workspace))
 (action
  (chdir
   test-cases/odig-files-workspace
   (progn (run %{exe:cram.exe} -test run.t) (diff? run.t run.t.corrected)))))

(alias
 (name odoc)
 (deps (package dune) (source_tree test-cases/odoc))
 (action
  (chdir
   test-cases/odoc
   (progn
    (run %{exe:cram.exe} -skip-versions 4.02.3 -test run.t)
    (diff? run.t run.t.corrected)))))

(alias
 (name odoc-package-mld-link)
 (deps (package dune) (source_tree test-cases/odoc-package-mld-link))
 (action
  (chdir
   test-cases/odoc-package-mld-link
   (progn
    (run %{exe:cram.exe} -skip-versions 4.02.3 -test run.t)
    (diff? run.t run.t.corrected)))))

(alias
 (name odoc-unique-mlds)
 (deps (package dune) (source_tree test-cases/odoc-unique-mlds))
 (action
  (chdir
   test-cases/odoc-unique-mlds
   (progn
    (run %{exe:cram.exe} -skip-versions 4.02.3 -test run.t)
    (diff? run.t run.t.corrected)))))

(alias
 (name old-dune-subsystem)
 (deps (package dune) (source_tree test-cases/old-dune-subsystem))
 (action
  (chdir
   test-cases/old-dune-subsystem
   (progn (run %{exe:cram.exe} -test run.t) (diff? run.t run.t.corrected)))))

(alias
 (name optional)
 (deps (package dune) (source_tree test-cases/optional))
 (action
  (chdir
   test-cases/optional
   (progn (run %{exe:cram.exe} -test run.t) (diff? run.t run.t.corrected)))))

(alias
 (name output-obj)
 (deps (package dune) (source_tree test-cases/output-obj))
 (action
  (chdir
   test-cases/output-obj
   (progn
    (run %{exe:cram.exe} -skip-versions <4.06.0 -test run.t)
    (diff? run.t run.t.corrected))))
 (enabled_if
  (and (<> %{ocaml-config:system} macosx) (<> %{ocaml-config:system} win))))

(alias
 (name package-dep)
 (deps (package dune) (source_tree test-cases/package-dep))
 (action
  (chdir
   test-cases/package-dep
   (progn (run %{exe:cram.exe} -test run.t) (diff? run.t run.t.corrected)))))

(alias
 (name path-variables)
 (deps (package dune) (source_tree test-cases/path-variables))
 (action
  (chdir
   test-cases/path-variables
   (progn (run %{exe:cram.exe} -test run.t) (diff? run.t run.t.corrected)))))

(alias
 (name pkg-config-quoting)
 (deps (package dune) (source_tree test-cases/pkg-config-quoting))
 (action
  (chdir
   test-cases/pkg-config-quoting
   (progn (run %{exe:cram.exe} -test run.t) (diff? run.t run.t.corrected)))))

(alias
 (name ppx-rewriter)
 (deps (package dune) (source_tree test-cases/ppx-rewriter))
 (action
  (chdir
   test-cases/ppx-rewriter
   (progn
    (run %{exe:cram.exe} -skip-versions 4.02.3 -test run.t)
    (diff? run.t run.t.corrected)))))

(alias
 (name preprocess-with-action)
 (deps (package dune) (source_tree test-cases/preprocess-with-action))
 (action
  (chdir
   test-cases/preprocess-with-action
   (progn (run %{exe:cram.exe} -test run.t) (diff? run.t run.t.corrected)))))

(alias
 (name private-modules)
 (deps (package dune) (source_tree test-cases/private-modules))
 (action
  (chdir
   test-cases/private-modules
   (progn (run %{exe:cram.exe} -test run.t) (diff? run.t run.t.corrected)))))

(alias
 (name private-public-overlap)
 (deps (package dune) (source_tree test-cases/private-public-overlap))
 (action
  (chdir
   test-cases/private-public-overlap
   (progn (run %{exe:cram.exe} -test run.t) (diff? run.t run.t.corrected)))))

(alias
 (name project-root)
 (deps (package dune) (source_tree test-cases/project-root))
 (action
  (chdir
   test-cases/project-root
   (progn (run %{exe:cram.exe} -test run.t) (diff? run.t run.t.corrected)))))

(alias
 (name promote)
 (deps (package dune) (source_tree test-cases/promote))
 (action
  (chdir
   test-cases/promote
   (progn (run %{exe:cram.exe} -test run.t) (diff? run.t run.t.corrected)))))

(alias
 (name quoting)
 (deps (package dune) (source_tree test-cases/quoting))
 (action
  (chdir
   test-cases/quoting
   (progn (run %{exe:cram.exe} -test run.t) (diff? run.t run.t.corrected)))))

(alias
 (name reason)
 (deps (package dune) (source_tree test-cases/reason))
 (action
  (chdir
   test-cases/reason
   (progn (run %{exe:cram.exe} -test run.t) (diff? run.t run.t.corrected)))))

(alias
 (name redirections)
 (deps (package dune) (source_tree test-cases/redirections))
 (action
  (chdir
   test-cases/redirections
   (progn (run %{exe:cram.exe} -test run.t) (diff? run.t run.t.corrected)))))

(alias
 (name reporting-of-cycles)
 (deps (package dune) (source_tree test-cases/reporting-of-cycles))
 (action
  (chdir
   test-cases/reporting-of-cycles
   (progn (run %{exe:cram.exe} -test run.t) (diff? run.t run.t.corrected)))))

(alias
 (name rule-target-external)
 (deps (package dune) (source_tree test-cases/rule-target-external))
 (action
  (chdir
   test-cases/rule-target-external
   (progn (run %{exe:cram.exe} -test run.t) (diff? run.t run.t.corrected)))))

(alias
 (name scope-bug)
 (deps (package dune) (source_tree test-cases/scope-bug))
 (action
  (chdir
   test-cases/scope-bug
   (progn (run %{exe:cram.exe} -test run.t) (diff? run.t run.t.corrected)))))

(alias
 (name scope-ppx-bug)
 (deps (package dune) (source_tree test-cases/scope-ppx-bug))
 (action
  (chdir
   test-cases/scope-ppx-bug
   (progn (run %{exe:cram.exe} -test run.t) (diff? run.t run.t.corrected)))))

(alias
 (name select)
 (deps (package dune) (source_tree test-cases/select))
 (action
  (chdir
   test-cases/select
   (progn (run %{exe:cram.exe} -test run.t) (diff? run.t run.t.corrected)))))

(alias
 (name several-packages)
 (deps (package dune) (source_tree test-cases/several-packages))
 (action
  (chdir
   test-cases/several-packages
   (progn (run %{exe:cram.exe} -test run.t) (diff? run.t run.t.corrected)))))

(alias
 (name shadow-bindings)
 (deps (package dune) (source_tree test-cases/shadow-bindings))
 (action
  (chdir
   test-cases/shadow-bindings
   (progn (run %{exe:cram.exe} -test run.t) (diff? run.t run.t.corrected)))))

(alias
 (name stale-artifact-removal)
 (deps (package dune) (source_tree test-cases/stale-artifact-removal))
 (action
  (chdir
   test-cases/stale-artifact-removal
   (progn (run %{exe:cram.exe} -test run.t) (diff? run.t run.t.corrected)))))

(alias
 (name stdlib-compilation)
 (deps (package dune) (source_tree test-cases/stdlib-compilation))
 (action
  (chdir
   test-cases/stdlib-compilation
   (progn (run %{exe:cram.exe} -test run.t) (diff? run.t run.t.corrected)))))

(alias
 (name subst)
 (deps (package dune) (source_tree test-cases/subst))
 (action
  (chdir
   test-cases/subst
   (progn (run %{exe:cram.exe} -test run.t) (diff? run.t run.t.corrected)))))

(alias
 (name syntax-versioning)
 (deps (package dune) (source_tree test-cases/syntax-versioning))
 (action
  (chdir
   test-cases/syntax-versioning
   (progn (run %{exe:cram.exe} -test run.t) (diff? run.t run.t.corrected)))))

(alias
 (name target-dir-alias)
 (deps (package dune) (source_tree test-cases/target-dir-alias))
 (action
  (chdir
   test-cases/target-dir-alias
   (progn (run %{exe:cram.exe} -test run.t) (diff? run.t run.t.corrected)))))

(alias
 (name targets-with-vars)
 (deps (package dune) (source_tree test-cases/targets-with-vars))
 (action
  (chdir
   test-cases/targets-with-vars
   (progn (run %{exe:cram.exe} -test run.t) (diff? run.t run.t.corrected)))))

(alias
 (name tests-stanza)
 (deps (package dune) (source_tree test-cases/tests-stanza))
 (action
  (chdir
   test-cases/tests-stanza
   (progn (run %{exe:cram.exe} -test run.t) (diff? run.t run.t.corrected)))))

(alias
 (name tests-stanza-action)
 (deps (package dune) (source_tree test-cases/tests-stanza-action))
 (action
  (chdir
   test-cases/tests-stanza-action
   (progn (run %{exe:cram.exe} -test run.t) (diff? run.t run.t.corrected)))))

(alias
 (name tests-stanza-action-syntax-version)
 (deps
  (package dune)
  (source_tree test-cases/tests-stanza-action-syntax-version))
 (action
  (chdir
   test-cases/tests-stanza-action-syntax-version
   (progn (run %{exe:cram.exe} -test run.t) (diff? run.t run.t.corrected)))))

(alias
 (name too-many-parens)
 (deps (package dune) (source_tree test-cases/too-many-parens))
 (action
  (chdir
   test-cases/too-many-parens
   (progn (run %{exe:cram.exe} -test run.t) (diff? run.t run.t.corrected)))))

(alias
 (name toplevel-stanza)
 (deps (package dune) (source_tree test-cases/toplevel-stanza))
 (action
  (chdir
   test-cases/toplevel-stanza
   (progn
    (run %{exe:cram.exe} -skip-versions <4.05.0 -test run.t)
    (diff? run.t run.t.corrected)))))

(alias
 (name trace-file)
 (deps (package dune) (source_tree test-cases/trace-file))
 (action
  (chdir
   test-cases/trace-file
   (progn (run %{exe:cram.exe} -test run.t) (diff? run.t run.t.corrected)))))

(alias
 (name transitive-deps-mode)
 (deps (package dune) (source_tree test-cases/transitive-deps-mode))
 (action
  (chdir
   test-cases/transitive-deps-mode
   (progn (run %{exe:cram.exe} -test run.t) (diff? run.t run.t.corrected)))))

(alias
 (name unreadable-src)
 (deps (package dune) (source_tree test-cases/unreadable-src))
 (action
  (chdir
   test-cases/unreadable-src
   (progn (run %{exe:cram.exe} -test run.t) (diff? run.t run.t.corrected)))))

(alias
 (name upgrader)
 (deps (package dune) (source_tree test-cases/upgrader))
 (action
  (chdir
   test-cases/upgrader
   (progn (run %{exe:cram.exe} -test run.t) (diff? run.t run.t.corrected)))))

(alias
 (name use-meta)
 (deps (package dune) (source_tree test-cases/use-meta))
 (action
  (chdir
   test-cases/use-meta
   (progn (run %{exe:cram.exe} -test run.t) (diff? run.t run.t.corrected)))))

(alias
 (name utop)
 (deps (package dune) (source_tree test-cases/utop))
 (action
  (chdir
   test-cases/utop
   (progn (run %{exe:cram.exe} -test run.t) (diff? run.t run.t.corrected)))))

(alias
 (name utop-default)
 (deps (package dune) (source_tree test-cases/utop-default))
 (action
  (chdir
   test-cases/utop-default
   (progn
    (run %{exe:cram.exe} -skip-versions <4.05.0 -test run.t)
    (diff? run.t run.t.corrected)))))

(alias
 (name utop-default-implementation)
 (deps (package dune) (source_tree test-cases/utop-default-implementation))
 (action
  (chdir
   test-cases/utop-default-implementation
   (progn
    (run %{exe:cram.exe} -skip-versions <4.05.0 -test run.t)
    (diff? run.t run.t.corrected)))))

(alias
 (name variants)
 (deps (package dune) (source_tree test-cases/variants))
 (action
  (chdir
   test-cases/variants
   (progn (run %{exe:cram.exe} -test run.t) (diff? run.t run.t.corrected)))))

(alias
 (name variants-external-declaration)
 (deps (package dune) (source_tree test-cases/variants-external-declaration))
 (action
  (chdir
   test-cases/variants-external-declaration
   (progn (run %{exe:cram.exe} -test run.t) (diff? run.t run.t.corrected)))))

(alias
 (name variants-external-declaration-conflict)
 (deps
  (package dune)
  (source_tree test-cases/variants-external-declaration-conflict))
 (action
  (chdir
   test-cases/variants-external-declaration-conflict
   (progn (run %{exe:cram.exe} -test run.t) (diff? run.t run.t.corrected)))))

(alias
 (name variants-multi-project)
 (deps (package dune) (source_tree test-cases/variants-multi-project))
 (action
  (chdir
   test-cases/variants-multi-project
   (progn (run %{exe:cram.exe} -test run.t) (diff? run.t run.t.corrected)))))

(alias
 (name variants-only-package)
 (deps (package dune) (source_tree test-cases/variants-only-package))
 (action
  (chdir
   test-cases/variants-only-package
   (progn (run %{exe:cram.exe} -test run.t) (diff? run.t run.t.corrected)))))

(alias
 (name variants-rogue-impl)
 (deps (package dune) (source_tree test-cases/variants-rogue-impl))
 (action
  (chdir
   test-cases/variants-rogue-impl
   (progn (run %{exe:cram.exe} -test run.t) (diff? run.t run.t.corrected)))))

(alias
 (name variants-wrong-external-declaration)
 (deps
  (package dune)
  (source_tree test-cases/variants-wrong-external-declaration))
 (action
  (chdir
   test-cases/variants-wrong-external-declaration
   (progn (run %{exe:cram.exe} -test run.t) (diff? run.t run.t.corrected)))))

(alias
 (name vendor)
 (deps (package dune) (source_tree test-cases/vendor))
 (action
  (chdir
   test-cases/vendor
   (progn (run %{exe:cram.exe} -test run.t) (diff? run.t run.t.corrected)))))

(alias
 (name vlib)
 (deps (package dune) (source_tree test-cases/vlib))
 (action
  (chdir
   test-cases/vlib
   (progn (run %{exe:cram.exe} -test run.t) (diff? run.t run.t.corrected)))))

(alias
 (name vlib-default-impl)
 (deps (package dune) (source_tree test-cases/vlib-default-impl))
 (action
  (chdir
   test-cases/vlib-default-impl
   (progn (run %{exe:cram.exe} -test run.t) (diff? run.t run.t.corrected)))))

(alias
 (name vlib-wrong-default-impl)
 (deps (package dune) (source_tree test-cases/vlib-wrong-default-impl))
 (action
  (chdir
   test-cases/vlib-wrong-default-impl
   (progn (run %{exe:cram.exe} -test run.t) (diff? run.t run.t.corrected)))))

(alias
 (name windows-diff)
 (deps (package dune) (source_tree test-cases/windows-diff))
 (action
  (chdir
   test-cases/windows-diff
   (progn (run %{exe:cram.exe} -test run.t) (diff? run.t run.t.corrected)))))

(alias
 (name workspaces)
 (deps (package dune) (source_tree test-cases/workspaces))
 (action
  (chdir
   test-cases/workspaces
   (progn (run %{exe:cram.exe} -test run.t) (diff? run.t run.t.corrected)))))

(alias
 (name wrapped-false-main-module-name)
 (deps (package dune) (source_tree test-cases/wrapped-false-main-module-name))
 (action
  (chdir
   test-cases/wrapped-false-main-module-name
   (progn (run %{exe:cram.exe} -test run.t) (diff? run.t run.t.corrected)))))

(alias
 (name wrapped-transition)
 (deps (package dune) (source_tree test-cases/wrapped-transition))
 (action
  (chdir
   test-cases/wrapped-transition
   (progn
    (run %{exe:cram.exe} -skip-versions <4.06.0 -test run.t)
    (diff? run.t run.t.corrected)))))

(alias
 (name runtest)
 (deps
  (alias action-modifying-a-dependency)
  (alias aliases)
  (alias all-alias)
  (alias allow_approximate_merlin)
  (alias bad-alias-error)
  (alias block-strings)
  (alias byte-code-only)
  (alias c-stubs)
  (alias check-alias)
  (alias configurator)
  (alias configurator-gh1166)
  (alias contents-depends-on-glob)
  (alias copy-files-non-sub-dir-error)
  (alias copy_files)
  (alias cross-compilation)
  (alias custom-build-dir)
  (alias custom-cross-compilation)
  (alias cxx-extension)
  (alias default-targets)
  (alias dep-on-dir-that-does-not-exist)
  (alias dep-vars)
  (alias depend-on-the-universe)
  (alias deps-conf-vars)
  (alias describe-project)
  (alias dev-flag-1103)
  (alias dialects)
  (alias dir-target-dep)
  (alias double-echo)
  (alias dune-build-dir-exec-1101)
  (alias dune-init)
  (alias dune-jbuild-var-case)
  (alias dune-package)
  (alias dune-ppx-driver-system)
  (alias dune-project-edition)
  (alias dune-project-meta)
  (alias dune-project-no-opam)
  (alias dup-fields)
  (alias duplicate-c-cxx)
  (alias duplicate-c-cxx-obj)
  (alias duplicate-project-names)
  (alias duplicate-target-no-loc)
  (alias embed-jbuild)
  (alias enabled_if)
  (alias env)
  (alias env-and-flags-include)
  (alias env-bin-pform)
  (alias env-bins)
  (alias env-cflags)
  (alias env-dune-file)
  (alias env-tracking)
  (alias env-var-expansion)
  (alias env-variables)
  (alias exclude-missing-module)
  (alias exe-name-mangle)
  (alias exec-cmd)
  (alias exec-missing)
  (alias external-lib-deps)
  (alias extra-lang-line)
  (alias fallback-dune)
  (alias findlib)
  (alias findlib-dynload)
  (alias findlib-error)
  (alias force-test)
  (alias format-dune-file)
  (alias formatting)
  (alias gen-opam-install-file)
  (alias github1019)
  (alias github1099)
  (alias github1231)
  (alias github1306)
  (alias github1342)
  (alias github1372)
  (alias github1395)
  (alias github1426)
  (alias github1529)
  (alias github1549)
  (alias github1560)
  (alias github1616)
  (alias github1811)
  (alias github1856)
  (alias github1946)
  (alias github20)
  (alias github2033)
  (alias github2061)
  (alias github2123)
  (alias github2206)
  (alias github2228)
  (alias github2272)
  (alias github24)
  (alias github25)
  (alias github534)
  (alias github568)
  (alias github597)
  (alias github644)
  (alias github660)
  (alias github717-odoc-index)
  (alias github734)
  (alias github759)
  (alias github761)
  (alias github764)
  (alias github784)
  (alias github992)
  (alias glob-deps)
  (alias ignored_subdirs)
  (alias include-loop)
  (alias include-qualified)
  (alias inline_tests)
  (alias install-dry-run)
  (alias install-libdir)
  (alias install-multiple-contexts)
  (alias install-partial-package)
  (alias install-rule-order)
  (alias install-with-var)
  (alias installable-dup-private-libs)
  (alias intf-only)
  (alias lib-available)
  (alias lib-errors)
  (alias link-deps)
  (alias lint)
  (alias loop)
  (alias macro-expand-error)
  (alias menhir)
  (alias merlin-tests)
  (alias meta-gen)
  (alias misc)
  (alias missing-loc-run)
  (alias multi-dir)
  (alias multiple-private-libs)
  (alias multiple-targets)
  (alias name-field-validation)
  (alias no-installable-mode)
  (alias no-name-field)
  (alias null-dep)
  (alias ocaml-config-macro)
  (alias ocaml-syntax)
  (alias ocamldep-multi-stanzas)
  (alias ocamllex-jbuild)
  (alias odig-files-workspace)
  (alias odoc)
  (alias odoc-package-mld-link)
  (alias odoc-unique-mlds)
  (alias old-dune-subsystem)
  (alias optional)
  (alias output-obj)
  (alias package-dep)
  (alias path-variables)
  (alias pkg-config-quoting)
  (alias ppx-rewriter)
  (alias preprocess-with-action)
  (alias private-modules)
  (alias private-public-overlap)
  (alias project-root)
  (alias promote)
  (alias quoting)
  (alias reason)
  (alias redirections)
  (alias reporting-of-cycles)
  (alias rule-target-external)
  (alias scope-bug)
  (alias scope-ppx-bug)
  (alias select)
  (alias several-packages)
  (alias shadow-bindings)
  (alias stale-artifact-removal)
  (alias stdlib-compilation)
  (alias subst)
  (alias syntax-versioning)
  (alias target-dir-alias)
  (alias targets-with-vars)
  (alias tests-stanza)
  (alias tests-stanza-action)
  (alias tests-stanza-action-syntax-version)
  (alias too-many-parens)
  (alias toplevel-stanza)
  (alias trace-file)
  (alias transitive-deps-mode)
  (alias unreadable-src)
  (alias upgrader)
  (alias use-meta)
  (alias utop)
  (alias utop-default)
  (alias utop-default-implementation)
  (alias variants)
  (alias variants-external-declaration)
  (alias variants-external-declaration-conflict)
  (alias variants-multi-project)
  (alias variants-only-package)
  (alias variants-rogue-impl)
  (alias variants-wrong-external-declaration)
  (alias vendor)
  (alias vlib)
  (alias vlib-default-impl)
  (alias vlib-wrong-default-impl)
  (alias windows-diff)
  (alias workspaces)
  (alias wrapped-false-main-module-name)
  (alias wrapped-transition)))

(alias
 (name runtest-no-deps)
 (deps
  (alias action-modifying-a-dependency)
  (alias aliases)
  (alias all-alias)
  (alias allow_approximate_merlin)
  (alias bad-alias-error)
  (alias block-strings)
  (alias byte-code-only)
  (alias c-stubs)
  (alias check-alias)
  (alias configurator)
  (alias configurator-gh1166)
  (alias contents-depends-on-glob)
  (alias copy-files-non-sub-dir-error)
  (alias copy_files)
  (alias custom-build-dir)
  (alias custom-cross-compilation)
  (alias cxx-extension)
  (alias default-targets)
  (alias dep-on-dir-that-does-not-exist)
  (alias dep-vars)
  (alias depend-on-the-universe)
  (alias deps-conf-vars)
  (alias describe-project)
  (alias dev-flag-1103)
  (alias dialects)
  (alias dir-target-dep)
  (alias double-echo)
  (alias dune-build-dir-exec-1101)
  (alias dune-init)
  (alias dune-jbuild-var-case)
  (alias dune-package)
  (alias dune-project-edition)
  (alias dune-project-meta)
  (alias dune-project-no-opam)
  (alias dup-fields)
  (alias duplicate-c-cxx)
  (alias duplicate-c-cxx-obj)
  (alias duplicate-project-names)
  (alias duplicate-target-no-loc)
  (alias embed-jbuild)
  (alias enabled_if)
  (alias env)
  (alias env-and-flags-include)
  (alias env-bin-pform)
  (alias env-bins)
  (alias env-cflags)
  (alias env-dune-file)
  (alias env-tracking)
  (alias env-var-expansion)
  (alias env-variables)
  (alias exclude-missing-module)
  (alias exe-name-mangle)
  (alias exec-cmd)
  (alias exec-missing)
  (alias external-lib-deps)
  (alias extra-lang-line)
  (alias fallback-dune)
  (alias findlib)
  (alias findlib-error)
  (alias force-test)
  (alias format-dune-file)
  (alias formatting)
  (alias github1019)
  (alias github1099)
  (alias github1231)
  (alias github1306)
  (alias github1342)
  (alias github1395)
  (alias github1426)
  (alias github1529)
  (alias github1549)
  (alias github1560)
  (alias github1616)
  (alias github1811)
  (alias github1856)
  (alias github1946)
  (alias github20)
  (alias github2033)
  (alias github2061)
  (alias github2123)
  (alias github2206)
  (alias github2228)
  (alias github2272)
  (alias github24)
  (alias github25)
  (alias github534)
  (alias github568)
  (alias github597)
  (alias github660)
  (alias github734)
  (alias github759)
  (alias github761)
  (alias github764)
  (alias github784)
  (alias github992)
  (alias glob-deps)
  (alias ignored_subdirs)
  (alias include-loop)
  (alias include-qualified)
  (alias inline_tests)
  (alias install-multiple-contexts)
  (alias install-partial-package)
  (alias install-rule-order)
  (alias install-with-var)
  (alias installable-dup-private-libs)
  (alias intf-only)
  (alias lib-available)
  (alias lib-errors)
  (alias link-deps)
  (alias loop)
  (alias macro-expand-error)
  (alias meta-gen)
  (alias misc)
  (alias missing-loc-run)
  (alias multi-dir)
  (alias multiple-targets)
  (alias name-field-validation)
  (alias no-installable-mode)
  (alias no-name-field)
  (alias null-dep)
  (alias ocaml-config-macro)
  (alias ocaml-syntax)
  (alias ocamldep-multi-stanzas)
  (alias ocamllex-jbuild)
  (alias odig-files-workspace)
  (alias old-dune-subsystem)
  (alias optional)
  (alias output-obj)
  (alias path-variables)
  (alias pkg-config-quoting)
  (alias preprocess-with-action)
  (alias private-modules)
  (alias project-root)
  (alias promote)
  (alias quoting)
  (alias redirections)
  (alias reporting-of-cycles)
  (alias rule-target-external)
  (alias scope-bug)
  (alias select)
  (alias several-packages)
  (alias shadow-bindings)
  (alias stale-artifact-removal)
  (alias stdlib-compilation)
  (alias subst)
  (alias syntax-versioning)
  (alias target-dir-alias)
  (alias targets-with-vars)
  (alias tests-stanza)
  (alias tests-stanza-action)
  (alias tests-stanza-action-syntax-version)
  (alias too-many-parens)
  (alias toplevel-stanza)
  (alias trace-file)
  (alias transitive-deps-mode)
  (alias unreadable-src)
  (alias upgrader)
  (alias variants)
  (alias variants-external-declaration)
  (alias variants-external-declaration-conflict)
  (alias variants-multi-project)
  (alias variants-only-package)
  (alias variants-rogue-impl)
  (alias variants-wrong-external-declaration)
  (alias vendor)
  (alias vlib)
  (alias vlib-default-impl)
  (alias vlib-wrong-default-impl)
  (alias windows-diff)
  (alias workspaces)
  (alias wrapped-false-main-module-name)
  (alias wrapped-transition)))

(alias
 (name runtest-disabled)
 (deps (alias cinaps) (alias envs-and-contexts)))

(alias (name runtest-js) (deps (alias js_of_ocaml)))

(alias (name runtest-coq) (deps (alias coq)))