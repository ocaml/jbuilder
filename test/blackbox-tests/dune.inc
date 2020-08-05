(rule
 (alias action-modifying-a-dependency)
 (deps
  (package dune)
  (source_tree test-cases/action-modifying-a-dependency.t)
  (alias test-deps))
 (action
  (chdir
   test-cases/action-modifying-a-dependency.t
   (progn (run dune-cram run run.t) (diff? run.t run.t.corrected)))))

(rule
 (alias aliases)
 (deps (package dune) (source_tree test-cases/aliases.t) (alias test-deps))
 (action
  (chdir
   test-cases/aliases.t
   (progn (run dune-cram run run.t) (diff? run.t run.t.corrected)))))

(rule
 (alias all-alias)
 (deps (package dune) (source_tree test-cases/all-alias.t) (alias test-deps))
 (action
  (chdir
   test-cases/all-alias.t
   (progn (run dune-cram run run.t) (diff? run.t run.t.corrected)))))

(rule
 (alias all-promotions)
 (deps
  (package dune)
  (source_tree test-cases/all-promotions.t)
  (alias test-deps))
 (action
  (chdir
   test-cases/all-promotions.t
   (progn (run dune-cram run run.t) (diff? run.t run.t.corrected)))))

(rule
 (alias bad-alias-error)
 (deps
  (package dune)
  (source_tree test-cases/bad-alias-error.t)
  (alias test-deps))
 (action
  (chdir
   test-cases/bad-alias-error.t
   (progn (run dune-cram run run.t) (diff? run.t run.t.corrected)))))

(rule
 (alias bin-eager-deps)
 (deps
  (package dune)
  (source_tree test-cases/bin-eager-deps.t)
  (alias test-deps))
 (action
  (chdir
   test-cases/bin-eager-deps.t
   (progn (run dune-cram run run.t) (diff? run.t run.t.corrected)))))

(rule
 (alias bisect-ppx-github3473)
 (deps
  (package dune)
  (source_tree test-cases/bisect-ppx/github3473.t)
  (alias test-deps))
 (action
  (chdir
   test-cases/bisect-ppx/github3473.t
   (progn (run dune-cram run run.t) (diff? run.t run.t.corrected)))))

(rule
 (alias bisect-ppx-main)
 (deps
  (package dune)
  (source_tree test-cases/bisect-ppx/main.t)
  (alias test-deps))
 (action
  (chdir
   test-cases/bisect-ppx/main.t
   (progn (run dune-cram run run.t) (diff? run.t run.t.corrected)))))

(rule
 (alias block-strings)
 (deps
  (package dune)
  (source_tree test-cases/block-strings.t)
  (alias test-deps))
 (action
  (chdir
   test-cases/block-strings.t
   (progn (run dune-cram run run.t) (diff? run.t run.t.corrected)))))

(rule
 (alias byte-code-only)
 (deps
  (package dune)
  (source_tree test-cases/byte-code-only.t)
  (alias test-deps))
 (action
  (chdir
   test-cases/byte-code-only.t
   (progn (run dune-cram run run.t) (diff? run.t run.t.corrected)))))

(rule
 (alias byte_complete)
 (deps
  (package dune)
  (source_tree test-cases/byte_complete.t)
  (alias test-deps))
 (action
  (chdir
   test-cases/byte_complete.t
   (progn (run dune-cram run run.t) (diff? run.t run.t.corrected)))))

(rule
 (alias c-stubs)
 (deps (package dune) (source_tree test-cases/c-stubs.t) (alias test-deps))
 (action
  (chdir
   test-cases/c-stubs.t
   (progn (run dune-cram run run.t) (diff? run.t run.t.corrected)))))

(rule
 (alias check-alias)
 (deps
  (package dune)
  (source_tree test-cases/check-alias.t)
  (alias test-deps))
 (action
  (chdir
   test-cases/check-alias.t
   (progn (run dune-cram run run.t) (diff? run.t run.t.corrected)))))

(rule
 (alias cinaps-include-subdirs)
 (deps
  (package dune)
  (source_tree test-cases/cinaps/include-subdirs.t)
  (alias test-deps))
 (action
  (chdir
   test-cases/cinaps/include-subdirs.t
   (progn (run dune-cram run run.t) (diff? run.t run.t.corrected)))))

(rule
 (alias cinaps-simple)
 (deps
  (package dune)
  (source_tree test-cases/cinaps/simple.t)
  (alias test-deps))
 (action
  (chdir
   test-cases/cinaps/simple.t
   (progn (run dune-cram run run.t) (diff? run.t run.t.corrected)))))

(rule
 (alias cmdliner-dep-conf)
 (deps
  (package dune)
  (source_tree test-cases/cmdliner-dep-conf.t)
  (alias test-deps))
 (action
  (chdir
   test-cases/cmdliner-dep-conf.t
   (progn (run dune-cram run run.t) (diff? run.t run.t.corrected)))))

(rule
 (alias contents-depends-on-glob)
 (deps
  (package dune)
  (source_tree test-cases/contents-depends-on-glob.t)
  (alias test-deps))
 (action
  (chdir
   test-cases/contents-depends-on-glob.t
   (progn (run dune-cram run run.t) (diff? run.t run.t.corrected)))))

(rule
 (alias copy-files-non-sub-dir-error)
 (deps
  (package dune)
  (source_tree test-cases/copy-files-non-sub-dir-error.t)
  (alias test-deps))
 (action
  (chdir
   test-cases/copy-files-non-sub-dir-error.t
   (progn (run dune-cram run run.t) (diff? run.t run.t.corrected)))))

(rule
 (alias copy_files)
 (deps (package dune) (source_tree test-cases/copy_files.t) (alias test-deps))
 (action
  (chdir
   test-cases/copy_files.t
   (progn (run dune-cram run run.t) (diff? run.t run.t.corrected)))))

(rule
 (alias coq-extract)
 (deps
  (package dune)
  (source_tree test-cases/coq/extract.t)
  (alias test-deps))
 (action
  (chdir
   test-cases/coq/extract.t
   (progn (run dune-cram run run.t) (diff? run.t run.t.corrected)))))

(rule
 (alias coq-github3624)
 (deps
  (package dune)
  (source_tree test-cases/coq/github3624.t)
  (alias test-deps))
 (action
  (chdir
   test-cases/coq/github3624.t
   (progn (run dune-cram run run.t) (diff? run.t run.t.corrected)))))

(rule
 (alias coq-main)
 (deps (package dune) (source_tree test-cases/coq/main.t) (alias test-deps))
 (action
  (chdir
   test-cases/coq/main.t
   (progn (run dune-cram run run.t) (diff? run.t run.t.corrected)))))

(rule
 (alias corrections)
 (deps
  (package dune)
  (source_tree test-cases/corrections.t)
  (alias test-deps))
 (action
  (chdir
   test-cases/corrections.t
   (progn (run dune-cram run run.t) (diff? run.t run.t.corrected)))))

(rule
 (alias cross-compilation)
 (deps
  (package dune)
  (source_tree test-cases/cross-compilation.t)
  (alias test-deps))
 (action
  (chdir
   test-cases/cross-compilation.t
   (progn (run dune-cram run run.t) (diff? run.t run.t.corrected)))))

(rule
 (alias custom-build-dir)
 (deps
  (package dune)
  (source_tree test-cases/custom-build-dir.t)
  (alias test-deps))
 (action
  (chdir
   test-cases/custom-build-dir.t
   (progn (run dune-cram run run.t) (diff? run.t run.t.corrected)))))

(rule
 (alias custom-cross-compilation)
 (deps
  (package dune)
  (source_tree test-cases/custom-cross-compilation.t)
  (alias test-deps))
 (action
  (chdir
   test-cases/custom-cross-compilation.t
   (progn (run dune-cram run run.t) (diff? run.t run.t.corrected)))))

(rule
 (alias cxx-extension)
 (deps
  (package dune)
  (source_tree test-cases/cxx-extension.t)
  (alias test-deps))
 (action
  (chdir
   test-cases/cxx-extension.t
   (progn (run dune-cram run run.t) (diff? run.t run.t.corrected)))))

(rule
 (alias cycle-detection-symlinks-2863)
 (deps
  (package dune)
  (source_tree test-cases/cycle-detection-symlinks-2863.t)
  (alias test-deps))
 (action
  (chdir
   test-cases/cycle-detection-symlinks-2863.t
   (progn (run dune-cram run run.t) (diff? run.t run.t.corrected)))))

(rule
 (alias default-implementation-package-mismatch1-package-mismatch1)
 (deps
  (package dune)
  (source_tree test-cases/default-implementation/package-mismatch1.t)
  (alias test-deps))
 (action
  (chdir
   test-cases/default-implementation/package-mismatch1.t
   (progn
    (run dune-cram run package-mismatch1.t)
    (diff? package-mismatch1.t package-mismatch1.t.corrected)))))

(rule
 (alias default-implementation-package-mismatch2-package-mismatch2)
 (deps
  (package dune)
  (source_tree test-cases/default-implementation/package-mismatch2.t)
  (alias test-deps))
 (action
  (chdir
   test-cases/default-implementation/package-mismatch2.t
   (progn
    (run dune-cram run package-mismatch2.t)
    (diff? package-mismatch2.t package-mismatch2.t.corrected)))))

(rule
 (alias default-targets)
 (deps
  (package dune)
  (source_tree test-cases/default-targets.t)
  (alias test-deps))
 (action
  (chdir
   test-cases/default-targets.t
   (progn (run dune-cram run run.t) (diff? run.t run.t.corrected)))))

(rule
 (alias dep-on-dir-that-does-not-exist)
 (deps
  (package dune)
  (source_tree test-cases/dep-on-dir-that-does-not-exist.t)
  (alias test-deps))
 (action
  (chdir
   test-cases/dep-on-dir-that-does-not-exist.t
   (progn (run dune-cram run run.t) (diff? run.t run.t.corrected)))))

(rule
 (alias dep-vars)
 (deps (package dune) (source_tree test-cases/dep-vars.t) (alias test-deps))
 (action
  (chdir
   test-cases/dep-vars.t
   (progn (run dune-cram run run.t) (diff? run.t run.t.corrected)))))

(rule
 (alias depend-on-the-universe)
 (deps
  (package dune)
  (source_tree test-cases/depend-on-the-universe.t)
  (alias test-deps))
 (action
  (chdir
   test-cases/depend-on-the-universe.t
   (progn (run dune-cram run run.t) (diff? run.t run.t.corrected)))))

(rule
 (alias deprecated-library-name)
 (deps
  (package dune)
  (source_tree test-cases/deprecated-library-name.t)
  (alias test-deps))
 (action
  (chdir
   test-cases/deprecated-library-name.t
   (progn (run dune-cram run run.t) (diff? run.t run.t.corrected)))))

(rule
 (alias deps-conf-vars)
 (deps
  (package dune)
  (source_tree test-cases/deps-conf-vars.t)
  (alias test-deps))
 (action
  (chdir
   test-cases/deps-conf-vars.t
   (progn (run dune-cram run run.t) (diff? run.t run.t.corrected)))))

(rule
 (alias describe)
 (deps (package dune) (source_tree test-cases/describe.t) (alias test-deps))
 (action
  (chdir
   test-cases/describe.t
   (progn (run dune-cram run run.t) (diff? run.t run.t.corrected)))))

(rule
 (alias dialects)
 (deps (package dune) (source_tree test-cases/dialects.t) (alias test-deps))
 (action
  (chdir
   test-cases/dialects.t
   (progn (run dune-cram run run.t) (diff? run.t run.t.corrected)))))

(rule
 (alias dir-target-dep)
 (deps
  (package dune)
  (source_tree test-cases/dir-target-dep.t)
  (alias test-deps))
 (action
  (chdir
   test-cases/dir-target-dep.t
   (progn (run dune-cram run run.t) (diff? run.t run.t.corrected)))))

(rule
 (alias disable-promotion)
 (deps
  (package dune)
  (source_tree test-cases/disable-promotion.t)
  (alias test-deps))
 (action
  (chdir
   test-cases/disable-promotion.t
   (progn (run dune-cram run run.t) (diff? run.t run.t.corrected)))))

(rule
 (alias double-echo)
 (deps
  (package dune)
  (source_tree test-cases/double-echo.t)
  (alias test-deps))
 (action
  (chdir
   test-cases/double-echo.t
   (progn (run dune-cram run run.t) (diff? run.t run.t.corrected)))))

(rule
 (alias dune-build-dir-exec-1101)
 (deps
  (package dune)
  (source_tree test-cases/dune-build-dir-exec-1101.t)
  (alias test-deps))
 (action
  (chdir
   test-cases/dune-build-dir-exec-1101.t
   (progn (run dune-cram run run.t) (diff? run.t run.t.corrected)))))

(rule
 (alias dune-cache-dedup)
 (deps
  (package dune)
  (source_tree test-cases/dune-cache/dedup.t)
  (alias test-deps))
 (action
  (chdir
   test-cases/dune-cache/dedup.t
   (progn (run dune-cram run run.t) (diff? run.t run.t.corrected)))))

(rule
 (alias dune-cache-dedup-direct)
 (deps
  (package dune)
  (source_tree test-cases/dune-cache/dedup-direct.t)
  (alias test-deps))
 (action
  (chdir
   test-cases/dune-cache/dedup-direct.t
   (progn (run dune-cram run run.t) (diff? run.t run.t.corrected)))))

(rule
 (alias dune-cache-dedup-direct-copy)
 (deps
  (package dune)
  (source_tree test-cases/dune-cache/dedup-direct-copy.t)
  (alias test-deps))
 (action
  (chdir
   test-cases/dune-cache/dedup-direct-copy.t
   (progn (run dune-cram run run.t) (diff? run.t run.t.corrected)))))

(rule
 (alias dune-cache-missing-data)
 (deps
  (package dune)
  (source_tree test-cases/dune-cache/missing-data)
  (alias test-deps))
 (action
  (chdir
   test-cases/dune-cache/missing-data
   (progn (run dune-cram run run.t) (diff? run.t run.t.corrected)))))

(rule
 (alias dune-cache-promote)
 (deps
  (package dune)
  (source_tree test-cases/dune-cache/promote.t)
  (alias test-deps))
 (action
  (chdir
   test-cases/dune-cache/promote.t
   (progn (run dune-cram run run.t) (diff? run.t run.t.corrected)))))

(rule
 (alias dune-cache-promote-copy)
 (deps
  (package dune)
  (source_tree test-cases/dune-cache/promote-copy.t)
  (alias test-deps))
 (action
  (chdir
   test-cases/dune-cache/promote-copy.t
   (progn (run dune-cram run run.t) (diff? run.t run.t.corrected)))))

(rule
 (alias dune-cache-promote-direct)
 (deps
  (package dune)
  (source_tree test-cases/dune-cache/promote-direct.t)
  (alias test-deps))
 (action
  (chdir
   test-cases/dune-cache/promote-direct.t
   (progn (run dune-cram run run.t) (diff? run.t run.t.corrected)))))

(rule
 (alias dune-cache-promote-direct-copy)
 (deps
  (package dune)
  (source_tree test-cases/dune-cache/promote-direct-copy.t)
  (alias test-deps))
 (action
  (chdir
   test-cases/dune-cache/promote-direct-copy.t
   (progn (run dune-cram run run.t) (diff? run.t run.t.corrected)))))

(rule
 (alias dune-cache-promotion-in-source-tree)
 (deps
  (package dune)
  (source_tree test-cases/dune-cache/promotion-in-source-tree.t)
  (alias test-deps))
 (action
  (chdir
   test-cases/dune-cache/promotion-in-source-tree.t
   (progn (run dune-cram run run.t) (diff? run.t run.t.corrected)))))

(rule
 (alias dune-cache-symlink)
 (deps
  (package dune)
  (source_tree test-cases/dune-cache/symlink.t)
  (alias test-deps))
 (action
  (chdir
   test-cases/dune-cache/symlink.t
   (progn (run dune-cram run run.t) (diff? run.t run.t.corrected)))))

(rule
 (alias dune-cache-symlink-direct)
 (deps
  (package dune)
  (source_tree test-cases/dune-cache/symlink-direct.t)
  (alias test-deps))
 (action
  (chdir
   test-cases/dune-cache/symlink-direct.t
   (progn (run dune-cram run run.t) (diff? run.t run.t.corrected)))))

(rule
 (alias dune-cache-trim)
 (deps
  (package dune)
  (source_tree test-cases/dune-cache/trim.t)
  (alias test-deps))
 (action
  (chdir
   test-cases/dune-cache/trim.t
   (progn (run dune-cram run run.t) (diff? run.t run.t.corrected)))))

(rule
 (alias dune-init)
 (deps (package dune) (source_tree test-cases/dune-init.t) (alias test-deps))
 (action
  (chdir
   test-cases/dune-init.t
   (progn (run dune-cram run run.t) (diff? run.t run.t.corrected)))))

(rule
 (alias dune-jbuild-var-case)
 (deps
  (package dune)
  (source_tree test-cases/dune-jbuild-var-case.t)
  (alias test-deps))
 (action
  (chdir
   test-cases/dune-jbuild-var-case.t
   (progn (run dune-cram run run.t) (diff? run.t run.t.corrected)))))

(rule
 (alias dune-package)
 (deps
  (package dune)
  (source_tree test-cases/dune-package.t)
  (alias test-deps))
 (action
  (chdir
   test-cases/dune-package.t
   (progn (run dune-cram run run.t) (diff? run.t run.t.corrected)))))

(rule
 (alias dune-ppx-driver-system)
 (deps
  (package dune)
  (source_tree test-cases/dune-ppx-driver-system.t)
  (alias test-deps))
 (action
  (chdir
   test-cases/dune-ppx-driver-system.t
   (progn (run dune-cram run run.t) (diff? run.t run.t.corrected)))))

(rule
 (alias dune-project-edition)
 (deps
  (package dune)
  (source_tree test-cases/dune-project-edition.t)
  (alias test-deps))
 (action
  (chdir
   test-cases/dune-project-edition.t
   (progn (run dune-cram run run.t) (diff? run.t run.t.corrected)))))

(rule
 (alias dune-project-meta-github2927)
 (deps
  (package dune)
  (source_tree test-cases/dune-project-meta/github2927)
  (alias test-deps))
 (action
  (chdir
   test-cases/dune-project-meta/github2927
   (progn (run dune-cram run run.t) (diff? run.t run.t.corrected)))))

(rule
 (alias dune-project-meta-main)
 (deps
  (package dune)
  (source_tree test-cases/dune-project-meta/main)
  (alias test-deps))
 (action
  (chdir
   test-cases/dune-project-meta/main
   (progn (run dune-cram run run.t) (diff? run.t run.t.corrected)))))

(rule
 (alias dune-project-no-opam)
 (deps
  (package dune)
  (source_tree test-cases/dune-project-no-opam.t)
  (alias test-deps))
 (action
  (chdir
   test-cases/dune-project-no-opam.t
   (progn (run dune-cram run run.t) (diff? run.t run.t.corrected)))))

(rule
 (alias dune_memory-and-the-universe)
 (deps
  (package dune)
  (source_tree test-cases/dune_memory-and-the-universe.t)
  (alias test-deps))
 (action
  (chdir
   test-cases/dune_memory-and-the-universe.t
   (progn (run dune-cram run run.t) (diff? run.t run.t.corrected)))))

(rule
 (alias dup-fields)
 (deps (package dune) (source_tree test-cases/dup-fields.t) (alias test-deps))
 (action
  (chdir
   test-cases/dup-fields.t
   (progn (run dune-cram run run.t) (diff? run.t run.t.corrected)))))

(rule
 (alias duplicate-c-cxx)
 (deps
  (package dune)
  (source_tree test-cases/duplicate-c-cxx.t)
  (alias test-deps))
 (action
  (chdir
   test-cases/duplicate-c-cxx.t
   (progn (run dune-cram run run.t) (diff? run.t run.t.corrected)))))

(rule
 (alias duplicate-c-cxx-obj)
 (deps
  (package dune)
  (source_tree test-cases/duplicate-c-cxx-obj.t)
  (alias test-deps))
 (action
  (chdir
   test-cases/duplicate-c-cxx-obj.t
   (progn (run dune-cram run run.t) (diff? run.t run.t.corrected)))))

(rule
 (alias duplicate-project-names)
 (deps
  (package dune)
  (source_tree test-cases/duplicate-project-names.t)
  (alias test-deps))
 (action
  (chdir
   test-cases/duplicate-project-names.t
   (progn (run dune-cram run run.t) (diff? run.t run.t.corrected)))))

(rule
 (alias duplicate-target-no-loc)
 (deps
  (package dune)
  (source_tree test-cases/duplicate-target-no-loc.t)
  (alias test-deps))
 (action
  (chdir
   test-cases/duplicate-target-no-loc.t
   (progn (run dune-cram run run.t) (diff? run.t run.t.corrected)))))

(rule
 (alias embed-jbuild)
 (deps
  (package dune)
  (source_tree test-cases/embed-jbuild.t)
  (alias test-deps))
 (action
  (chdir
   test-cases/embed-jbuild.t
   (progn (run dune-cram run run.t) (diff? run.t run.t.corrected)))))

(rule
 (alias empty-package-name)
 (deps
  (package dune)
  (source_tree test-cases/empty-package-name.t)
  (alias test-deps))
 (action
  (chdir
   test-cases/empty-package-name.t
   (progn (run dune-cram run run.t) (diff? run.t run.t.corrected)))))

(rule
 (alias enabled_if)
 (deps (package dune) (source_tree test-cases/enabled_if.t) (alias test-deps))
 (action
  (chdir
   test-cases/enabled_if.t
   (progn (run dune-cram run run.t) (diff? run.t run.t.corrected)))))

(rule
 (alias enabled_if-exec)
 (deps
  (package dune)
  (source_tree test-cases/enabled_if-exec.t)
  (alias test-deps))
 (action
  (chdir
   test-cases/enabled_if-exec.t
   (progn (run dune-cram run run.t) (diff? run.t run.t.corrected)))))

(rule
 (alias env-env-affects-expander-affects-env)
 (deps
  (package dune)
  (source_tree test-cases/env/env-affects-expander-affects-env.t)
  (alias test-deps))
 (action
  (chdir
   test-cases/env/env-affects-expander-affects-env.t
   (progn (run dune-cram run run.t) (diff? run.t run.t.corrected)))))

(rule
 (alias env-env-and-flags-include)
 (deps
  (package dune)
  (source_tree test-cases/env/env-and-flags-include.t)
  (alias test-deps))
 (action
  (chdir
   test-cases/env/env-and-flags-include.t
   (progn (run dune-cram run run.t) (diff? run.t run.t.corrected)))))

(rule
 (alias env-env-bin-pform)
 (deps
  (package dune)
  (source_tree test-cases/env/env-bin-pform.t)
  (alias test-deps))
 (action
  (chdir
   test-cases/env/env-bin-pform.t
   (progn (run dune-cram run run.t) (diff? run.t run.t.corrected)))))

(rule
 (alias env-env-bins)
 (deps
  (package dune)
  (source_tree test-cases/env/env-bins.t)
  (alias test-deps)
  (sandbox none))
 (action
  (chdir
   test-cases/env/env-bins.t
   (progn (run dune-cram run run.t) (diff? run.t run.t.corrected)))))

(rule
 (alias env-env-cflags)
 (deps
  (package dune)
  (source_tree test-cases/env/env-cflags.t)
  (alias test-deps))
 (action
  (chdir
   test-cases/env/env-cflags.t
   (progn (run dune-cram run run.t) (diff? run.t run.t.corrected))))
 (enabled_if (>= %{ocaml_version} 4.06.0)))

(rule
 (alias env-env-dune-file)
 (deps
  (package dune)
  (source_tree test-cases/env/env-dune-file.t)
  (alias test-deps))
 (action
  (chdir
   test-cases/env/env-dune-file.t
   (progn (run dune-cram run run.t) (diff? run.t run.t.corrected)))))

(rule
 (alias env-env-simple)
 (deps
  (package dune)
  (source_tree test-cases/env/env-simple.t)
  (alias test-deps))
 (action
  (chdir
   test-cases/env/env-simple.t
   (progn (run dune-cram run run.t) (diff? run.t run.t.corrected))))
 (enabled_if (>= %{ocaml_version} 4.06.0)))

(rule
 (alias env-env-tracking)
 (deps
  (package dune)
  (source_tree test-cases/env/env-tracking.t)
  (alias test-deps))
 (action
  (chdir
   test-cases/env/env-tracking.t
   (progn (run dune-cram run run.t) (diff? run.t run.t.corrected)))))

(rule
 (alias env-env-var-expansion)
 (deps
  (package dune)
  (source_tree test-cases/env/env-var-expansion.t)
  (alias test-deps))
 (action
  (chdir
   test-cases/env/env-var-expansion.t
   (progn (run dune-cram run run.t) (diff? run.t run.t.corrected)))))

(rule
 (alias env-env-variables)
 (deps
  (package dune)
  (source_tree test-cases/env/env-variables.t)
  (alias test-deps))
 (action
  (chdir
   test-cases/env/env-variables.t
   (progn (run dune-cram run run.t) (diff? run.t run.t.corrected)))))

(rule
 (alias env-envs-and-contexts)
 (deps
  (package dune)
  (source_tree test-cases/env/envs-and-contexts.t)
  (alias test-deps))
 (action
  (chdir
   test-cases/env/envs-and-contexts.t
   (progn (run dune-cram run run.t) (diff? run.t run.t.corrected)))))

(rule
 (alias exclude-missing-module)
 (deps
  (package dune)
  (source_tree test-cases/exclude-missing-module.t)
  (alias test-deps))
 (action
  (chdir
   test-cases/exclude-missing-module.t
   (progn (run dune-cram run run.t) (diff? run.t run.t.corrected)))))

(rule
 (alias exe-name-mangle)
 (deps
  (package dune)
  (source_tree test-cases/exe-name-mangle.t)
  (alias test-deps))
 (action
  (chdir
   test-cases/exe-name-mangle.t
   (progn (run dune-cram run run.t) (diff? run.t run.t.corrected)))))

(rule
 (alias exec-cmd)
 (deps (package dune) (source_tree test-cases/exec-cmd.t) (alias test-deps))
 (action
  (chdir
   test-cases/exec-cmd.t
   (progn (run dune-cram run run.t) (diff? run.t run.t.corrected)))))

(rule
 (alias exec-missing)
 (deps
  (package dune)
  (source_tree test-cases/exec-missing.t)
  (alias test-deps))
 (action
  (chdir
   test-cases/exec-missing.t
   (progn (run dune-cram run run.t) (diff? run.t run.t.corrected)))))

(rule
 (alias exes-with-c)
 (deps
  (package dune)
  (source_tree test-cases/exes-with-c.t)
  (alias test-deps))
 (action
  (chdir
   test-cases/exes-with-c.t
   (progn (run dune-cram run run.t) (diff? run.t run.t.corrected)))))

(rule
 (alias explicit_js_mode)
 (deps
  (package dune)
  (source_tree test-cases/explicit_js_mode.t)
  (alias test-deps))
 (action
  (chdir
   test-cases/explicit_js_mode.t
   (progn (run dune-cram run run.t) (diff? run.t run.t.corrected)))))

(rule
 (alias extensions-versionning)
 (deps
  (package dune)
  (source_tree test-cases/extensions-versionning.t)
  (alias test-deps))
 (action
  (chdir
   test-cases/extensions-versionning.t
   (progn (run dune-cram run run.t) (diff? run.t run.t.corrected)))))

(rule
 (alias external-lib-deps-github3143)
 (deps
  (package dune)
  (source_tree test-cases/external-lib-deps/github3143.t)
  (alias test-deps))
 (action
  (chdir
   test-cases/external-lib-deps/github3143.t
   (progn (run dune-cram run run.t) (diff? run.t run.t.corrected)))))

(rule
 (alias external-lib-deps-missing)
 (deps
  (package dune)
  (source_tree test-cases/external-lib-deps/missing)
  (alias test-deps))
 (action
  (chdir
   test-cases/external-lib-deps/missing
   (progn (run dune-cram run run.t) (diff? run.t run.t.corrected)))))

(rule
 (alias external-lib-deps-simple)
 (deps
  (package dune)
  (source_tree test-cases/external-lib-deps/simple.t)
  (alias test-deps))
 (action
  (chdir
   test-cases/external-lib-deps/simple.t
   (progn (run dune-cram run run.t) (diff? run.t run.t.corrected)))))

(rule
 (alias extra-lang-line)
 (deps
  (package dune)
  (source_tree test-cases/extra-lang-line.t)
  (alias test-deps))
 (action
  (chdir
   test-cases/extra-lang-line.t
   (progn (run dune-cram run run.t) (diff? run.t run.t.corrected)))))

(rule
 (alias fallback-dune)
 (deps
  (package dune)
  (source_tree test-cases/fallback-dune.t)
  (alias test-deps))
 (action
  (chdir
   test-cases/fallback-dune.t
   (progn (run dune-cram run run.t) (diff? run.t run.t.corrected)))))

(rule
 (alias fdo)
 (deps (package dune) (source_tree test-cases/fdo.t) (alias test-deps))
 (action
  (chdir
   test-cases/fdo.t
   (progn (run dune-cram run run.t) (diff? run.t run.t.corrected))))
 (enabled_if (>= %{ocaml_version} 4.11.0)))

(rule
 (alias findlib)
 (deps (package dune) (source_tree test-cases/findlib.t) (alias test-deps))
 (action
  (chdir
   test-cases/findlib.t
   (progn (run dune-cram run run.t) (diff? run.t run.t.corrected)))))

(rule
 (alias findlib-dynload)
 (deps
  (package dune)
  (source_tree test-cases/findlib-dynload.t)
  (alias test-deps))
 (action
  (chdir
   test-cases/findlib-dynload.t
   (progn (run dune-cram run run.t) (diff? run.t run.t.corrected)))))

(rule
 (alias findlib-error)
 (deps
  (package dune)
  (source_tree test-cases/findlib-error.t)
  (alias test-deps))
 (action
  (chdir
   test-cases/findlib-error.t
   (progn (run dune-cram run run.t) (diff? run.t run.t.corrected)))))

(rule
 (alias forbidden_libraries)
 (deps
  (package dune)
  (source_tree test-cases/forbidden_libraries.t)
  (alias test-deps))
 (action
  (chdir
   test-cases/forbidden_libraries.t
   (progn (run dune-cram run run.t) (diff? run.t run.t.corrected)))))

(rule
 (alias force-test)
 (deps (package dune) (source_tree test-cases/force-test.t) (alias test-deps))
 (action
  (chdir
   test-cases/force-test.t
   (progn (run dune-cram run run.t) (diff? run.t run.t.corrected)))))

(rule
 (alias foreign-library)
 (deps
  (package dune)
  (source_tree test-cases/foreign-library.t)
  (alias test-deps))
 (action
  (chdir
   test-cases/foreign-library.t
   (progn (run dune-cram run run.t) (diff? run.t run.t.corrected)))))

(rule
 (alias foreign-stubs)
 (deps
  (package dune)
  (source_tree test-cases/foreign-stubs.t)
  (alias test-deps))
 (action
  (chdir
   test-cases/foreign-stubs.t
   (progn (run dune-cram run run.t) (diff? run.t run.t.corrected)))))

(rule
 (alias format-dune-file)
 (deps
  (package dune)
  (source_tree test-cases/format-dune-file.t)
  (alias test-deps))
 (action
  (chdir
   test-cases/format-dune-file.t
   (progn (run dune-cram run run.t) (diff? run.t run.t.corrected)))))

(rule
 (alias formatting)
 (deps (package dune) (source_tree test-cases/formatting.t) (alias test-deps))
 (action
  (chdir
   test-cases/formatting.t
   (progn (run dune-cram run run.t) (diff? run.t run.t.corrected)))))

(rule
 (alias gen-opam-install-file)
 (deps
  (package dune)
  (source_tree test-cases/gen-opam-install-file.t)
  (alias test-deps))
 (action
  (chdir
   test-cases/gen-opam-install-file.t
   (progn (run dune-cram run run.t) (diff? run.t run.t.corrected)))))

(rule
 (alias generated-source-dir-overlap)
 (deps
  (package dune)
  (source_tree test-cases/generated-source-dir-overlap.t)
  (alias test-deps))
 (action
  (chdir
   test-cases/generated-source-dir-overlap.t
   (progn (run dune-cram run run.t) (diff? run.t run.t.corrected)))))

(rule
 (alias github1019)
 (deps (package dune) (source_tree test-cases/github1019.t) (alias test-deps))
 (action
  (chdir
   test-cases/github1019.t
   (progn (run dune-cram run run.t) (diff? run.t run.t.corrected)))))

(rule
 (alias github1099)
 (deps (package dune) (source_tree test-cases/github1099.t) (alias test-deps))
 (action
  (chdir
   test-cases/github1099.t
   (progn (run dune-cram run run.t) (diff? run.t run.t.corrected)))))

(rule
 (alias github1231)
 (deps (package dune) (source_tree test-cases/github1231.t) (alias test-deps))
 (action
  (chdir
   test-cases/github1231.t
   (progn (run dune-cram run run.t) (diff? run.t run.t.corrected)))))

(rule
 (alias github1342)
 (deps (package dune) (source_tree test-cases/github1342.t) (alias test-deps))
 (action
  (chdir
   test-cases/github1342.t
   (progn (run dune-cram run run.t) (diff? run.t run.t.corrected)))))

(rule
 (alias github1372)
 (deps (package dune) (source_tree test-cases/github1372.t) (alias test-deps))
 (action
  (chdir
   test-cases/github1372.t
   (progn (run dune-cram run run.t) (diff? run.t run.t.corrected)))))

(rule
 (alias github1395)
 (deps (package dune) (source_tree test-cases/github1395.t) (alias test-deps))
 (action
  (chdir
   test-cases/github1395.t
   (progn (run dune-cram run run.t) (diff? run.t run.t.corrected)))))

(rule
 (alias github1426)
 (deps (package dune) (source_tree test-cases/github1426.t) (alias test-deps))
 (action
  (chdir
   test-cases/github1426.t
   (progn (run dune-cram run run.t) (diff? run.t run.t.corrected)))))

(rule
 (alias github1529)
 (deps (package dune) (source_tree test-cases/github1529.t) (alias test-deps))
 (action
  (chdir
   test-cases/github1529.t
   (progn (run dune-cram run run.t) (diff? run.t run.t.corrected)))))

(rule
 (alias github1541)
 (deps (package dune) (source_tree test-cases/github1541.t) (alias test-deps))
 (action
  (chdir
   test-cases/github1541.t
   (progn (run dune-cram run run.t) (diff? run.t run.t.corrected)))))

(rule
 (alias github1549)
 (deps (package dune) (source_tree test-cases/github1549.t) (alias test-deps))
 (action
  (chdir
   test-cases/github1549.t
   (progn (run dune-cram run run.t) (diff? run.t run.t.corrected)))))

(rule
 (alias github1560)
 (deps (package dune) (source_tree test-cases/github1560.t) (alias test-deps))
 (action
  (chdir
   test-cases/github1560.t
   (progn (run dune-cram run run.t) (diff? run.t run.t.corrected)))))

(rule
 (alias github1616)
 (deps (package dune) (source_tree test-cases/github1616.t) (alias test-deps))
 (action
  (chdir
   test-cases/github1616.t
   (progn (run dune-cram run run.t) (diff? run.t run.t.corrected)))))

(rule
 (alias github1811)
 (deps (package dune) (source_tree test-cases/github1811.t) (alias test-deps))
 (action
  (chdir
   test-cases/github1811.t
   (progn (run dune-cram run run.t) (diff? run.t run.t.corrected)))))

(rule
 (alias github1855)
 (deps (package dune) (source_tree test-cases/github1855.t) (alias test-deps))
 (action
  (chdir
   test-cases/github1855.t
   (progn (run dune-cram run run.t) (diff? run.t run.t.corrected)))))

(rule
 (alias github1856)
 (deps (package dune) (source_tree test-cases/github1856.t) (alias test-deps))
 (action
  (chdir
   test-cases/github1856.t
   (progn (run dune-cram run run.t) (diff? run.t run.t.corrected)))))

(rule
 (alias github1946)
 (deps (package dune) (source_tree test-cases/github1946.t) (alias test-deps))
 (action
  (chdir
   test-cases/github1946.t
   (progn (run dune-cram run run.t) (diff? run.t run.t.corrected)))))

(rule
 (alias github20)
 (deps (package dune) (source_tree test-cases/github20.t) (alias test-deps))
 (action
  (chdir
   test-cases/github20.t
   (progn (run dune-cram run run.t) (diff? run.t run.t.corrected)))))

(rule
 (alias github2033)
 (deps (package dune) (source_tree test-cases/github2033.t) (alias test-deps))
 (action
  (chdir
   test-cases/github2033.t
   (progn (run dune-cram run run.t) (diff? run.t run.t.corrected)))))

(rule
 (alias github2061)
 (deps (package dune) (source_tree test-cases/github2061.t) (alias test-deps))
 (action
  (chdir
   test-cases/github2061.t
   (progn (run dune-cram run run.t) (diff? run.t run.t.corrected)))))

(rule
 (alias github2123)
 (deps (package dune) (source_tree test-cases/github2123.t) (alias test-deps))
 (action
  (chdir
   test-cases/github2123.t
   (progn (run dune-cram run run.t) (diff? run.t run.t.corrected)))))

(rule
 (alias github2206)
 (deps (package dune) (source_tree test-cases/github2206.t) (alias test-deps))
 (action
  (chdir
   test-cases/github2206.t
   (progn (run dune-cram run run.t) (diff? run.t run.t.corrected)))))

(rule
 (alias github2228)
 (deps (package dune) (source_tree test-cases/github2228.t) (alias test-deps))
 (action
  (chdir
   test-cases/github2228.t
   (progn (run dune-cram run run.t) (diff? run.t run.t.corrected)))))

(rule
 (alias github2272)
 (deps (package dune) (source_tree test-cases/github2272.t) (alias test-deps))
 (action
  (chdir
   test-cases/github2272.t
   (progn (run dune-cram run run.t) (diff? run.t run.t.corrected)))))

(rule
 (alias github24)
 (deps (package dune) (source_tree test-cases/github24.t) (alias test-deps))
 (action
  (chdir
   test-cases/github24.t
   (progn (run dune-cram run run.t) (diff? run.t run.t.corrected)))))

(rule
 (alias github2499)
 (deps (package dune) (source_tree test-cases/github2499.t) (alias test-deps))
 (action
  (chdir
   test-cases/github2499.t
   (progn (run dune-cram run run.t) (diff? run.t run.t.corrected)))))

(rule
 (alias github25)
 (deps (package dune) (source_tree test-cases/github25.t) (alias test-deps))
 (action
  (chdir
   test-cases/github25.t
   (progn (run dune-cram run run.t) (diff? run.t run.t.corrected)))))

(rule
 (alias github2584)
 (deps (package dune) (source_tree test-cases/github2584.t) (alias test-deps))
 (action
  (chdir
   test-cases/github2584.t
   (progn (run dune-cram run run.t) (diff? run.t run.t.corrected)))))

(rule
 (alias github2629)
 (deps (package dune) (source_tree test-cases/github2629.t) (alias test-deps))
 (action
  (chdir
   test-cases/github2629.t
   (progn (run dune-cram run run.t) (diff? run.t run.t.corrected)))))

(rule
 (alias github2681)
 (deps (package dune) (source_tree test-cases/github2681.t) (alias test-deps))
 (action
  (chdir
   test-cases/github2681.t
   (progn (run dune-cram run run.t) (diff? run.t run.t.corrected)))))

(rule
 (alias github2848)
 (deps (package dune) (source_tree test-cases/github2848.t) (alias test-deps))
 (action
  (chdir
   test-cases/github2848.t
   (progn (run dune-cram run run.t) (diff? run.t run.t.corrected)))))

(rule
 (alias github2990)
 (deps (package dune) (source_tree test-cases/github2990.t) (alias test-deps))
 (action
  (chdir
   test-cases/github2990.t
   (progn (run dune-cram run run.t) (diff? run.t run.t.corrected)))))

(rule
 (alias github3043)
 (deps (package dune) (source_tree test-cases/github3043.t) (alias test-deps))
 (action
  (chdir
   test-cases/github3043.t
   (progn (run dune-cram run run.t) (diff? run.t run.t.corrected)))))

(rule
 (alias github3046)
 (deps (package dune) (source_tree test-cases/github3046.t) (alias test-deps))
 (action
  (chdir
   test-cases/github3046.t
   (progn (run dune-cram run run.t) (diff? run.t run.t.corrected)))))

(rule
 (alias github3180)
 (deps (package dune) (source_tree test-cases/github3180.t) (alias test-deps))
 (action
  (chdir
   test-cases/github3180.t
   (progn (run dune-cram run run.t) (diff? run.t run.t.corrected)))))

(rule
 (alias github3188)
 (deps (package dune) (source_tree test-cases/github3188.t) (alias test-deps))
 (action
  (chdir
   test-cases/github3188.t
   (progn (run dune-cram run run.t) (diff? run.t run.t.corrected)))))

(rule
 (alias github3440)
 (deps (package dune) (source_tree test-cases/github3440.t) (alias test-deps))
 (action
  (chdir
   test-cases/github3440.t
   (progn (run dune-cram run run.t) (diff? run.t run.t.corrected)))))

(rule
 (alias github3490)
 (deps (package dune) (source_tree test-cases/github3490.t) (alias test-deps))
 (action
  (chdir
   test-cases/github3490.t
   (progn (run dune-cram run run.t) (diff? run.t run.t.corrected)))))

(rule
 (alias github3530)
 (deps (package dune) (source_tree test-cases/github3530.t) (alias test-deps))
 (action
  (chdir
   test-cases/github3530.t
   (progn (run dune-cram run run.t) (diff? run.t run.t.corrected)))))

(rule
 (alias github534)
 (deps (package dune) (source_tree test-cases/github534.t) (alias test-deps))
 (action
  (chdir
   test-cases/github534.t
   (progn (run dune-cram run run.t) (diff? run.t run.t.corrected)))))

(rule
 (alias github568)
 (deps (package dune) (source_tree test-cases/github568.t) (alias test-deps))
 (action
  (chdir
   test-cases/github568.t
   (progn (run dune-cram run run.t) (diff? run.t run.t.corrected)))))

(rule
 (alias github597)
 (deps (package dune) (source_tree test-cases/github597.t) (alias test-deps))
 (action
  (chdir
   test-cases/github597.t
   (progn (run dune-cram run run.t) (diff? run.t run.t.corrected)))))

(rule
 (alias github644)
 (deps (package dune) (source_tree test-cases/github644.t) (alias test-deps))
 (action
  (chdir
   test-cases/github644.t
   (progn (run dune-cram run run.t) (diff? run.t run.t.corrected)))))

(rule
 (alias github660)
 (deps (package dune) (source_tree test-cases/github660.t) (alias test-deps))
 (action
  (chdir
   test-cases/github660.t
   (progn (run dune-cram run run.t) (diff? run.t run.t.corrected)))))

(rule
 (alias github734)
 (deps (package dune) (source_tree test-cases/github734.t) (alias test-deps))
 (action
  (chdir
   test-cases/github734.t
   (progn (run dune-cram run run.t) (diff? run.t run.t.corrected)))))

(rule
 (alias github759)
 (deps (package dune) (source_tree test-cases/github759.t) (alias test-deps))
 (action
  (chdir
   test-cases/github759.t
   (progn (run dune-cram run run.t) (diff? run.t run.t.corrected)))))

(rule
 (alias github761)
 (deps (package dune) (source_tree test-cases/github761.t) (alias test-deps))
 (action
  (chdir
   test-cases/github761.t
   (progn (run dune-cram run run.t) (diff? run.t run.t.corrected)))))

(rule
 (alias github764)
 (deps (package dune) (source_tree test-cases/github764.t) (alias test-deps))
 (action
  (chdir
   test-cases/github764.t
   (progn (run dune-cram run run.t) (diff? run.t run.t.corrected))))
 (enabled_if (<> %{ocaml-config:system} win)))

(rule
 (alias github784)
 (deps (package dune) (source_tree test-cases/github784.t) (alias test-deps))
 (action
  (chdir
   test-cases/github784.t
   (progn (run dune-cram run run.t) (diff? run.t run.t.corrected)))))

(rule
 (alias github992)
 (deps (package dune) (source_tree test-cases/github992.t) (alias test-deps))
 (action
  (chdir
   test-cases/github992.t
   (progn (run dune-cram run run.t) (diff? run.t run.t.corrected)))))

(rule
 (alias glob-deps)
 (deps (package dune) (source_tree test-cases/glob-deps.t) (alias test-deps))
 (action
  (chdir
   test-cases/glob-deps.t
   (progn (run dune-cram run run.t) (diff? run.t run.t.corrected)))))

(rule
 (alias glob-deps-change)
 (deps
  (package dune)
  (source_tree test-cases/glob-deps-change.t)
  (alias test-deps))
 (action
  (chdir
   test-cases/glob-deps-change.t
   (progn (run dune-cram run run.t) (diff? run.t run.t.corrected)))))

(rule
 (alias hints-root-executables)
 (deps
  (package dune)
  (source_tree test-cases/hints-root-executables.t)
  (alias test-deps))
 (action
  (chdir
   test-cases/hints-root-executables.t
   (progn (run dune-cram run run.t) (diff? run.t run.t.corrected)))))

(rule
 (alias ignored_subdirs)
 (deps
  (package dune)
  (source_tree test-cases/ignored_subdirs.t)
  (alias test-deps))
 (action
  (chdir
   test-cases/ignored_subdirs.t
   (progn (run dune-cram run run.t) (diff? run.t run.t.corrected)))))

(rule
 (alias include-loop)
 (deps
  (package dune)
  (source_tree test-cases/include-loop.t)
  (alias test-deps))
 (action
  (chdir
   test-cases/include-loop.t
   (progn (run dune-cram run run.t) (diff? run.t run.t.corrected)))))

(rule
 (alias include-qualified)
 (deps
  (package dune)
  (source_tree test-cases/include-qualified.t)
  (alias test-deps))
 (action
  (chdir
   test-cases/include-qualified.t
   (progn (run dune-cram run run.t) (diff? run.t run.t.corrected)))))

(rule
 (alias incremental-rebuilds)
 (deps
  (package dune)
  (source_tree test-cases/incremental-rebuilds.t)
  (alias test-deps))
 (action
  (chdir
   test-cases/incremental-rebuilds.t
   (progn (run dune-cram run run.t) (diff? run.t run.t.corrected)))))

(rule
 (alias inline_tests-dune-file)
 (deps
  (package dune)
  (source_tree test-cases/inline_tests/dune-file.t)
  (alias test-deps))
 (action
  (chdir
   test-cases/inline_tests/dune-file.t
   (progn (run dune-cram run run.t) (diff? run.t run.t.corrected)))))

(rule
 (alias inline_tests-many-backends-choose)
 (deps
  (package dune)
  (source_tree test-cases/inline_tests/many-backends-choose.t)
  (alias test-deps))
 (action
  (chdir
   test-cases/inline_tests/many-backends-choose.t
   (progn (run dune-cram run run.t) (diff? run.t run.t.corrected)))))

(rule
 (alias inline_tests-missing-backend)
 (deps
  (package dune)
  (source_tree test-cases/inline_tests/missing-backend.t)
  (alias test-deps))
 (action
  (chdir
   test-cases/inline_tests/missing-backend.t
   (progn (run dune-cram run run.t) (diff? run.t run.t.corrected)))))

(rule
 (alias inline_tests-multi-mode)
 (deps
  (package dune)
  (source_tree test-cases/inline_tests-multi-mode.t)
  (alias test-deps))
 (action
  (chdir
   test-cases/inline_tests-multi-mode.t
   (progn (run dune-cram run run.t) (diff? run.t run.t.corrected)))))

(rule
 (alias inline_tests-multiple-inline-tests)
 (deps
  (package dune)
  (source_tree test-cases/inline_tests/multiple-inline-tests.t)
  (alias test-deps))
 (action
  (chdir
   test-cases/inline_tests/multiple-inline-tests.t
   (progn (run dune-cram run run.t) (diff? run.t run.t.corrected)))))

(rule
 (alias inline_tests-simple)
 (deps
  (package dune)
  (source_tree test-cases/inline_tests/simple.t)
  (alias test-deps))
 (action
  (chdir
   test-cases/inline_tests/simple.t
   (progn (run dune-cram run run.t) (diff? run.t run.t.corrected)))))

(rule
 (alias inline_tests-too-many-backends)
 (deps
  (package dune)
  (source_tree test-cases/inline_tests/too-many-backends.t)
  (alias test-deps))
 (action
  (chdir
   test-cases/inline_tests/too-many-backends.t
   (progn (run dune-cram run run.t) (diff? run.t run.t.corrected)))))

(rule
 (alias install-dry-run)
 (deps
  (package dune)
  (source_tree test-cases/install-dry-run.t)
  (alias test-deps))
 (action
  (chdir
   test-cases/install-dry-run.t
   (progn (run dune-cram run run.t) (diff? run.t run.t.corrected)))))

(rule
 (alias install-libdir)
 (deps
  (package dune)
  (source_tree test-cases/install-libdir.t)
  (alias test-deps))
 (action
  (chdir
   test-cases/install-libdir.t
   (progn (run dune-cram run run.t) (diff? run.t run.t.corrected)))))

(rule
 (alias install-mandir)
 (deps
  (package dune)
  (source_tree test-cases/install-mandir.t)
  (alias test-deps))
 (action
  (chdir
   test-cases/install-mandir.t
   (progn (run dune-cram run run.t) (diff? run.t run.t.corrected)))))

(rule
 (alias install-multiple-contexts)
 (deps
  (package dune)
  (source_tree test-cases/install-multiple-contexts.t)
  (alias test-deps))
 (action
  (chdir
   test-cases/install-multiple-contexts.t
   (progn (run dune-cram run run.t) (diff? run.t run.t.corrected)))))

(rule
 (alias install-partial-package)
 (deps
  (package dune)
  (source_tree test-cases/install-partial-package.t)
  (alias test-deps))
 (action
  (chdir
   test-cases/install-partial-package.t
   (progn (run dune-cram run run.t) (diff? run.t run.t.corrected)))))

(rule
 (alias install-rule-order)
 (deps
  (package dune)
  (source_tree test-cases/install-rule-order.t)
  (alias test-deps))
 (action
  (chdir
   test-cases/install-rule-order.t
   (progn (run dune-cram run run.t) (diff? run.t run.t.corrected)))))

(rule
 (alias install-single-section)
 (deps
  (package dune)
  (source_tree test-cases/install-single-section.t)
  (alias test-deps))
 (action
  (chdir
   test-cases/install-single-section.t
   (progn (run dune-cram run run.t) (diff? run.t run.t.corrected)))))

(rule
 (alias install-with-var)
 (deps
  (package dune)
  (source_tree test-cases/install-with-var.t)
  (alias test-deps))
 (action
  (chdir
   test-cases/install-with-var.t
   (progn (run dune-cram run run.t) (diff? run.t run.t.corrected)))))

(rule
 (alias installable-dup-private-libs)
 (deps
  (package dune)
  (source_tree test-cases/installable-dup-private-libs.t)
  (alias test-deps))
 (action
  (chdir
   test-cases/installable-dup-private-libs.t
   (progn (run dune-cram run run.t) (diff? run.t run.t.corrected)))))

(rule
 (alias instrumentation)
 (deps
  (package dune)
  (source_tree test-cases/instrumentation.t)
  (alias test-deps))
 (action
  (chdir
   test-cases/instrumentation.t
   (progn (run dune-cram run run.t) (diff? run.t run.t.corrected)))))

(rule
 (alias intf-only)
 (deps (package dune) (source_tree test-cases/intf-only.t) (alias test-deps))
 (action
  (chdir
   test-cases/intf-only.t
   (progn (run dune-cram run run.t) (diff? run.t run.t.corrected)))))

(rule
 (alias invalid-dune-package)
 (deps
  (package dune)
  (source_tree test-cases/invalid-dune-package.t)
  (alias test-deps))
 (action
  (chdir
   test-cases/invalid-dune-package.t
   (progn (run dune-cram run run.t) (diff? run.t run.t.corrected)))))

(rule
 (alias invalid-module-name)
 (deps
  (package dune)
  (source_tree test-cases/invalid-module-name.t)
  (alias test-deps))
 (action
  (chdir
   test-cases/invalid-module-name.t
   (progn (run dune-cram run run.t) (diff? run.t run.t.corrected)))))

(rule
 (alias invalid-pform)
 (deps
  (package dune)
  (source_tree test-cases/invalid-pform.t)
  (alias test-deps))
 (action
  (chdir
   test-cases/invalid-pform.t
   (progn (run dune-cram run run.t) (diff? run.t run.t.corrected)))))

(rule
 (alias jsoo-github3622)
 (deps
  (package dune)
  (source_tree test-cases/jsoo/github3622.t)
  (alias test-deps))
 (action
  (chdir
   test-cases/jsoo/github3622.t
   (progn (run dune-cram run run.t) (diff? run.t run.t.corrected)))))

(rule
 (alias jsoo-inline-tests)
 (deps
  (package dune)
  (source_tree test-cases/jsoo/inline-tests.t)
  (alias test-deps))
 (action
  (chdir
   test-cases/jsoo/inline-tests.t
   (progn (run dune-cram run run.t) (diff? run.t run.t.corrected)))))

(rule
 (alias jsoo-simple)
 (deps
  (package dune)
  (source_tree test-cases/jsoo/simple.t)
  (alias test-deps))
 (action
  (chdir
   test-cases/jsoo/simple.t
   (progn (run dune-cram run run.t) (diff? run.t run.t.corrected)))))

(rule
 (alias lib)
 (deps (package dune) (source_tree test-cases/lib.t) (alias test-deps))
 (action
  (chdir
   test-cases/lib.t
   (progn (run dune-cram run run.t) (diff? run.t run.t.corrected)))))

(rule
 (alias lib-available)
 (deps
  (package dune)
  (source_tree test-cases/lib-available.t)
  (alias test-deps))
 (action
  (chdir
   test-cases/lib-available.t
   (progn (run dune-cram run run.t) (diff? run.t run.t.corrected)))))

(rule
 (alias lib-errors)
 (deps (package dune) (source_tree test-cases/lib-errors.t) (alias test-deps))
 (action
  (chdir
   test-cases/lib-errors.t
   (progn (run dune-cram run run.t) (diff? run.t run.t.corrected)))))

(rule
 (alias link-deps)
 (deps (package dune) (source_tree test-cases/link-deps.t) (alias test-deps))
 (action
  (chdir
   test-cases/link-deps.t
   (progn (run dune-cram run run.t) (diff? run.t run.t.corrected)))))

(rule
 (alias link-includes)
 (deps
  (package dune)
  (source_tree test-cases/link-includes.t)
  (alias test-deps))
 (action
  (chdir
   test-cases/link-includes.t
   (progn (run dune-cram run run.t) (diff? run.t run.t.corrected)))))

(rule
 (alias lint)
 (deps (package dune) (source_tree test-cases/lint.t) (alias test-deps))
 (action
  (chdir
   test-cases/lint.t
   (progn (run dune-cram run run.t) (diff? run.t run.t.corrected)))))

(rule
 (alias loop)
 (deps (package dune) (source_tree test-cases/loop.t) (alias test-deps))
 (action
  (chdir
   test-cases/loop.t
   (progn (run dune-cram run run.t) (diff? run.t run.t.corrected)))))

(rule
 (alias macro-expand-error)
 (deps
  (package dune)
  (source_tree test-cases/macro-expand-error.t)
  (alias test-deps))
 (action
  (chdir
   test-cases/macro-expand-error.t
   (progn (run dune-cram run run.t) (diff? run.t run.t.corrected)))))

(rule
 (alias mdx-stanza)
 (deps (package dune) (source_tree test-cases/mdx-stanza.t) (alias test-deps))
 (action
  (chdir
   test-cases/mdx-stanza.t
   (progn (run dune-cram run run.t) (diff? run.t run.t.corrected)))))

(rule
 (alias menhir-cmly)
 (deps
  (package dune)
  (source_tree test-cases/menhir/cmly.t)
  (alias test-deps))
 (action
  (chdir
   test-cases/menhir/cmly.t
   (progn (run dune-cram run run.t) (diff? run.t run.t.corrected)))))

(rule
 (alias menhir-env)
 (deps (package dune) (source_tree test-cases/menhir/env.t) (alias test-deps))
 (action
  (chdir
   test-cases/menhir/env.t
   (progn (run dune-cram run run.t) (diff? run.t run.t.corrected)))))

(rule
 (alias menhir-general)
 (deps
  (package dune)
  (source_tree test-cases/menhir/general.t)
  (alias test-deps))
 (action
  (chdir
   test-cases/menhir/general.t
   (progn (run dune-cram run run.t) (diff? run.t run.t.corrected)))))

(rule
 (alias menhir-general-2.0)
 (deps
  (package dune)
  (source_tree test-cases/menhir/general-2.0.t)
  (alias test-deps))
 (action
  (chdir
   test-cases/menhir/general-2.0.t
   (progn (run dune-cram run run.t) (diff? run.t run.t.corrected)))))

(rule
 (alias menhir-promote)
 (deps
  (package dune)
  (source_tree test-cases/menhir/promote.t)
  (alias test-deps))
 (action
  (chdir
   test-cases/menhir/promote.t
   (progn (run dune-cram run run.t) (diff? run.t run.t.corrected)))))

(rule
 (alias merlin-allow_approximate_merlin)
 (deps
  (package dune)
  (source_tree test-cases/merlin/allow_approximate_merlin.t)
  (alias test-deps))
 (action
  (chdir
   test-cases/merlin/allow_approximate_merlin.t
   (progn (run dune-cram run run.t) (diff? run.t run.t.corrected)))))

(rule
 (alias merlin-merlin-tests)
 (deps
  (package dune)
  (source_tree test-cases/merlin/merlin-tests.t)
  (alias test-deps))
 (action
  (chdir
   test-cases/merlin/merlin-tests.t
   (progn (run dune-cram run run.t) (diff? run.t run.t.corrected)))))

(rule
 (alias merlin-server)
 (deps
  (package dune)
  (source_tree test-cases/merlin/server.t)
  (alias test-deps))
 (action
  (chdir
   test-cases/merlin/server.t
   (progn (run dune-cram run run.t) (diff? run.t run.t.corrected)))))

(rule
 (alias merlin-src-dirs-of-deps)
 (deps
  (package dune)
  (source_tree test-cases/merlin/src-dirs-of-deps.t)
  (alias test-deps))
 (action
  (chdir
   test-cases/merlin/src-dirs-of-deps.t
   (progn (run dune-cram run run.t) (diff? run.t run.t.corrected)))))

(rule
 (alias meta-gen)
 (deps (package dune) (source_tree test-cases/meta-gen.t) (alias test-deps))
 (action
  (chdir
   test-cases/meta-gen.t
   (progn (run dune-cram run run.t) (diff? run.t run.t.corrected)))))

(rule
 (alias misc)
 (deps (package dune) (source_tree test-cases/misc.t) (alias test-deps))
 (action
  (chdir
   test-cases/misc.t
   (progn (run dune-cram run run.t) (diff? run.t run.t.corrected)))))

(rule
 (alias missing-loc-run)
 (deps
  (package dune)
  (source_tree test-cases/missing-loc-run.t)
  (alias test-deps))
 (action
  (chdir
   test-cases/missing-loc-run.t
   (progn (run dune-cram run run.t) (diff? run.t run.t.corrected)))))

(rule
 (alias multi-dir)
 (deps (package dune) (source_tree test-cases/multi-dir.t) (alias test-deps))
 (action
  (chdir
   test-cases/multi-dir.t
   (progn (run dune-cram run run.t) (diff? run.t run.t.corrected)))))

(rule
 (alias multiple-targets)
 (deps
  (package dune)
  (source_tree test-cases/multiple-targets.t)
  (alias test-deps))
 (action
  (chdir
   test-cases/multiple-targets.t
   (progn (run dune-cram run run.t) (diff? run.t run.t.corrected)))))

(rule
 (alias name-field-validation)
 (deps
  (package dune)
  (source_tree test-cases/name-field-validation.t)
  (alias test-deps))
 (action
  (chdir
   test-cases/name-field-validation.t
   (progn (run dune-cram run run.t) (diff? run.t run.t.corrected)))))

(rule
 (alias no-infer)
 (deps (package dune) (source_tree test-cases/no-infer.t) (alias test-deps))
 (action
  (chdir
   test-cases/no-infer.t
   (progn (run dune-cram run run.t) (diff? run.t run.t.corrected)))))

(rule
 (alias no-installable-mode)
 (deps
  (package dune)
  (source_tree test-cases/no-installable-mode.t)
  (alias test-deps))
 (action
  (chdir
   test-cases/no-installable-mode.t
   (progn (run dune-cram run run.t) (diff? run.t run.t.corrected)))))

(rule
 (alias null-dep)
 (deps (package dune) (source_tree test-cases/null-dep.t) (alias test-deps))
 (action
  (chdir
   test-cases/null-dep.t
   (progn (run dune-cram run run.t) (diff? run.t run.t.corrected)))))

(rule
 (alias ocaml-config-macro)
 (deps
  (package dune)
  (source_tree test-cases/ocaml-config-macro.t)
  (alias test-deps))
 (action
  (chdir
   test-cases/ocaml-config-macro.t
   (progn (run dune-cram run run.t) (diff? run.t run.t.corrected)))))

(rule
 (alias ocaml-syntax)
 (deps
  (package dune)
  (source_tree test-cases/ocaml-syntax.t)
  (alias test-deps))
 (action
  (chdir
   test-cases/ocaml-syntax.t
   (progn (run dune-cram run run.t) (diff? run.t run.t.corrected)))))

(rule
 (alias ocamldep-multi-stanzas)
 (deps
  (package dune)
  (source_tree test-cases/ocamldep-multi-stanzas.t)
  (alias test-deps))
 (action
  (chdir
   test-cases/ocamldep-multi-stanzas.t
   (progn (run dune-cram run run.t) (diff? run.t run.t.corrected)))))

(rule
 (alias odig-files-workspace)
 (deps
  (package dune)
  (source_tree test-cases/odig-files-workspace.t)
  (alias test-deps))
 (action
  (chdir
   test-cases/odig-files-workspace.t
   (progn (run dune-cram run run.t) (diff? run.t run.t.corrected)))))

(rule
 (alias odoc-github717-odoc-index)
 (deps
  (package dune)
  (source_tree test-cases/odoc/github717-odoc-index.t)
  (alias test-deps))
 (action
  (chdir
   test-cases/odoc/github717-odoc-index.t
   (progn (run dune-cram run run.t) (diff? run.t run.t.corrected))))
 (enabled_if (<> %{ocaml_version} 4.02.3)))

(rule
 (alias odoc-multiple-private-libs)
 (deps
  (package dune)
  (source_tree test-cases/odoc/multiple-private-libs.t)
  (alias test-deps))
 (action
  (chdir
   test-cases/odoc/multiple-private-libs.t
   (progn (run dune-cram run run.t) (diff? run.t run.t.corrected))))
 (enabled_if (<> %{ocaml_version} 4.02.3)))

(rule
 (alias odoc-odoc-package-mld-link)
 (deps
  (package dune)
  (source_tree test-cases/odoc/odoc-package-mld-link.t)
  (alias test-deps))
 (action
  (chdir
   test-cases/odoc/odoc-package-mld-link.t
   (progn (run dune-cram run run.t) (diff? run.t run.t.corrected))))
 (enabled_if (<> %{ocaml_version} 4.02.3)))

(rule
 (alias odoc-odoc-simple)
 (deps
  (package dune)
  (source_tree test-cases/odoc/odoc-simple.t)
  (alias test-deps))
 (action
  (chdir
   test-cases/odoc/odoc-simple.t
   (progn (run dune-cram run run.t) (diff? run.t run.t.corrected))))
 (enabled_if (<> %{ocaml_version} 4.02.3)))

(rule
 (alias odoc-odoc-unique-mlds)
 (deps
  (package dune)
  (source_tree test-cases/odoc/odoc-unique-mlds.t)
  (alias test-deps))
 (action
  (chdir
   test-cases/odoc/odoc-unique-mlds.t
   (progn (run dune-cram run run.t) (diff? run.t run.t.corrected))))
 (enabled_if (<> %{ocaml_version} 4.02.3)))

(rule
 (alias odoc-warnings)
 (deps
  (package dune)
  (source_tree test-cases/odoc/warnings.t)
  (alias test-deps))
 (action
  (chdir
   test-cases/odoc/warnings.t
   (progn (run dune-cram run run.t) (diff? run.t run.t.corrected))))
 (enabled_if (<> %{ocaml_version} 4.02.3)))

(rule
 (alias old-dune-subsystem)
 (deps
  (package dune)
  (source_tree test-cases/old-dune-subsystem.t)
  (alias test-deps))
 (action
  (chdir
   test-cases/old-dune-subsystem.t
   (progn (run dune-cram run run.t) (diff? run.t run.t.corrected)))))

(rule
 (alias optional)
 (deps (package dune) (source_tree test-cases/optional.t) (alias test-deps))
 (action
  (chdir
   test-cases/optional.t
   (progn (run dune-cram run run.t) (diff? run.t run.t.corrected)))))

(rule
 (alias optional-executable)
 (deps
  (package dune)
  (source_tree test-cases/optional-executable.t)
  (alias test-deps))
 (action
  (chdir
   test-cases/optional-executable.t
   (progn (run dune-cram run run.t) (diff? run.t run.t.corrected)))))

(rule
 (alias output-obj)
 (deps (package dune) (source_tree test-cases/output-obj.t) (alias test-deps))
 (action
  (chdir
   test-cases/output-obj.t
   (progn (run dune-cram run run.t) (diff? run.t run.t.corrected))))
 (enabled_if
  (and
   (and (<> %{ocaml-config:system} macosx) (<> %{ocaml-config:system} win))
   (>= %{ocaml_version} 4.06.0))))

(rule
 (alias package-dep)
 (deps
  (package dune)
  (source_tree test-cases/package-dep.t)
  (alias test-deps))
 (action
  (chdir
   test-cases/package-dep.t
   (progn (run dune-cram run run.t) (diff? run.t run.t.corrected)))))

(rule
 (alias path-variables)
 (deps
  (package dune)
  (source_tree test-cases/path-variables.t)
  (alias test-deps))
 (action
  (chdir
   test-cases/path-variables.t
   (progn (run dune-cram run run.t) (diff? run.t run.t.corrected)))))

(rule
 (alias pipe-actions)
 (deps
  (package dune)
  (source_tree test-cases/pipe-actions.t)
  (alias test-deps))
 (action
  (chdir
   test-cases/pipe-actions.t
   (progn (run dune-cram run run.t) (diff? run.t run.t.corrected)))))

(rule
 (alias plugin-mode)
 (deps
  (package dune)
  (source_tree test-cases/plugin-mode.t)
  (alias test-deps))
 (action
  (chdir
   test-cases/plugin-mode.t
   (progn (run dune-cram run run.t) (diff? run.t run.t.corrected)))))

(rule
 (alias ppx-rewriter)
 (deps
  (package dune)
  (source_tree test-cases/ppx-rewriter.t)
  (alias test-deps))
 (action
  (chdir
   test-cases/ppx-rewriter.t
   (progn (run dune-cram run run.t) (diff? run.t run.t.corrected))))
 (enabled_if (<> %{ocaml_version} 4.02.3)))

(rule
 (alias ppx-runtime-dependencies)
 (deps
  (package dune)
  (source_tree test-cases/ppx-runtime-dependencies.t)
  (alias test-deps))
 (action
  (chdir
   test-cases/ppx-runtime-dependencies.t
   (progn (run dune-cram run run.t) (diff? run.t run.t.corrected)))))

(rule
 (alias preprocess-with-action)
 (deps
  (package dune)
  (source_tree test-cases/preprocess-with-action.t)
  (alias test-deps))
 (action
  (chdir
   test-cases/preprocess-with-action.t
   (progn (run dune-cram run run.t) (diff? run.t run.t.corrected)))))

(rule
 (alias private-modules)
 (deps
  (package dune)
  (source_tree test-cases/private-modules.t)
  (alias test-deps))
 (action
  (chdir
   test-cases/private-modules.t
   (progn (run dune-cram run run.t) (diff? run.t run.t.corrected)))))

(rule
 (alias private-public-overlap)
 (deps
  (package dune)
  (source_tree test-cases/private-public-overlap.t)
  (alias test-deps))
 (action
  (chdir
   test-cases/private-public-overlap.t
   (progn (run dune-cram run run.t) (diff? run.t run.t.corrected)))))

(rule
 (alias project-root)
 (deps
  (package dune)
  (source_tree test-cases/project-root.t)
  (alias test-deps))
 (action
  (chdir
   test-cases/project-root.t
   (progn (run dune-cram run run.t) (diff? run.t run.t.corrected)))))

(rule
 (alias promote)
 (deps (package dune) (source_tree test-cases/promote.t) (alias test-deps))
 (action
  (chdir
   test-cases/promote.t
   (progn (run dune-cram run run.t) (diff? run.t run.t.corrected)))))

(rule
 (alias promote-directory)
 (deps
  (package dune)
  (source_tree test-cases/promote-directory)
  (alias test-deps))
 (action
  (chdir
   test-cases/promote-directory
   (progn (run dune-cram run run.t) (diff? run.t run.t.corrected)))))

(rule
 (alias quoting)
 (deps (package dune) (source_tree test-cases/quoting.t) (alias test-deps))
 (action
  (chdir
   test-cases/quoting.t
   (progn (run dune-cram run run.t) (diff? run.t run.t.corrected)))))

(rule
 (alias re-exported-deps)
 (deps
  (package dune)
  (source_tree test-cases/re-exported-deps.t)
  (alias test-deps))
 (action
  (chdir
   test-cases/re-exported-deps.t
   (progn (run dune-cram run run.t) (diff? run.t run.t.corrected)))))

(rule
 (alias reason)
 (deps (package dune) (source_tree test-cases/reason.t) (alias test-deps))
 (action
  (chdir
   test-cases/reason.t
   (progn (run dune-cram run run.t) (diff? run.t run.t.corrected)))))

(rule
 (alias redirections)
 (deps
  (package dune)
  (source_tree test-cases/redirections.t)
  (alias test-deps))
 (action
  (chdir
   test-cases/redirections.t
   (progn (run dune-cram run run.t) (diff? run.t run.t.corrected)))))

(rule
 (alias reporting-of-cycles)
 (deps
  (package dune)
  (source_tree test-cases/reporting-of-cycles.t)
  (alias test-deps))
 (action
  (chdir
   test-cases/reporting-of-cycles.t
   (progn (run dune-cram run run.t) (diff? run.t run.t.corrected)))))

(rule
 (alias rule-target-external)
 (deps
  (package dune)
  (source_tree test-cases/rule-target-external.t)
  (alias test-deps))
 (action
  (chdir
   test-cases/rule-target-external.t
   (progn (run dune-cram run run.t) (diff? run.t run.t.corrected)))))

(rule
 (alias rule-target-inferrence)
 (deps
  (package dune)
  (source_tree test-cases/rule-target-inferrence.t)
  (alias test-deps))
 (action
  (chdir
   test-cases/rule-target-inferrence.t
   (progn (run dune-cram run run.t) (diff? run.t run.t.corrected)))))

(rule
 (alias sandboxing)
 (deps (package dune) (source_tree test-cases/sandboxing.t) (alias test-deps))
 (action
  (chdir
   test-cases/sandboxing.t
   (progn (run dune-cram run run.t) (diff? run.t run.t.corrected)))))

(rule
 (alias scope-bug)
 (deps (package dune) (source_tree test-cases/scope-bug.t) (alias test-deps))
 (action
  (chdir
   test-cases/scope-bug.t
   (progn (run dune-cram run run.t) (diff? run.t run.t.corrected)))))

(rule
 (alias scope-ppx-bug)
 (deps
  (package dune)
  (source_tree test-cases/scope-ppx-bug.t)
  (alias test-deps))
 (action
  (chdir
   test-cases/scope-ppx-bug.t
   (progn (run dune-cram run run.t) (diff? run.t run.t.corrected)))))

(rule
 (alias select)
 (deps (package dune) (source_tree test-cases/select.t) (alias test-deps))
 (action
  (chdir
   test-cases/select.t
   (progn (run dune-cram run run.t) (diff? run.t run.t.corrected)))))

(rule
 (alias select-2-0-rules)
 (deps
  (package dune)
  (source_tree test-cases/select-2-0-rules.t)
  (alias test-deps))
 (action
  (chdir
   test-cases/select-2-0-rules.t
   (progn (run dune-cram run run.t) (diff? run.t run.t.corrected)))))

(rule
 (alias several-packages)
 (deps
  (package dune)
  (source_tree test-cases/several-packages.t)
  (alias test-deps))
 (action
  (chdir
   test-cases/several-packages.t
   (progn (run dune-cram run run.t) (diff? run.t run.t.corrected)))))

(rule
 (alias shadow-bindings)
 (deps
  (package dune)
  (source_tree test-cases/shadow-bindings.t)
  (alias test-deps))
 (action
  (chdir
   test-cases/shadow-bindings.t
   (progn (run dune-cram run run.t) (diff? run.t run.t.corrected)))))

(rule
 (alias stale-artifact-removal)
 (deps
  (package dune)
  (source_tree test-cases/stale-artifact-removal.t)
  (alias test-deps))
 (action
  (chdir
   test-cases/stale-artifact-removal.t
   (progn (run dune-cram run run.t) (diff? run.t run.t.corrected)))))

(rule
 (alias stdlib-compilation)
 (deps
  (package dune)
  (source_tree test-cases/stdlib-compilation.t)
  (alias test-deps))
 (action
  (chdir
   test-cases/stdlib-compilation.t
   (progn (run dune-cram run run.t) (diff? run.t run.t.corrected)))))

(rule
 (alias strict-package-deps)
 (deps
  (package dune)
  (source_tree test-cases/strict-package-deps.t)
  (alias test-deps))
 (action
  (chdir
   test-cases/strict-package-deps.t
   (progn (run dune-cram run run.t) (diff? run.t run.t.corrected)))))

(rule
 (alias subdir-stanza)
 (deps
  (package dune)
  (source_tree test-cases/subdir-stanza.t)
  (alias test-deps))
 (action
  (chdir
   test-cases/subdir-stanza.t
   (progn (run dune-cram run run.t) (diff? run.t run.t.corrected)))))

(rule
 (alias subst)
 (deps (package dune) (source_tree test-cases/subst.t) (alias test-deps))
 (action
  (chdir
   test-cases/subst.t
   (progn (run dune-cram run run.t) (diff? run.t run.t.corrected)))))

(rule
 (alias syntax-versioning)
 (deps
  (package dune)
  (source_tree test-cases/syntax-versioning.t)
  (alias test-deps))
 (action
  (chdir
   test-cases/syntax-versioning.t
   (progn (run dune-cram run run.t) (diff? run.t run.t.corrected)))))

(rule
 (alias target-dir-alias)
 (deps
  (package dune)
  (source_tree test-cases/target-dir-alias.t)
  (alias test-deps))
 (action
  (chdir
   test-cases/target-dir-alias.t
   (progn (run dune-cram run run.t) (diff? run.t run.t.corrected)))))

(rule
 (alias targets-with-vars)
 (deps
  (package dune)
  (source_tree test-cases/targets-with-vars.t)
  (alias test-deps))
 (action
  (chdir
   test-cases/targets-with-vars.t
   (progn (run dune-cram run run.t) (diff? run.t run.t.corrected)))))

(rule
 (alias tests-stanza)
 (deps
  (package dune)
  (source_tree test-cases/tests-stanza.t)
  (alias test-deps))
 (action
  (chdir
   test-cases/tests-stanza.t
   (progn (run dune-cram run run.t) (diff? run.t run.t.corrected)))))

(rule
 (alias tests-stanza-action)
 (deps
  (package dune)
  (source_tree test-cases/tests-stanza-action.t)
  (alias test-deps))
 (action
  (chdir
   test-cases/tests-stanza-action.t
   (progn (run dune-cram run run.t) (diff? run.t run.t.corrected)))))

(rule
 (alias tests-stanza-action-syntax-version)
 (deps
  (package dune)
  (source_tree test-cases/tests-stanza-action-syntax-version.t)
  (alias test-deps))
 (action
  (chdir
   test-cases/tests-stanza-action-syntax-version.t
   (progn (run dune-cram run run.t) (diff? run.t run.t.corrected)))))

(rule
 (alias toplevel-integration)
 (deps
  (package dune)
  (source_tree test-cases/toplevel-integration.t)
  (alias test-deps))
 (action
  (chdir
   test-cases/toplevel-integration.t
   (progn (run dune-cram run run.t) (diff? run.t run.t.corrected)))))

(rule
 (alias toplevel-stanza)
 (deps
  (package dune)
  (source_tree test-cases/toplevel-stanza.t)
  (alias test-deps))
 (action
  (chdir
   test-cases/toplevel-stanza.t
   (progn (run dune-cram run run.t) (diff? run.t run.t.corrected))))
 (enabled_if (>= %{ocaml_version} 4.05.0)))

(rule
 (alias trace-file)
 (deps (package dune) (source_tree test-cases/trace-file.t) (alias test-deps))
 (action
  (chdir
   test-cases/trace-file.t
   (progn (run dune-cram run run.t) (diff? run.t run.t.corrected)))))

(rule
 (alias transitive-deps-mode)
 (deps
  (package dune)
  (source_tree test-cases/transitive-deps-mode.t)
  (alias test-deps))
 (action
  (chdir
   test-cases/transitive-deps-mode.t
   (progn (run dune-cram run run.t) (diff? run.t run.t.corrected)))))

(rule
 (alias unreadable-src)
 (deps
  (package dune)
  (source_tree test-cases/unreadable-src.t)
  (alias test-deps))
 (action
  (chdir
   test-cases/unreadable-src.t
   (progn (run dune-cram run run.t) (diff? run.t run.t.corrected)))))

(rule
 (alias unused-preprocessor-deps)
 (deps
  (package dune)
  (source_tree test-cases/unused-preprocessor-deps.t)
  (alias test-deps))
 (action
  (chdir
   test-cases/unused-preprocessor-deps.t
   (progn (run dune-cram run run.t) (diff? run.t run.t.corrected)))))

(rule
 (alias upgrader)
 (deps (package dune) (source_tree test-cases/upgrader.t) (alias test-deps))
 (action
  (chdir
   test-cases/upgrader.t
   (progn (run dune-cram run run.t) (diff? run.t run.t.corrected)))))

(rule
 (alias use-meta)
 (deps (package dune) (source_tree test-cases/use-meta.t) (alias test-deps))
 (action
  (chdir
   test-cases/use-meta.t
   (progn (run dune-cram run run.t) (diff? run.t run.t.corrected)))))

(rule
 (alias utop-utop-default)
 (deps
  (package dune)
  (source_tree test-cases/utop/utop-default.t)
  (alias test-deps))
 (action
  (chdir
   test-cases/utop/utop-default.t
   (progn (run dune-cram run run.t) (diff? run.t run.t.corrected))))
 (enabled_if (>= %{ocaml_version} 4.05.0)))

(rule
 (alias utop-utop-default-implementation)
 (deps
  (package dune)
  (source_tree test-cases/utop/utop-default-implementation.t)
  (alias test-deps))
 (action
  (chdir
   test-cases/utop/utop-default-implementation.t
   (progn (run dune-cram run run.t) (diff? run.t run.t.corrected))))
 (enabled_if (>= %{ocaml_version} 4.05.0)))

(rule
 (alias utop-utop-ppx-rewriters)
 (deps
  (package dune)
  (source_tree test-cases/utop/utop-ppx-rewriters.t)
  (alias test-deps))
 (action
  (chdir
   test-cases/utop/utop-ppx-rewriters.t
   (progn (run dune-cram run run.t) (diff? run.t run.t.corrected))))
 (enabled_if (>= %{ocaml_version} 4.05.0)))

(rule
 (alias utop-utop-simple)
 (deps
  (package dune)
  (source_tree test-cases/utop/utop-simple.t)
  (alias test-deps))
 (action
  (chdir
   test-cases/utop/utop-simple.t
   (progn (run dune-cram run run.t) (diff? run.t run.t.corrected))))
 (enabled_if (>= %{ocaml_version} 4.05.0)))

(rule
 (alias variables-for-artifacts)
 (deps
  (package dune)
  (source_tree test-cases/variables-for-artifacts.t)
  (alias test-deps))
 (action
  (chdir
   test-cases/variables-for-artifacts.t
   (progn (run dune-cram run run.t) (diff? run.t run.t.corrected)))))

(rule
 (alias vendor-dash-p-and-vendored-packages)
 (deps
  (package dune)
  (source_tree test-cases/vendor/dash-p-and-vendored-packages)
  (alias test-deps))
 (action
  (chdir
   test-cases/vendor/dash-p-and-vendored-packages
   (progn (run dune-cram run run.t) (diff? run.t run.t.corrected)))))

(rule
 (alias vendor-do-not-install-vendored-packages)
 (deps
  (package dune)
  (source_tree test-cases/vendor/do-not-install-vendored-packages)
  (alias test-deps))
 (action
  (chdir
   test-cases/vendor/do-not-install-vendored-packages
   (progn (run dune-cram run run.t) (diff? run.t run.t.corrected)))))

(rule
 (alias vendor-main)
 (deps (package dune) (source_tree test-cases/vendor/main) (alias test-deps))
 (action
  (chdir
   test-cases/vendor/main
   (progn (run dune-cram run run.t) (diff? run.t run.t.corrected)))))

(rule
 (alias virtual-libraries-double-implementation)
 (deps
  (package dune)
  (source_tree test-cases/virtual-libraries/double-implementation)
  (alias test-deps))
 (action
  (chdir
   test-cases/virtual-libraries/double-implementation
   (progn (run dune-cram run run.t) (diff? run.t run.t.corrected)))))

(rule
 (alias virtual-libraries-dune-package-info)
 (deps
  (package dune)
  (source_tree test-cases/virtual-libraries/dune-package-info)
  (alias test-deps))
 (action
  (chdir
   test-cases/virtual-libraries/dune-package-info
   (progn (run dune-cram run run.t) (diff? run.t run.t.corrected)))))

(rule
 (alias virtual-libraries-github2896)
 (deps
  (package dune)
  (source_tree test-cases/virtual-libraries/github2896)
  (alias test-deps))
 (action
  (chdir
   test-cases/virtual-libraries/github2896
   (progn (run dune-cram run run.t) (diff? run.t run.t.corrected)))))

(rule
 (alias virtual-libraries-impl-lib-interface-module)
 (deps
  (package dune)
  (source_tree test-cases/virtual-libraries/impl-lib-interface-module)
  (alias test-deps))
 (action
  (chdir
   test-cases/virtual-libraries/impl-lib-interface-module
   (progn (run dune-cram run run.t) (diff? run.t run.t.corrected)))))

(rule
 (alias virtual-libraries-impl-not-virtual)
 (deps
  (package dune)
  (source_tree test-cases/virtual-libraries/impl-not-virtual)
  (alias test-deps))
 (action
  (chdir
   test-cases/virtual-libraries/impl-not-virtual
   (progn (run dune-cram run run.t) (diff? run.t run.t.corrected)))))

(rule
 (alias virtual-libraries-impl-not-virtual-external)
 (deps
  (package dune)
  (source_tree test-cases/virtual-libraries/impl-not-virtual-external)
  (alias test-deps))
 (action
  (chdir
   test-cases/virtual-libraries/impl-not-virtual-external
   (progn (run dune-cram run run.t) (diff? run.t run.t.corrected)))))

(rule
 (alias virtual-libraries-impl-private-modules)
 (deps
  (package dune)
  (source_tree test-cases/virtual-libraries/impl-private-modules)
  (alias test-deps))
 (action
  (chdir
   test-cases/virtual-libraries/impl-private-modules
   (progn (run dune-cram run run.t) (diff? run.t run.t.corrected)))))

(rule
 (alias virtual-libraries-impl-public-modules)
 (deps
  (package dune)
  (source_tree test-cases/virtual-libraries/impl-public-modules)
  (alias test-deps))
 (action
  (chdir
   test-cases/virtual-libraries/impl-public-modules
   (progn (run dune-cram run run.t) (diff? run.t run.t.corrected)))))

(rule
 (alias virtual-libraries-impl-using-vlib-modules)
 (deps
  (package dune)
  (source_tree test-cases/virtual-libraries/impl-using-vlib-modules)
  (alias test-deps))
 (action
  (chdir
   test-cases/virtual-libraries/impl-using-vlib-modules
   (progn (run dune-cram run run.t) (diff? run.t run.t.corrected)))))

(rule
 (alias virtual-libraries-implements-external)
 (deps
  (package dune)
  (source_tree test-cases/virtual-libraries/implements-external)
  (alias test-deps))
 (action
  (chdir
   test-cases/virtual-libraries/implements-external
   (progn (run dune-cram run run.t) (diff? run.t run.t.corrected)))))

(rule
 (alias virtual-libraries-install-file)
 (deps
  (package dune)
  (source_tree test-cases/virtual-libraries/install-file)
  (alias test-deps))
 (action
  (chdir
   test-cases/virtual-libraries/install-file
   (progn (run dune-cram run run.t) (diff? run.t run.t.corrected)))))

(rule
 (alias virtual-libraries-invalid-virtual-lib)
 (deps
  (package dune)
  (source_tree test-cases/virtual-libraries/invalid-virtual-lib)
  (alias test-deps))
 (action
  (chdir
   test-cases/virtual-libraries/invalid-virtual-lib
   (progn (run dune-cram run run.t) (diff? run.t run.t.corrected)))))

(rule
 (alias virtual-libraries-missing-implementation)
 (deps
  (package dune)
  (source_tree test-cases/virtual-libraries/missing-implementation)
  (alias test-deps))
 (action
  (chdir
   test-cases/virtual-libraries/missing-implementation
   (progn (run dune-cram run run.t) (diff? run.t run.t.corrected)))))

(rule
 (alias virtual-libraries-module-fields)
 (deps
  (package dune)
  (source_tree test-cases/virtual-libraries/module-fields)
  (alias test-deps))
 (action
  (chdir
   test-cases/virtual-libraries/module-fields
   (progn (run dune-cram run run.t) (diff? run.t run.t.corrected)))))

(rule
 (alias virtual-libraries-no-vlib-present)
 (deps
  (package dune)
  (source_tree test-cases/virtual-libraries/no-vlib-present)
  (alias test-deps))
 (action
  (chdir
   test-cases/virtual-libraries/no-vlib-present
   (progn (run dune-cram run run.t) (diff? run.t run.t.corrected)))))

(rule
 (alias virtual-libraries-preprocess)
 (deps
  (package dune)
  (source_tree test-cases/virtual-libraries/preprocess)
  (alias test-deps))
 (action
  (chdir
   test-cases/virtual-libraries/preprocess
   (progn (run dune-cram run run.t) (diff? run.t run.t.corrected)))))

(rule
 (alias virtual-libraries-private-modules-overlapping-names)
 (deps
  (package dune)
  (source_tree test-cases/virtual-libraries/private-modules-overlapping-names)
  (alias test-deps))
 (action
  (chdir
   test-cases/virtual-libraries/private-modules-overlapping-names
   (progn (run dune-cram run run.t) (diff? run.t run.t.corrected)))))

(rule
 (alias virtual-libraries-unwrapped)
 (deps
  (package dune)
  (source_tree test-cases/virtual-libraries/unwrapped)
  (alias test-deps))
 (action
  (chdir
   test-cases/virtual-libraries/unwrapped
   (progn (run dune-cram run run.t) (diff? run.t run.t.corrected)))))

(rule
 (alias virtual-libraries-variants-simple)
 (deps
  (package dune)
  (source_tree test-cases/virtual-libraries/variants-simple)
  (alias test-deps))
 (action
  (chdir
   test-cases/virtual-libraries/variants-simple
   (progn (run dune-cram run run.t) (diff? run.t run.t.corrected)))))

(rule
 (alias virtual-libraries-variants-sub-module)
 (deps
  (package dune)
  (source_tree test-cases/virtual-libraries/variants-sub-module)
  (alias test-deps))
 (action
  (chdir
   test-cases/virtual-libraries/variants-sub-module
   (progn (run dune-cram run run.t) (diff? run.t run.t.corrected)))))

(rule
 (alias virtual-libraries-vlib-default-impl)
 (deps
  (package dune)
  (source_tree test-cases/virtual-libraries/vlib-default-impl)
  (alias test-deps))
 (action
  (chdir
   test-cases/virtual-libraries/vlib-default-impl
   (progn (run dune-cram run run.t) (diff? run.t run.t.corrected)))))

(rule
 (alias virtual-libraries-vlib-wrong-default-impl)
 (deps
  (package dune)
  (source_tree test-cases/virtual-libraries/vlib-wrong-default-impl)
  (alias test-deps))
 (action
  (chdir
   test-cases/virtual-libraries/vlib-wrong-default-impl
   (progn (run dune-cram run run.t) (diff? run.t run.t.corrected)))))

(rule
 (alias windows-diff)
 (deps
  (package dune)
  (source_tree test-cases/windows-diff.t)
  (alias test-deps))
 (action
  (chdir
   test-cases/windows-diff.t
   (progn (run dune-cram run run.t) (diff? run.t run.t.corrected)))))

(rule
 (alias with-exit-codes)
 (deps
  (package dune)
  (source_tree test-cases/with-exit-codes.t)
  (alias test-deps))
 (action
  (chdir
   test-cases/with-exit-codes.t
   (progn (run dune-cram run run.t) (diff? run.t run.t.corrected)))))

(rule
 (alias with-nested-exit-codes)
 (deps
  (package dune)
  (source_tree test-cases/with-nested-exit-codes.t)
  (alias test-deps))
 (action
  (chdir
   test-cases/with-nested-exit-codes.t
   (progn (run dune-cram run run.t) (diff? run.t run.t.corrected)))))

(rule
 (alias with-stdin-from)
 (deps
  (package dune)
  (source_tree test-cases/with-stdin-from.t)
  (alias test-deps))
 (action
  (chdir
   test-cases/with-stdin-from.t
   (progn (run dune-cram run run.t) (diff? run.t run.t.corrected)))))

(rule
 (alias with-unsupported-nested-exit-codes)
 (deps
  (package dune)
  (source_tree test-cases/with-unsupported-nested-exit-codes.t)
  (alias test-deps))
 (action
  (chdir
   test-cases/with-unsupported-nested-exit-codes.t
   (progn (run dune-cram run run.t) (diff? run.t run.t.corrected)))))

(rule
 (alias workspace-paths)
 (deps
  (package dune)
  (source_tree test-cases/workspace-paths.t)
  (alias test-deps))
 (action
  (chdir
   test-cases/workspace-paths.t
   (progn (run dune-cram run run.t) (diff? run.t run.t.corrected)))))

(rule
 (alias workspaces)
 (deps (package dune) (source_tree test-cases/workspaces.t) (alias test-deps))
 (action
  (chdir
   test-cases/workspaces.t
   (progn (run dune-cram run run.t) (diff? run.t run.t.corrected)))))

(rule
 (alias wrapped-false-main-module-name)
 (deps
  (package dune)
  (source_tree test-cases/wrapped-false-main-module-name.t)
  (alias test-deps))
 (action
  (chdir
   test-cases/wrapped-false-main-module-name.t
   (progn (run dune-cram run run.t) (diff? run.t run.t.corrected)))))

(rule
 (alias wrapped-transition)
 (deps
  (package dune)
  (source_tree test-cases/wrapped-transition.t)
  (alias test-deps))
 (action
  (chdir
   test-cases/wrapped-transition.t
   (progn (run dune-cram run run.t) (diff? run.t run.t.corrected))))
 (enabled_if (>= %{ocaml_version} 4.06.0)))

(rule
 (alias write-permissions)
 (deps
  (package dune)
  (source_tree test-cases/write-permissions.t)
  (alias test-deps))
 (action
  (chdir
   test-cases/write-permissions.t
   (progn (run dune-cram run run.t) (diff? run.t run.t.corrected)))))

(alias
 (name runtest)
 (deps
  (alias action-modifying-a-dependency)
  (alias aliases)
  (alias all-alias)
  (alias all-promotions)
  (alias bad-alias-error)
  (alias bin-eager-deps)
  (alias block-strings)
  (alias byte-code-only)
  (alias byte_complete)
  (alias c-stubs)
  (alias check-alias)
  (alias cinaps-include-subdirs)
  (alias cinaps-simple)
  (alias cmdliner-dep-conf)
  (alias contents-depends-on-glob)
  (alias copy-files-non-sub-dir-error)
  (alias copy_files)
  (alias coq-github3624)
  (alias corrections)
  (alias cross-compilation)
  (alias custom-build-dir)
  (alias custom-cross-compilation)
  (alias cxx-extension)
  (alias cycle-detection-symlinks-2863)
  (alias default-implementation-package-mismatch1-package-mismatch1)
  (alias default-implementation-package-mismatch2-package-mismatch2)
  (alias default-targets)
  (alias dep-on-dir-that-does-not-exist)
  (alias dep-vars)
  (alias depend-on-the-universe)
  (alias deprecated-library-name)
  (alias deps-conf-vars)
  (alias describe)
  (alias dialects)
  (alias dir-target-dep)
  (alias disable-promotion)
  (alias double-echo)
  (alias dune-build-dir-exec-1101)
  (alias dune-cache-dedup)
  (alias dune-cache-dedup-direct)
  (alias dune-cache-dedup-direct-copy)
  (alias dune-cache-missing-data)
  (alias dune-cache-promote)
  (alias dune-cache-promote-copy)
  (alias dune-cache-promote-direct)
  (alias dune-cache-promote-direct-copy)
  (alias dune-cache-promotion-in-source-tree)
  (alias dune-cache-symlink)
  (alias dune-cache-symlink-direct)
  (alias dune-cache-trim)
  (alias dune-init)
  (alias dune-jbuild-var-case)
  (alias dune-package)
  (alias dune-ppx-driver-system)
  (alias dune-project-edition)
  (alias dune-project-meta-github2927)
  (alias dune-project-meta-main)
  (alias dune-project-no-opam)
  (alias dune_memory-and-the-universe)
  (alias dup-fields)
  (alias duplicate-c-cxx)
  (alias duplicate-c-cxx-obj)
  (alias duplicate-project-names)
  (alias duplicate-target-no-loc)
  (alias embed-jbuild)
  (alias empty-package-name)
  (alias enabled_if)
  (alias enabled_if-exec)
  (alias env-env-affects-expander-affects-env)
  (alias env-env-and-flags-include)
  (alias env-env-bin-pform)
  (alias env-env-bins)
  (alias env-env-cflags)
  (alias env-env-dune-file)
  (alias env-env-simple)
  (alias env-env-tracking)
  (alias env-env-var-expansion)
  (alias env-env-variables)
  (alias exclude-missing-module)
  (alias exe-name-mangle)
  (alias exec-cmd)
  (alias exec-missing)
  (alias exes-with-c)
  (alias extensions-versionning)
  (alias external-lib-deps-github3143)
  (alias external-lib-deps-missing)
  (alias external-lib-deps-simple)
  (alias extra-lang-line)
  (alias fallback-dune)
  (alias findlib)
  (alias findlib-dynload)
  (alias findlib-error)
  (alias forbidden_libraries)
  (alias force-test)
  (alias foreign-library)
  (alias foreign-stubs)
  (alias format-dune-file)
  (alias formatting)
  (alias gen-opam-install-file)
  (alias generated-source-dir-overlap)
  (alias github1019)
  (alias github1099)
  (alias github1231)
  (alias github1342)
  (alias github1372)
  (alias github1395)
  (alias github1426)
  (alias github1529)
  (alias github1541)
  (alias github1549)
  (alias github1560)
  (alias github1616)
  (alias github1811)
  (alias github1855)
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
  (alias github2499)
  (alias github25)
  (alias github2584)
  (alias github2629)
  (alias github2681)
  (alias github2848)
  (alias github2990)
  (alias github3043)
  (alias github3046)
  (alias github3180)
  (alias github3188)
  (alias github3440)
  (alias github3490)
  (alias github3530)
  (alias github534)
  (alias github568)
  (alias github597)
  (alias github644)
  (alias github660)
  (alias github734)
  (alias github759)
  (alias github761)
  (alias github764)
  (alias github784)
  (alias github992)
  (alias glob-deps)
  (alias glob-deps-change)
  (alias hints-root-executables)
  (alias ignored_subdirs)
  (alias include-loop)
  (alias include-qualified)
  (alias incremental-rebuilds)
  (alias inline_tests-dune-file)
  (alias inline_tests-many-backends-choose)
  (alias inline_tests-missing-backend)
  (alias inline_tests-multi-mode)
  (alias inline_tests-multiple-inline-tests)
  (alias inline_tests-simple)
  (alias inline_tests-too-many-backends)
  (alias install-dry-run)
  (alias install-libdir)
  (alias install-mandir)
  (alias install-multiple-contexts)
  (alias install-partial-package)
  (alias install-rule-order)
  (alias install-single-section)
  (alias install-with-var)
  (alias installable-dup-private-libs)
  (alias instrumentation)
  (alias intf-only)
  (alias invalid-dune-package)
  (alias invalid-module-name)
  (alias invalid-pform)
  (alias jsoo-github3622)
  (alias jsoo-inline-tests)
  (alias jsoo-simple)
  (alias lib)
  (alias lib-available)
  (alias lib-errors)
  (alias link-deps)
  (alias link-includes)
  (alias lint)
  (alias loop)
  (alias macro-expand-error)
  (alias mdx-stanza)
  (alias menhir-cmly)
  (alias menhir-env)
  (alias menhir-general)
  (alias menhir-general-2.0)
  (alias menhir-promote)
  (alias merlin-allow_approximate_merlin)
  (alias merlin-merlin-tests)
  (alias merlin-server)
  (alias merlin-src-dirs-of-deps)
  (alias meta-gen)
  (alias misc)
  (alias missing-loc-run)
  (alias multi-dir)
  (alias multiple-targets)
  (alias name-field-validation)
  (alias no-infer)
  (alias no-installable-mode)
  (alias null-dep)
  (alias ocaml-config-macro)
  (alias ocaml-syntax)
  (alias ocamldep-multi-stanzas)
  (alias odig-files-workspace)
  (alias odoc-github717-odoc-index)
  (alias odoc-multiple-private-libs)
  (alias odoc-odoc-package-mld-link)
  (alias odoc-odoc-simple)
  (alias odoc-odoc-unique-mlds)
  (alias odoc-warnings)
  (alias old-dune-subsystem)
  (alias optional)
  (alias optional-executable)
  (alias output-obj)
  (alias package-dep)
  (alias path-variables)
  (alias pipe-actions)
  (alias plugin-mode)
  (alias ppx-rewriter)
  (alias ppx-runtime-dependencies)
  (alias preprocess-with-action)
  (alias private-modules)
  (alias private-public-overlap)
  (alias project-root)
  (alias promote)
  (alias promote-directory)
  (alias quoting)
  (alias re-exported-deps)
  (alias reason)
  (alias redirections)
  (alias reporting-of-cycles)
  (alias rule-target-external)
  (alias rule-target-inferrence)
  (alias sandboxing)
  (alias scope-bug)
  (alias scope-ppx-bug)
  (alias select)
  (alias select-2-0-rules)
  (alias several-packages)
  (alias shadow-bindings)
  (alias stale-artifact-removal)
  (alias stdlib-compilation)
  (alias strict-package-deps)
  (alias subdir-stanza)
  (alias subst)
  (alias syntax-versioning)
  (alias target-dir-alias)
  (alias targets-with-vars)
  (alias tests-stanza)
  (alias tests-stanza-action)
  (alias tests-stanza-action-syntax-version)
  (alias toplevel-integration)
  (alias toplevel-stanza)
  (alias trace-file)
  (alias transitive-deps-mode)
  (alias unreadable-src)
  (alias unused-preprocessor-deps)
  (alias upgrader)
  (alias use-meta)
  (alias utop-utop-default)
  (alias utop-utop-default-implementation)
  (alias utop-utop-ppx-rewriters)
  (alias utop-utop-simple)
  (alias variables-for-artifacts)
  (alias vendor-dash-p-and-vendored-packages)
  (alias vendor-do-not-install-vendored-packages)
  (alias vendor-main)
  (alias virtual-libraries-double-implementation)
  (alias virtual-libraries-dune-package-info)
  (alias virtual-libraries-github2896)
  (alias virtual-libraries-impl-lib-interface-module)
  (alias virtual-libraries-impl-not-virtual)
  (alias virtual-libraries-impl-not-virtual-external)
  (alias virtual-libraries-impl-private-modules)
  (alias virtual-libraries-impl-public-modules)
  (alias virtual-libraries-impl-using-vlib-modules)
  (alias virtual-libraries-implements-external)
  (alias virtual-libraries-install-file)
  (alias virtual-libraries-invalid-virtual-lib)
  (alias virtual-libraries-missing-implementation)
  (alias virtual-libraries-module-fields)
  (alias virtual-libraries-no-vlib-present)
  (alias virtual-libraries-preprocess)
  (alias virtual-libraries-private-modules-overlapping-names)
  (alias virtual-libraries-unwrapped)
  (alias virtual-libraries-variants-simple)
  (alias virtual-libraries-variants-sub-module)
  (alias virtual-libraries-vlib-default-impl)
  (alias virtual-libraries-vlib-wrong-default-impl)
  (alias windows-diff)
  (alias with-exit-codes)
  (alias with-nested-exit-codes)
  (alias with-stdin-from)
  (alias with-unsupported-nested-exit-codes)
  (alias workspace-paths)
  (alias workspaces)
  (alias wrapped-false-main-module-name)
  (alias wrapped-transition)
  (alias write-permissions)))

(alias
 (name runtest-no-deps)
 (deps
  (alias action-modifying-a-dependency)
  (alias aliases)
  (alias all-alias)
  (alias all-promotions)
  (alias bad-alias-error)
  (alias bin-eager-deps)
  (alias block-strings)
  (alias byte-code-only)
  (alias byte_complete)
  (alias c-stubs)
  (alias check-alias)
  (alias cmdliner-dep-conf)
  (alias contents-depends-on-glob)
  (alias copy-files-non-sub-dir-error)
  (alias copy_files)
  (alias coq-github3624)
  (alias corrections)
  (alias custom-build-dir)
  (alias custom-cross-compilation)
  (alias cxx-extension)
  (alias cycle-detection-symlinks-2863)
  (alias default-implementation-package-mismatch1-package-mismatch1)
  (alias default-implementation-package-mismatch2-package-mismatch2)
  (alias default-targets)
  (alias dep-on-dir-that-does-not-exist)
  (alias dep-vars)
  (alias depend-on-the-universe)
  (alias deprecated-library-name)
  (alias deps-conf-vars)
  (alias describe)
  (alias dialects)
  (alias dir-target-dep)
  (alias disable-promotion)
  (alias double-echo)
  (alias dune-build-dir-exec-1101)
  (alias dune-cache-dedup)
  (alias dune-cache-dedup-direct)
  (alias dune-cache-dedup-direct-copy)
  (alias dune-cache-missing-data)
  (alias dune-cache-promote)
  (alias dune-cache-promote-copy)
  (alias dune-cache-promote-direct)
  (alias dune-cache-promote-direct-copy)
  (alias dune-cache-promotion-in-source-tree)
  (alias dune-cache-symlink)
  (alias dune-cache-symlink-direct)
  (alias dune-cache-trim)
  (alias dune-init)
  (alias dune-jbuild-var-case)
  (alias dune-package)
  (alias dune-project-edition)
  (alias dune-project-meta-github2927)
  (alias dune-project-meta-main)
  (alias dune-project-no-opam)
  (alias dune_memory-and-the-universe)
  (alias dup-fields)
  (alias duplicate-c-cxx)
  (alias duplicate-c-cxx-obj)
  (alias duplicate-project-names)
  (alias duplicate-target-no-loc)
  (alias embed-jbuild)
  (alias empty-package-name)
  (alias enabled_if)
  (alias enabled_if-exec)
  (alias env-env-affects-expander-affects-env)
  (alias env-env-and-flags-include)
  (alias env-env-bin-pform)
  (alias env-env-bins)
  (alias env-env-cflags)
  (alias env-env-dune-file)
  (alias env-env-simple)
  (alias env-env-tracking)
  (alias env-env-var-expansion)
  (alias env-env-variables)
  (alias exclude-missing-module)
  (alias exe-name-mangle)
  (alias exec-cmd)
  (alias exec-missing)
  (alias exes-with-c)
  (alias extensions-versionning)
  (alias external-lib-deps-github3143)
  (alias external-lib-deps-missing)
  (alias external-lib-deps-simple)
  (alias extra-lang-line)
  (alias fallback-dune)
  (alias findlib)
  (alias findlib-error)
  (alias forbidden_libraries)
  (alias force-test)
  (alias foreign-stubs)
  (alias format-dune-file)
  (alias formatting)
  (alias generated-source-dir-overlap)
  (alias github1019)
  (alias github1099)
  (alias github1231)
  (alias github1342)
  (alias github1395)
  (alias github1426)
  (alias github1529)
  (alias github1541)
  (alias github1549)
  (alias github1560)
  (alias github1616)
  (alias github1811)
  (alias github1855)
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
  (alias github2499)
  (alias github25)
  (alias github2584)
  (alias github2629)
  (alias github2681)
  (alias github2848)
  (alias github2990)
  (alias github3043)
  (alias github3046)
  (alias github3180)
  (alias github3440)
  (alias github3490)
  (alias github3530)
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
  (alias glob-deps-change)
  (alias hints-root-executables)
  (alias ignored_subdirs)
  (alias include-loop)
  (alias include-qualified)
  (alias incremental-rebuilds)
  (alias inline_tests-dune-file)
  (alias inline_tests-many-backends-choose)
  (alias inline_tests-missing-backend)
  (alias inline_tests-multi-mode)
  (alias inline_tests-multiple-inline-tests)
  (alias inline_tests-simple)
  (alias inline_tests-too-many-backends)
  (alias install-mandir)
  (alias install-multiple-contexts)
  (alias install-partial-package)
  (alias install-rule-order)
  (alias install-single-section)
  (alias install-with-var)
  (alias installable-dup-private-libs)
  (alias intf-only)
  (alias invalid-dune-package)
  (alias invalid-module-name)
  (alias invalid-pform)
  (alias lib)
  (alias lib-available)
  (alias lib-errors)
  (alias link-deps)
  (alias link-includes)
  (alias loop)
  (alias macro-expand-error)
  (alias menhir-env)
  (alias merlin-allow_approximate_merlin)
  (alias merlin-server)
  (alias merlin-src-dirs-of-deps)
  (alias meta-gen)
  (alias misc)
  (alias missing-loc-run)
  (alias multi-dir)
  (alias multiple-targets)
  (alias name-field-validation)
  (alias no-infer)
  (alias no-installable-mode)
  (alias null-dep)
  (alias ocaml-config-macro)
  (alias ocaml-syntax)
  (alias ocamldep-multi-stanzas)
  (alias odig-files-workspace)
  (alias old-dune-subsystem)
  (alias optional)
  (alias optional-executable)
  (alias output-obj)
  (alias path-variables)
  (alias pipe-actions)
  (alias plugin-mode)
  (alias preprocess-with-action)
  (alias private-modules)
  (alias project-root)
  (alias promote)
  (alias promote-directory)
  (alias quoting)
  (alias re-exported-deps)
  (alias redirections)
  (alias reporting-of-cycles)
  (alias rule-target-external)
  (alias rule-target-inferrence)
  (alias sandboxing)
  (alias scope-bug)
  (alias select)
  (alias select-2-0-rules)
  (alias several-packages)
  (alias shadow-bindings)
  (alias stale-artifact-removal)
  (alias stdlib-compilation)
  (alias strict-package-deps)
  (alias subdir-stanza)
  (alias subst)
  (alias syntax-versioning)
  (alias target-dir-alias)
  (alias targets-with-vars)
  (alias tests-stanza)
  (alias tests-stanza-action)
  (alias tests-stanza-action-syntax-version)
  (alias trace-file)
  (alias transitive-deps-mode)
  (alias unreadable-src)
  (alias unused-preprocessor-deps)
  (alias upgrader)
  (alias variables-for-artifacts)
  (alias vendor-dash-p-and-vendored-packages)
  (alias vendor-do-not-install-vendored-packages)
  (alias vendor-main)
  (alias virtual-libraries-double-implementation)
  (alias virtual-libraries-dune-package-info)
  (alias virtual-libraries-github2896)
  (alias virtual-libraries-impl-lib-interface-module)
  (alias virtual-libraries-impl-not-virtual)
  (alias virtual-libraries-impl-not-virtual-external)
  (alias virtual-libraries-impl-private-modules)
  (alias virtual-libraries-impl-public-modules)
  (alias virtual-libraries-impl-using-vlib-modules)
  (alias virtual-libraries-implements-external)
  (alias virtual-libraries-install-file)
  (alias virtual-libraries-invalid-virtual-lib)
  (alias virtual-libraries-missing-implementation)
  (alias virtual-libraries-module-fields)
  (alias virtual-libraries-no-vlib-present)
  (alias virtual-libraries-preprocess)
  (alias virtual-libraries-private-modules-overlapping-names)
  (alias virtual-libraries-unwrapped)
  (alias virtual-libraries-variants-simple)
  (alias virtual-libraries-variants-sub-module)
  (alias virtual-libraries-vlib-default-impl)
  (alias virtual-libraries-vlib-wrong-default-impl)
  (alias windows-diff)
  (alias with-exit-codes)
  (alias with-nested-exit-codes)
  (alias with-stdin-from)
  (alias with-unsupported-nested-exit-codes)
  (alias workspace-paths)
  (alias workspaces)
  (alias wrapped-false-main-module-name)
  (alias wrapped-transition)
  (alias write-permissions)))

(alias
 (name runtest-disabled)
 (deps
  (alias bisect-ppx-github3473)
  (alias bisect-ppx-main)
  (alias env-envs-and-contexts)
  (alias fdo)))

(alias (name runtest-js) (deps (alias explicit_js_mode)))

(alias (name runtest-coq) (deps (alias coq-extract) (alias coq-main)))