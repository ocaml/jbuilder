(rule
 (alias action-modifying-a-dependency)
 (deps (package dune) (source_tree test-cases/action-modifying-a-dependency))
 (action
  (chdir
   test-cases/action-modifying-a-dependency
   (progn (run dune-cram run run.t) (diff? run.t run.t.corrected)))))

(rule
 (alias aliases)
 (deps (package dune) (source_tree test-cases/aliases))
 (action
  (chdir
   test-cases/aliases
   (progn (run dune-cram run run.t) (diff? run.t run.t.corrected)))))

(rule
 (alias all-alias)
 (deps (package dune) (source_tree test-cases/all-alias))
 (action
  (chdir
   test-cases/all-alias
   (progn (run dune-cram run run.t) (diff? run.t run.t.corrected)))))

(rule
 (alias all-promotions)
 (deps (package dune) (source_tree test-cases/all-promotions))
 (action
  (chdir
   test-cases/all-promotions
   (progn (run dune-cram run run.t) (diff? run.t run.t.corrected)))))

(rule
 (alias bad-alias-error)
 (deps (package dune) (source_tree test-cases/bad-alias-error))
 (action
  (chdir
   test-cases/bad-alias-error
   (progn (run dune-cram run run.t) (diff? run.t run.t.corrected)))))

(rule
 (alias bin-eager-deps)
 (deps (package dune) (source_tree test-cases/bin-eager-deps))
 (action
  (chdir
   test-cases/bin-eager-deps
   (progn (run dune-cram run run.t) (diff? run.t run.t.corrected)))))

(rule
 (alias block-strings)
 (deps (package dune) (source_tree test-cases/block-strings))
 (action
  (chdir
   test-cases/block-strings
   (progn (run dune-cram run run.t) (diff? run.t run.t.corrected)))))

(rule
 (alias byte-code-only)
 (deps (package dune) (source_tree test-cases/byte-code-only))
 (action
  (chdir
   test-cases/byte-code-only
   (progn (run dune-cram run run.t) (diff? run.t run.t.corrected)))))

(rule
 (alias byte_complete)
 (deps (package dune) (source_tree test-cases/byte_complete))
 (action
  (chdir
   test-cases/byte_complete
   (progn (run dune-cram run run.t) (diff? run.t run.t.corrected)))))

(rule
 (alias c-stubs)
 (deps (package dune) (source_tree test-cases/c-stubs))
 (action
  (chdir
   test-cases/c-stubs
   (progn (run dune-cram run run.t) (diff? run.t run.t.corrected)))))

(rule
 (alias check-alias)
 (deps (package dune) (source_tree test-cases/check-alias))
 (action
  (chdir
   test-cases/check-alias
   (progn (run dune-cram run run.t) (diff? run.t run.t.corrected)))))

(rule
 (alias cinaps)
 (deps (package dune) (source_tree test-cases/cinaps))
 (action
  (chdir
   test-cases/cinaps
   (progn (run dune-cram run run.t) (diff? run.t run.t.corrected)))))

(rule
 (alias cmdliner-dep-conf)
 (deps (package dune) (source_tree test-cases/cmdliner-dep-conf))
 (action
  (chdir
   test-cases/cmdliner-dep-conf
   (progn (run dune-cram run run.t) (diff? run.t run.t.corrected)))))

(rule
 (alias contents-depends-on-glob)
 (deps (package dune) (source_tree test-cases/contents-depends-on-glob))
 (action
  (chdir
   test-cases/contents-depends-on-glob
   (progn (run dune-cram run run.t) (diff? run.t run.t.corrected)))))

(rule
 (alias copy-files-non-sub-dir-error)
 (deps (package dune) (source_tree test-cases/copy-files-non-sub-dir-error))
 (action
  (chdir
   test-cases/copy-files-non-sub-dir-error
   (progn (run dune-cram run run.t) (diff? run.t run.t.corrected)))))

(rule
 (alias copy_files)
 (deps (package dune) (source_tree test-cases/copy_files))
 (action
  (chdir
   test-cases/copy_files
   (progn (run dune-cram run run.t) (diff? run.t run.t.corrected)))))

(rule
 (alias coq)
 (deps (package dune) (source_tree test-cases/coq))
 (action
  (chdir
   test-cases/coq
   (progn (run dune-cram run run.t) (diff? run.t run.t.corrected)))))

(rule
 (alias coq-extract)
 (deps (package dune) (source_tree test-cases/coq/extract))
 (action
  (chdir
   test-cases/coq/extract
   (progn (run dune-cram run run.t) (diff? run.t run.t.corrected)))))

(rule
 (alias corrections)
 (deps (package dune) (source_tree test-cases/corrections))
 (action
  (chdir
   test-cases/corrections
   (progn (run dune-cram run run.t) (diff? run.t run.t.corrected)))))

(rule
 (alias cross-compilation)
 (deps (package dune) (source_tree test-cases/cross-compilation))
 (action
  (chdir
   test-cases/cross-compilation
   (progn (run dune-cram run run.t) (diff? run.t run.t.corrected)))))

(rule
 (alias custom-build-dir)
 (deps (package dune) (source_tree test-cases/custom-build-dir))
 (action
  (chdir
   test-cases/custom-build-dir
   (progn (run dune-cram run run.t) (diff? run.t run.t.corrected)))))

(rule
 (alias custom-cross-compilation)
 (deps (package dune) (source_tree test-cases/custom-cross-compilation))
 (action
  (chdir
   test-cases/custom-cross-compilation
   (progn (run dune-cram run run.t) (diff? run.t run.t.corrected)))))

(rule
 (alias cxx-extension)
 (deps (package dune) (source_tree test-cases/cxx-extension))
 (action
  (chdir
   test-cases/cxx-extension
   (progn (run dune-cram run run.t) (diff? run.t run.t.corrected)))))

(rule
 (alias cycle-detection-symlinks-2863)
 (deps (package dune) (source_tree test-cases/cycle-detection-symlinks-2863))
 (action
  (chdir
   test-cases/cycle-detection-symlinks-2863
   (progn (run dune-cram run run.t) (diff? run.t run.t.corrected)))))

(rule
 (alias default-targets)
 (deps (package dune) (source_tree test-cases/default-targets))
 (action
  (chdir
   test-cases/default-targets
   (progn (run dune-cram run run.t) (diff? run.t run.t.corrected)))))

(rule
 (alias dep-on-dir-that-does-not-exist)
 (deps (package dune) (source_tree test-cases/dep-on-dir-that-does-not-exist))
 (action
  (chdir
   test-cases/dep-on-dir-that-does-not-exist
   (progn (run dune-cram run run.t) (diff? run.t run.t.corrected)))))

(rule
 (alias dep-vars)
 (deps (package dune) (source_tree test-cases/dep-vars))
 (action
  (chdir
   test-cases/dep-vars
   (progn (run dune-cram run run.t) (diff? run.t run.t.corrected)))))

(rule
 (alias depend-on-the-universe)
 (deps (package dune) (source_tree test-cases/depend-on-the-universe))
 (action
  (chdir
   test-cases/depend-on-the-universe
   (progn (run dune-cram run run.t) (diff? run.t run.t.corrected)))))

(rule
 (alias deprecated-library-name)
 (deps (package dune) (source_tree test-cases/deprecated-library-name))
 (action
  (chdir
   test-cases/deprecated-library-name
   (progn (run dune-cram run run.t) (diff? run.t run.t.corrected)))))

(rule
 (alias deps-conf-vars)
 (deps (package dune) (source_tree test-cases/deps-conf-vars))
 (action
  (chdir
   test-cases/deps-conf-vars
   (progn (run dune-cram run run.t) (diff? run.t run.t.corrected)))))

(rule
 (alias describe)
 (deps (package dune) (source_tree test-cases/describe))
 (action
  (chdir
   test-cases/describe
   (progn (run dune-cram run run.t) (diff? run.t run.t.corrected)))))

(rule
 (alias dialects)
 (deps (package dune) (source_tree test-cases/dialects))
 (action
  (chdir
   test-cases/dialects
   (progn (run dune-cram run run.t) (diff? run.t run.t.corrected)))))

(rule
 (alias dir-target-dep)
 (deps (package dune) (source_tree test-cases/dir-target-dep))
 (action
  (chdir
   test-cases/dir-target-dep
   (progn (run dune-cram run run.t) (diff? run.t run.t.corrected)))))

(rule
 (alias disable-promotion)
 (deps (package dune) (source_tree test-cases/disable-promotion))
 (action
  (chdir
   test-cases/disable-promotion
   (progn (run dune-cram run run.t) (diff? run.t run.t.corrected)))))

(rule
 (alias double-echo)
 (deps (package dune) (source_tree test-cases/double-echo))
 (action
  (chdir
   test-cases/double-echo
   (progn (run dune-cram run run.t) (diff? run.t run.t.corrected)))))

(rule
 (alias dune-build-dir-exec-1101)
 (deps (package dune) (source_tree test-cases/dune-build-dir-exec-1101))
 (action
  (chdir
   test-cases/dune-build-dir-exec-1101
   (progn (run dune-cram run run.t) (diff? run.t run.t.corrected)))))

(rule
 (alias dune-cache-dedup)
 (deps (package dune) (source_tree test-cases/dune-cache/dedup))
 (action
  (chdir
   test-cases/dune-cache/dedup
   (progn (run dune-cram run run.t) (diff? run.t run.t.corrected)))))

(rule
 (alias dune-cache-dedup-direct)
 (deps (package dune) (source_tree test-cases/dune-cache/dedup-direct))
 (action
  (chdir
   test-cases/dune-cache/dedup-direct
   (progn (run dune-cram run run.t) (diff? run.t run.t.corrected)))))

(rule
 (alias dune-cache-dedup-direct-copy)
 (deps (package dune) (source_tree test-cases/dune-cache/dedup-direct-copy))
 (action
  (chdir
   test-cases/dune-cache/dedup-direct-copy
   (progn (run dune-cram run run.t) (diff? run.t run.t.corrected)))))

(rule
 (alias dune-cache-promote)
 (deps (package dune) (source_tree test-cases/dune-cache/promote))
 (action
  (chdir
   test-cases/dune-cache/promote
   (progn (run dune-cram run run.t) (diff? run.t run.t.corrected)))))

(rule
 (alias dune-cache-promote-copy)
 (deps (package dune) (source_tree test-cases/dune-cache/promote-copy))
 (action
  (chdir
   test-cases/dune-cache/promote-copy
   (progn (run dune-cram run run.t) (diff? run.t run.t.corrected)))))

(rule
 (alias dune-cache-promote-direct)
 (deps (package dune) (source_tree test-cases/dune-cache/promote-direct))
 (action
  (chdir
   test-cases/dune-cache/promote-direct
   (progn (run dune-cram run run.t) (diff? run.t run.t.corrected)))))

(rule
 (alias dune-cache-promote-direct-copy)
 (deps (package dune) (source_tree test-cases/dune-cache/promote-direct-copy))
 (action
  (chdir
   test-cases/dune-cache/promote-direct-copy
   (progn (run dune-cram run run.t) (diff? run.t run.t.corrected)))))

(rule
 (alias dune-cache-promotion-in-source-tree)
 (deps
  (package dune)
  (source_tree test-cases/dune-cache/promotion-in-source-tree))
 (action
  (chdir
   test-cases/dune-cache/promotion-in-source-tree
   (progn (run dune-cram run run.t) (diff? run.t run.t.corrected)))))

(rule
 (alias dune-cache-symlink)
 (deps (package dune) (source_tree test-cases/dune-cache/symlink))
 (action
  (chdir
   test-cases/dune-cache/symlink
   (progn (run dune-cram run run.t) (diff? run.t run.t.corrected)))))

(rule
 (alias dune-cache-symlink-direct)
 (deps (package dune) (source_tree test-cases/dune-cache/symlink-direct))
 (action
  (chdir
   test-cases/dune-cache/symlink-direct
   (progn (run dune-cram run run.t) (diff? run.t run.t.corrected)))))

(rule
 (alias dune-cache-trim)
 (deps (package dune) (source_tree test-cases/dune-cache/trim))
 (action
  (chdir
   test-cases/dune-cache/trim
   (progn (run dune-cram run run.t) (diff? run.t run.t.corrected)))))

(rule
 (alias dune-init)
 (deps (package dune) (source_tree test-cases/dune-init))
 (action
  (chdir
   test-cases/dune-init
   (progn (run dune-cram run run.t) (diff? run.t run.t.corrected)))))

(rule
 (alias dune-jbuild-var-case)
 (deps (package dune) (source_tree test-cases/dune-jbuild-var-case))
 (action
  (chdir
   test-cases/dune-jbuild-var-case
   (progn (run dune-cram run run.t) (diff? run.t run.t.corrected)))))

(rule
 (alias dune-package)
 (deps (package dune) (source_tree test-cases/dune-package))
 (action
  (chdir
   test-cases/dune-package
   (progn (run dune-cram run run.t) (diff? run.t run.t.corrected)))))

(rule
 (alias dune-ppx-driver-system)
 (deps (package dune) (source_tree test-cases/dune-ppx-driver-system))
 (action
  (chdir
   test-cases/dune-ppx-driver-system
   (progn (run dune-cram run run.t) (diff? run.t run.t.corrected)))))

(rule
 (alias dune-project-edition)
 (deps (package dune) (source_tree test-cases/dune-project-edition))
 (action
  (chdir
   test-cases/dune-project-edition
   (progn (run dune-cram run run.t) (diff? run.t run.t.corrected)))))

(rule
 (alias dune-project-meta)
 (deps (package dune) (source_tree test-cases/dune-project-meta))
 (action
  (chdir
   test-cases/dune-project-meta
   (progn (run dune-cram run run.t) (diff? run.t run.t.corrected)))))

(rule
 (alias dune-project-meta-github2927)
 (deps (package dune) (source_tree test-cases/dune-project-meta/github2927))
 (action
  (chdir
   test-cases/dune-project-meta/github2927
   (progn (run dune-cram run run.t) (diff? run.t run.t.corrected)))))

(rule
 (alias dune-project-no-opam)
 (deps (package dune) (source_tree test-cases/dune-project-no-opam))
 (action
  (chdir
   test-cases/dune-project-no-opam
   (progn (run dune-cram run run.t) (diff? run.t run.t.corrected)))))

(rule
 (alias dune_memory-and-the-universe)
 (deps (package dune) (source_tree test-cases/dune_memory-and-the-universe))
 (action
  (chdir
   test-cases/dune_memory-and-the-universe
   (progn (run dune-cram run run.t) (diff? run.t run.t.corrected)))))

(rule
 (alias dup-fields)
 (deps (package dune) (source_tree test-cases/dup-fields))
 (action
  (chdir
   test-cases/dup-fields
   (progn (run dune-cram run run.t) (diff? run.t run.t.corrected)))))

(rule
 (alias duplicate-c-cxx)
 (deps (package dune) (source_tree test-cases/duplicate-c-cxx))
 (action
  (chdir
   test-cases/duplicate-c-cxx
   (progn (run dune-cram run run.t) (diff? run.t run.t.corrected)))))

(rule
 (alias duplicate-c-cxx-obj)
 (deps (package dune) (source_tree test-cases/duplicate-c-cxx-obj))
 (action
  (chdir
   test-cases/duplicate-c-cxx-obj
   (progn (run dune-cram run run.t) (diff? run.t run.t.corrected)))))

(rule
 (alias duplicate-project-names)
 (deps (package dune) (source_tree test-cases/duplicate-project-names))
 (action
  (chdir
   test-cases/duplicate-project-names
   (progn (run dune-cram run run.t) (diff? run.t run.t.corrected)))))

(rule
 (alias duplicate-target-no-loc)
 (deps (package dune) (source_tree test-cases/duplicate-target-no-loc))
 (action
  (chdir
   test-cases/duplicate-target-no-loc
   (progn (run dune-cram run run.t) (diff? run.t run.t.corrected)))))

(rule
 (alias embed-jbuild)
 (deps (package dune) (source_tree test-cases/embed-jbuild))
 (action
  (chdir
   test-cases/embed-jbuild
   (progn (run dune-cram run run.t) (diff? run.t run.t.corrected)))))

(rule
 (alias enabled_if)
 (deps (package dune) (source_tree test-cases/enabled_if))
 (action
  (chdir
   test-cases/enabled_if
   (progn (run dune-cram run run.t) (diff? run.t run.t.corrected)))))

(rule
 (alias enabled_if-exec)
 (deps (package dune) (source_tree test-cases/enabled_if-exec))
 (action
  (chdir
   test-cases/enabled_if-exec
   (progn (run dune-cram run run.t) (diff? run.t run.t.corrected)))))

(rule
 (alias env-env-affects-expander-affects-env)
 (deps
  (package dune)
  (source_tree test-cases/env/env-affects-expander-affects-env))
 (action
  (chdir
   test-cases/env/env-affects-expander-affects-env
   (progn (run dune-cram run run.t) (diff? run.t run.t.corrected)))))

(rule
 (alias env-env-and-flags-include)
 (deps (package dune) (source_tree test-cases/env/env-and-flags-include))
 (action
  (chdir
   test-cases/env/env-and-flags-include
   (progn (run dune-cram run run.t) (diff? run.t run.t.corrected)))))

(rule
 (alias env-env-bin-pform)
 (deps (package dune) (source_tree test-cases/env/env-bin-pform))
 (action
  (chdir
   test-cases/env/env-bin-pform
   (progn (run dune-cram run run.t) (diff? run.t run.t.corrected)))))

(rule
 (alias env-env-bins)
 (deps (package dune) (source_tree test-cases/env/env-bins) (sandbox none))
 (action
  (chdir
   test-cases/env/env-bins
   (progn (run dune-cram run run.t) (diff? run.t run.t.corrected)))))

(rule
 (alias env-env-cflags)
 (deps (package dune) (source_tree test-cases/env/env-cflags))
 (action
  (chdir
   test-cases/env/env-cflags
   (progn (run dune-cram run run.t) (diff? run.t run.t.corrected))))
 (enabled_if (>= %{ocaml_version} 4.06.0)))

(rule
 (alias env-env-dune-file)
 (deps (package dune) (source_tree test-cases/env/env-dune-file))
 (action
  (chdir
   test-cases/env/env-dune-file
   (progn (run dune-cram run run.t) (diff? run.t run.t.corrected)))))

(rule
 (alias env-env-simple)
 (deps (package dune) (source_tree test-cases/env/env-simple))
 (action
  (chdir
   test-cases/env/env-simple
   (progn (run dune-cram run run.t) (diff? run.t run.t.corrected))))
 (enabled_if (>= %{ocaml_version} 4.06.0)))

(rule
 (alias env-env-tracking)
 (deps (package dune) (source_tree test-cases/env/env-tracking))
 (action
  (chdir
   test-cases/env/env-tracking
   (progn (run dune-cram run run.t) (diff? run.t run.t.corrected)))))

(rule
 (alias env-env-var-expansion)
 (deps (package dune) (source_tree test-cases/env/env-var-expansion))
 (action
  (chdir
   test-cases/env/env-var-expansion
   (progn (run dune-cram run run.t) (diff? run.t run.t.corrected)))))

(rule
 (alias env-env-variables)
 (deps (package dune) (source_tree test-cases/env/env-variables))
 (action
  (chdir
   test-cases/env/env-variables
   (progn (run dune-cram run run.t) (diff? run.t run.t.corrected)))))

(rule
 (alias env-envs-and-contexts)
 (deps (package dune) (source_tree test-cases/env/envs-and-contexts))
 (action
  (chdir
   test-cases/env/envs-and-contexts
   (progn (run dune-cram run run.t) (diff? run.t run.t.corrected)))))

(rule
 (alias exclude-missing-module)
 (deps (package dune) (source_tree test-cases/exclude-missing-module))
 (action
  (chdir
   test-cases/exclude-missing-module
   (progn (run dune-cram run run.t) (diff? run.t run.t.corrected)))))

(rule
 (alias exe-name-mangle)
 (deps (package dune) (source_tree test-cases/exe-name-mangle))
 (action
  (chdir
   test-cases/exe-name-mangle
   (progn (run dune-cram run run.t) (diff? run.t run.t.corrected)))))

(rule
 (alias exec-cmd)
 (deps (package dune) (source_tree test-cases/exec-cmd))
 (action
  (chdir
   test-cases/exec-cmd
   (progn (run dune-cram run run.t) (diff? run.t run.t.corrected)))))

(rule
 (alias exec-missing)
 (deps (package dune) (source_tree test-cases/exec-missing))
 (action
  (chdir
   test-cases/exec-missing
   (progn (run dune-cram run run.t) (diff? run.t run.t.corrected)))))

(rule
 (alias exes-with-c)
 (deps (package dune) (source_tree test-cases/exes-with-c))
 (action
  (chdir
   test-cases/exes-with-c
   (progn (run dune-cram run run.t) (diff? run.t run.t.corrected)))))

(rule
 (alias explicit_js_mode)
 (deps (package dune) (source_tree test-cases/explicit_js_mode))
 (action
  (chdir
   test-cases/explicit_js_mode
   (progn (run dune-cram run run.t) (diff? run.t run.t.corrected)))))

(rule
 (alias extensions-versionning)
 (deps (package dune) (source_tree test-cases/extensions-versionning))
 (action
  (chdir
   test-cases/extensions-versionning
   (progn (run dune-cram run run.t) (diff? run.t run.t.corrected)))))

(rule
 (alias external-lib-deps-github3143)
 (deps (package dune) (source_tree test-cases/external-lib-deps/github3143))
 (action
  (chdir
   test-cases/external-lib-deps/github3143
   (progn (run dune-cram run run.t) (diff? run.t run.t.corrected)))))

(rule
 (alias external-lib-deps-simple)
 (deps (package dune) (source_tree test-cases/external-lib-deps/simple))
 (action
  (chdir
   test-cases/external-lib-deps/simple
   (progn (run dune-cram run run.t) (diff? run.t run.t.corrected)))))

(rule
 (alias extra-lang-line)
 (deps (package dune) (source_tree test-cases/extra-lang-line))
 (action
  (chdir
   test-cases/extra-lang-line
   (progn (run dune-cram run run.t) (diff? run.t run.t.corrected)))))

(rule
 (alias fallback-dune)
 (deps (package dune) (source_tree test-cases/fallback-dune))
 (action
  (chdir
   test-cases/fallback-dune
   (progn (run dune-cram run run.t) (diff? run.t run.t.corrected)))))

(rule
 (alias fdo)
 (deps (package dune) (source_tree test-cases/fdo))
 (action
  (chdir
   test-cases/fdo
   (progn (run dune-cram run run.t) (diff? run.t run.t.corrected))))
 (enabled_if (>= %{ocaml_version} 4.11.0)))

(rule
 (alias findlib)
 (deps (package dune) (source_tree test-cases/findlib))
 (action
  (chdir
   test-cases/findlib
   (progn (run dune-cram run run.t) (diff? run.t run.t.corrected)))))

(rule
 (alias findlib-dynload)
 (deps (package dune) (source_tree test-cases/findlib-dynload))
 (action
  (chdir
   test-cases/findlib-dynload
   (progn (run dune-cram run run.t) (diff? run.t run.t.corrected)))))

(rule
 (alias findlib-error)
 (deps (package dune) (source_tree test-cases/findlib-error))
 (action
  (chdir
   test-cases/findlib-error
   (progn (run dune-cram run run.t) (diff? run.t run.t.corrected)))))

(rule
 (alias forbidden_libraries)
 (deps (package dune) (source_tree test-cases/forbidden_libraries))
 (action
  (chdir
   test-cases/forbidden_libraries
   (progn (run dune-cram run run.t) (diff? run.t run.t.corrected)))))

(rule
 (alias force-test)
 (deps (package dune) (source_tree test-cases/force-test))
 (action
  (chdir
   test-cases/force-test
   (progn (run dune-cram run run.t) (diff? run.t run.t.corrected)))))

(rule
 (alias foreign-library)
 (deps (package dune) (source_tree test-cases/foreign-library))
 (action
  (chdir
   test-cases/foreign-library
   (progn (run dune-cram run run.t) (diff? run.t run.t.corrected)))))

(rule
 (alias foreign-stubs)
 (deps (package dune) (source_tree test-cases/foreign-stubs))
 (action
  (chdir
   test-cases/foreign-stubs
   (progn (run dune-cram run run.t) (diff? run.t run.t.corrected)))))

(rule
 (alias format-dune-file)
 (deps (package dune) (source_tree test-cases/format-dune-file))
 (action
  (chdir
   test-cases/format-dune-file
   (progn (run dune-cram run run.t) (diff? run.t run.t.corrected)))))

(rule
 (alias formatting)
 (deps (package dune) (source_tree test-cases/formatting))
 (action
  (chdir
   test-cases/formatting
   (progn (run dune-cram run run.t) (diff? run.t run.t.corrected)))))

(rule
 (alias gen-opam-install-file)
 (deps (package dune) (source_tree test-cases/gen-opam-install-file))
 (action
  (chdir
   test-cases/gen-opam-install-file
   (progn (run dune-cram run run.t) (diff? run.t run.t.corrected)))))

(rule
 (alias github1019)
 (deps (package dune) (source_tree test-cases/github1019))
 (action
  (chdir
   test-cases/github1019
   (progn (run dune-cram run run.t) (diff? run.t run.t.corrected)))))

(rule
 (alias github1099)
 (deps (package dune) (source_tree test-cases/github1099))
 (action
  (chdir
   test-cases/github1099
   (progn (run dune-cram run run.t) (diff? run.t run.t.corrected)))))

(rule
 (alias github1231)
 (deps (package dune) (source_tree test-cases/github1231))
 (action
  (chdir
   test-cases/github1231
   (progn (run dune-cram run run.t) (diff? run.t run.t.corrected)))))

(rule
 (alias github1342)
 (deps (package dune) (source_tree test-cases/github1342))
 (action
  (chdir
   test-cases/github1342
   (progn (run dune-cram run run.t) (diff? run.t run.t.corrected)))))

(rule
 (alias github1372)
 (deps (package dune) (source_tree test-cases/github1372))
 (action
  (chdir
   test-cases/github1372
   (progn (run dune-cram run run.t) (diff? run.t run.t.corrected)))))

(rule
 (alias github1395)
 (deps (package dune) (source_tree test-cases/github1395))
 (action
  (chdir
   test-cases/github1395
   (progn (run dune-cram run run.t) (diff? run.t run.t.corrected)))))

(rule
 (alias github1426)
 (deps (package dune) (source_tree test-cases/github1426))
 (action
  (chdir
   test-cases/github1426
   (progn (run dune-cram run run.t) (diff? run.t run.t.corrected)))))

(rule
 (alias github1529)
 (deps (package dune) (source_tree test-cases/github1529))
 (action
  (chdir
   test-cases/github1529
   (progn (run dune-cram run run.t) (diff? run.t run.t.corrected)))))

(rule
 (alias github1541)
 (deps (package dune) (source_tree test-cases/github1541))
 (action
  (chdir
   test-cases/github1541
   (progn (run dune-cram run run.t) (diff? run.t run.t.corrected)))))

(rule
 (alias github1549)
 (deps (package dune) (source_tree test-cases/github1549))
 (action
  (chdir
   test-cases/github1549
   (progn (run dune-cram run run.t) (diff? run.t run.t.corrected)))))

(rule
 (alias github1560)
 (deps (package dune) (source_tree test-cases/github1560))
 (action
  (chdir
   test-cases/github1560
   (progn (run dune-cram run run.t) (diff? run.t run.t.corrected)))))

(rule
 (alias github1616)
 (deps (package dune) (source_tree test-cases/github1616))
 (action
  (chdir
   test-cases/github1616
   (progn (run dune-cram run run.t) (diff? run.t run.t.corrected)))))

(rule
 (alias github1811)
 (deps (package dune) (source_tree test-cases/github1811))
 (action
  (chdir
   test-cases/github1811
   (progn (run dune-cram run run.t) (diff? run.t run.t.corrected)))))

(rule
 (alias github1855)
 (deps (package dune) (source_tree test-cases/github1855))
 (action
  (chdir
   test-cases/github1855
   (progn (run dune-cram run run.t) (diff? run.t run.t.corrected)))))

(rule
 (alias github1856)
 (deps (package dune) (source_tree test-cases/github1856))
 (action
  (chdir
   test-cases/github1856
   (progn (run dune-cram run run.t) (diff? run.t run.t.corrected)))))

(rule
 (alias github1946)
 (deps (package dune) (source_tree test-cases/github1946))
 (action
  (chdir
   test-cases/github1946
   (progn (run dune-cram run run.t) (diff? run.t run.t.corrected)))))

(rule
 (alias github20)
 (deps (package dune) (source_tree test-cases/github20))
 (action
  (chdir
   test-cases/github20
   (progn (run dune-cram run run.t) (diff? run.t run.t.corrected)))))

(rule
 (alias github2033)
 (deps (package dune) (source_tree test-cases/github2033))
 (action
  (chdir
   test-cases/github2033
   (progn (run dune-cram run run.t) (diff? run.t run.t.corrected)))))

(rule
 (alias github2061)
 (deps (package dune) (source_tree test-cases/github2061))
 (action
  (chdir
   test-cases/github2061
   (progn (run dune-cram run run.t) (diff? run.t run.t.corrected)))))

(rule
 (alias github2123)
 (deps (package dune) (source_tree test-cases/github2123))
 (action
  (chdir
   test-cases/github2123
   (progn (run dune-cram run run.t) (diff? run.t run.t.corrected)))))

(rule
 (alias github2206)
 (deps (package dune) (source_tree test-cases/github2206))
 (action
  (chdir
   test-cases/github2206
   (progn (run dune-cram run run.t) (diff? run.t run.t.corrected)))))

(rule
 (alias github2228)
 (deps (package dune) (source_tree test-cases/github2228))
 (action
  (chdir
   test-cases/github2228
   (progn (run dune-cram run run.t) (diff? run.t run.t.corrected)))))

(rule
 (alias github2272)
 (deps (package dune) (source_tree test-cases/github2272))
 (action
  (chdir
   test-cases/github2272
   (progn (run dune-cram run run.t) (diff? run.t run.t.corrected)))))

(rule
 (alias github24)
 (deps (package dune) (source_tree test-cases/github24))
 (action
  (chdir
   test-cases/github24
   (progn (run dune-cram run run.t) (diff? run.t run.t.corrected)))))

(rule
 (alias github2499)
 (deps (package dune) (source_tree test-cases/github2499))
 (action
  (chdir
   test-cases/github2499
   (progn (run dune-cram run run.t) (diff? run.t run.t.corrected)))))

(rule
 (alias github25)
 (deps (package dune) (source_tree test-cases/github25))
 (action
  (setenv
   OCAMLPATH
   ./findlib-packages
   (chdir
    test-cases/github25
    (progn (run dune-cram run run.t) (diff? run.t run.t.corrected))))))

(rule
 (alias github2584)
 (deps (package dune) (source_tree test-cases/github2584))
 (action
  (chdir
   test-cases/github2584
   (progn (run dune-cram run run.t) (diff? run.t run.t.corrected)))))

(rule
 (alias github2629)
 (deps (package dune) (source_tree test-cases/github2629))
 (action
  (chdir
   test-cases/github2629
   (progn (run dune-cram run run.t) (diff? run.t run.t.corrected)))))

(rule
 (alias github2681)
 (deps (package dune) (source_tree test-cases/github2681))
 (action
  (chdir
   test-cases/github2681
   (progn (run dune-cram run run.t) (diff? run.t run.t.corrected)))))

(rule
 (alias github2848)
 (deps (package dune) (source_tree test-cases/github2848))
 (action
  (chdir
   test-cases/github2848
   (progn (run dune-cram run run.t) (diff? run.t run.t.corrected)))))

(rule
 (alias github3046)
 (deps (package dune) (source_tree test-cases/github3046))
 (action
  (chdir
   test-cases/github3046
   (progn (run dune-cram run run.t) (diff? run.t run.t.corrected)))))

(rule
 (alias github3180)
 (deps (package dune) (source_tree test-cases/github3180))
 (action
  (chdir
   test-cases/github3180
   (progn (run dune-cram run run.t) (diff? run.t run.t.corrected)))))

(rule
 (alias github3306)
 (deps (package dune) (source_tree test-cases/github3306))
 (action
  (chdir
   test-cases/github3306
   (progn (run dune-cram run run.t) (diff? run.t run.t.corrected)))))

(rule
 (alias github534)
 (deps (package dune) (source_tree test-cases/github534))
 (action
  (chdir
   test-cases/github534
   (progn (run dune-cram run run.t) (diff? run.t run.t.corrected)))))

(rule
 (alias github568)
 (deps (package dune) (source_tree test-cases/github568))
 (action
  (chdir
   test-cases/github568
   (progn (run dune-cram run run.t) (diff? run.t run.t.corrected)))))

(rule
 (alias github597)
 (deps (package dune) (source_tree test-cases/github597))
 (action
  (chdir
   test-cases/github597
   (progn (run dune-cram run run.t) (diff? run.t run.t.corrected)))))

(rule
 (alias github644)
 (deps (package dune) (source_tree test-cases/github644))
 (action
  (chdir
   test-cases/github644
   (progn (run dune-cram run run.t) (diff? run.t run.t.corrected)))))

(rule
 (alias github660)
 (deps (package dune) (source_tree test-cases/github660))
 (action
  (chdir
   test-cases/github660
   (progn (run dune-cram run run.t) (diff? run.t run.t.corrected)))))

(rule
 (alias github734)
 (deps (package dune) (source_tree test-cases/github734))
 (action
  (chdir
   test-cases/github734
   (progn (run dune-cram run run.t) (diff? run.t run.t.corrected)))))

(rule
 (alias github759)
 (deps (package dune) (source_tree test-cases/github759))
 (action
  (chdir
   test-cases/github759
   (progn (run dune-cram run run.t) (diff? run.t run.t.corrected)))))

(rule
 (alias github761)
 (deps (package dune) (source_tree test-cases/github761))
 (action
  (chdir
   test-cases/github761
   (progn (run dune-cram run run.t) (diff? run.t run.t.corrected)))))

(rule
 (alias github764)
 (deps (package dune) (source_tree test-cases/github764))
 (action
  (chdir
   test-cases/github764
   (progn (run dune-cram run run.t) (diff? run.t run.t.corrected))))
 (enabled_if (<> %{ocaml-config:system} win)))

(rule
 (alias github784)
 (deps (package dune) (source_tree test-cases/github784))
 (action
  (chdir
   test-cases/github784
   (progn (run dune-cram run run.t) (diff? run.t run.t.corrected)))))

(rule
 (alias github992)
 (deps (package dune) (source_tree test-cases/github992))
 (action
  (chdir
   test-cases/github992
   (progn (run dune-cram run run.t) (diff? run.t run.t.corrected)))))

(rule
 (alias glob-deps)
 (deps (package dune) (source_tree test-cases/glob-deps))
 (action
  (chdir
   test-cases/glob-deps
   (progn (run dune-cram run run.t) (diff? run.t run.t.corrected)))))

(rule
 (alias glob-deps-change)
 (deps (package dune) (source_tree test-cases/glob-deps-change))
 (action
  (chdir
   test-cases/glob-deps-change
   (progn (run dune-cram run run.t) (diff? run.t run.t.corrected)))))

(rule
 (alias hints-root-executables)
 (deps (package dune) (source_tree test-cases/hints-root-executables))
 (action
  (chdir
   test-cases/hints-root-executables
   (progn (run dune-cram run run.t) (diff? run.t run.t.corrected)))))

(rule
 (alias ignored_subdirs)
 (deps (package dune) (source_tree test-cases/ignored_subdirs))
 (action
  (chdir
   test-cases/ignored_subdirs
   (progn (run dune-cram run run.t) (diff? run.t run.t.corrected)))))

(rule
 (alias include-loop)
 (deps (package dune) (source_tree test-cases/include-loop))
 (action
  (chdir
   test-cases/include-loop
   (progn (run dune-cram run run.t) (diff? run.t run.t.corrected)))))

(rule
 (alias include-qualified)
 (deps (package dune) (source_tree test-cases/include-qualified))
 (action
  (chdir
   test-cases/include-qualified
   (progn (run dune-cram run run.t) (diff? run.t run.t.corrected)))))

(rule
 (alias incremental-rebuilds)
 (deps (package dune) (source_tree test-cases/incremental-rebuilds))
 (action
  (chdir
   test-cases/incremental-rebuilds
   (progn (run dune-cram run run.t) (diff? run.t run.t.corrected)))))

(rule
 (alias inline_tests)
 (deps (package dune) (source_tree test-cases/inline_tests))
 (action
  (chdir
   test-cases/inline_tests
   (progn (run dune-cram run run.t) (diff? run.t run.t.corrected)))))

(rule
 (alias inline_tests-multi-mode)
 (deps (package dune) (source_tree test-cases/inline_tests-multi-mode))
 (action
  (chdir
   test-cases/inline_tests-multi-mode
   (progn (run dune-cram run run.t) (diff? run.t run.t.corrected)))))

(rule
 (alias install-dry-run)
 (deps (package dune) (source_tree test-cases/install-dry-run))
 (action
  (chdir
   test-cases/install-dry-run
   (progn (run dune-cram run run.t) (diff? run.t run.t.corrected)))))

(rule
 (alias install-libdir)
 (deps (package dune) (source_tree test-cases/install-libdir))
 (action
  (chdir
   test-cases/install-libdir
   (progn (run dune-cram run run.t) (diff? run.t run.t.corrected)))))

(rule
 (alias install-mandir)
 (deps (package dune) (source_tree test-cases/install-mandir))
 (action
  (chdir
   test-cases/install-mandir
   (progn (run dune-cram run run.t) (diff? run.t run.t.corrected)))))

(rule
 (alias install-multiple-contexts)
 (deps (package dune) (source_tree test-cases/install-multiple-contexts))
 (action
  (chdir
   test-cases/install-multiple-contexts
   (progn (run dune-cram run run.t) (diff? run.t run.t.corrected)))))

(rule
 (alias install-partial-package)
 (deps (package dune) (source_tree test-cases/install-partial-package))
 (action
  (chdir
   test-cases/install-partial-package
   (progn (run dune-cram run run.t) (diff? run.t run.t.corrected)))))

(rule
 (alias install-rule-order)
 (deps (package dune) (source_tree test-cases/install-rule-order))
 (action
  (chdir
   test-cases/install-rule-order
   (progn (run dune-cram run run.t) (diff? run.t run.t.corrected)))))

(rule
 (alias install-single-section)
 (deps (package dune) (source_tree test-cases/install-single-section))
 (action
  (chdir
   test-cases/install-single-section
   (progn (run dune-cram run run.t) (diff? run.t run.t.corrected)))))

(rule
 (alias install-with-var)
 (deps (package dune) (source_tree test-cases/install-with-var))
 (action
  (chdir
   test-cases/install-with-var
   (progn (run dune-cram run run.t) (diff? run.t run.t.corrected)))))

(rule
 (alias installable-dup-private-libs)
 (deps (package dune) (source_tree test-cases/installable-dup-private-libs))
 (action
  (chdir
   test-cases/installable-dup-private-libs
   (progn (run dune-cram run run.t) (diff? run.t run.t.corrected)))))

(rule
 (alias intf-only)
 (deps (package dune) (source_tree test-cases/intf-only))
 (action
  (chdir
   test-cases/intf-only
   (progn (run dune-cram run run.t) (diff? run.t run.t.corrected)))))

(rule
 (alias invalid-dune-package)
 (deps (package dune) (source_tree test-cases/invalid-dune-package))
 (action
  (chdir
   test-cases/invalid-dune-package
   (progn (run dune-cram run run.t) (diff? run.t run.t.corrected)))))

(rule
 (alias invalid-module-name)
 (deps (package dune) (source_tree test-cases/invalid-module-name))
 (action
  (chdir
   test-cases/invalid-module-name
   (progn (run dune-cram run run.t) (diff? run.t run.t.corrected)))))

(rule
 (alias js_of_ocaml)
 (deps (package dune) (source_tree test-cases/js_of_ocaml))
 (action
  (setenv
   NODE
   %{bin:node}
   (chdir
    test-cases/js_of_ocaml
    (progn (run dune-cram run run.t) (diff? run.t run.t.corrected))))))

(rule
 (alias lib)
 (deps (package dune) (source_tree test-cases/lib))
 (action
  (chdir
   test-cases/lib
   (progn (run dune-cram run run.t) (diff? run.t run.t.corrected)))))

(rule
 (alias lib-available)
 (deps (package dune) (source_tree test-cases/lib-available))
 (action
  (chdir
   test-cases/lib-available
   (progn (run dune-cram run run.t) (diff? run.t run.t.corrected)))))

(rule
 (alias lib-errors)
 (deps (package dune) (source_tree test-cases/lib-errors))
 (action
  (chdir
   test-cases/lib-errors
   (progn (run dune-cram run run.t) (diff? run.t run.t.corrected)))))

(rule
 (alias link-deps)
 (deps (package dune) (source_tree test-cases/link-deps))
 (action
  (chdir
   test-cases/link-deps
   (progn (run dune-cram run run.t) (diff? run.t run.t.corrected)))))

(rule
 (alias link-includes)
 (deps (package dune) (source_tree test-cases/link-includes))
 (action
  (chdir
   test-cases/link-includes
   (progn (run dune-cram run run.t) (diff? run.t run.t.corrected)))))

(rule
 (alias lint)
 (deps (package dune) (source_tree test-cases/lint))
 (action
  (chdir
   test-cases/lint
   (progn (run dune-cram run run.t) (diff? run.t run.t.corrected)))))

(rule
 (alias loop)
 (deps (package dune) (source_tree test-cases/loop))
 (action
  (chdir
   test-cases/loop
   (progn (run dune-cram run run.t) (diff? run.t run.t.corrected)))))

(rule
 (alias macro-expand-error)
 (deps (package dune) (source_tree test-cases/macro-expand-error))
 (action
  (chdir
   test-cases/macro-expand-error
   (progn (run dune-cram run run.t) (diff? run.t run.t.corrected)))))

(rule
 (alias mdx-stanza)
 (deps (package dune) (source_tree test-cases/mdx-stanza))
 (action
  (chdir
   test-cases/mdx-stanza
   (progn (run dune-cram run run.t) (diff? run.t run.t.corrected)))))

(rule
 (alias menhir-cmly)
 (deps (package dune) (source_tree test-cases/menhir/cmly))
 (action
  (chdir
   test-cases/menhir/cmly
   (progn (run dune-cram run run.t) (diff? run.t run.t.corrected)))))

(rule
 (alias menhir-env)
 (deps (package dune) (source_tree test-cases/menhir/env))
 (action
  (chdir
   test-cases/menhir/env
   (progn (run dune-cram run run.t) (diff? run.t run.t.corrected)))))

(rule
 (alias menhir-general)
 (deps (package dune) (source_tree test-cases/menhir/general))
 (action
  (chdir
   test-cases/menhir/general
   (progn (run dune-cram run run.t) (diff? run.t run.t.corrected)))))

(rule
 (alias menhir-general-2.0)
 (deps (package dune) (source_tree test-cases/menhir/general-2.0))
 (action
  (chdir
   test-cases/menhir/general-2.0
   (progn (run dune-cram run run.t) (diff? run.t run.t.corrected)))))

(rule
 (alias menhir-promote)
 (deps (package dune) (source_tree test-cases/menhir/promote))
 (action
  (chdir
   test-cases/menhir/promote
   (progn (run dune-cram run run.t) (diff? run.t run.t.corrected)))))

(rule
 (alias merlin-allow_approximate_merlin)
 (deps
  (package dune)
  (source_tree test-cases/merlin/allow_approximate_merlin))
 (action
  (chdir
   test-cases/merlin/allow_approximate_merlin
   (progn (run dune-cram run run.t) (diff? run.t run.t.corrected)))))

(rule
 (alias merlin-merlin-tests)
 (deps (package dune) (source_tree test-cases/merlin/merlin-tests))
 (action
  (chdir
   test-cases/merlin/merlin-tests
   (progn (run dune-cram run run.t) (diff? run.t run.t.corrected)))))

(rule
 (alias merlin-src-dirs-of-deps)
 (deps (package dune) (source_tree test-cases/merlin/src-dirs-of-deps))
 (action
  (chdir
   test-cases/merlin/src-dirs-of-deps
   (progn (run dune-cram run run.t) (diff? run.t run.t.corrected)))))

(rule
 (alias meta-gen)
 (deps (package dune) (source_tree test-cases/meta-gen))
 (action
  (chdir
   test-cases/meta-gen
   (progn (run dune-cram run run.t) (diff? run.t run.t.corrected)))))

(rule
 (alias misc)
 (deps (package dune) (source_tree test-cases/misc))
 (action
  (chdir
   test-cases/misc
   (progn (run dune-cram run run.t) (diff? run.t run.t.corrected)))))

(rule
 (alias missing-loc-run)
 (deps (package dune) (source_tree test-cases/missing-loc-run))
 (action
  (chdir
   test-cases/missing-loc-run
   (progn (run dune-cram run run.t) (diff? run.t run.t.corrected)))))

(rule
 (alias multi-dir)
 (deps (package dune) (source_tree test-cases/multi-dir))
 (action
  (chdir
   test-cases/multi-dir
   (progn (run dune-cram run run.t) (diff? run.t run.t.corrected)))))

(rule
 (alias multiple-targets)
 (deps (package dune) (source_tree test-cases/multiple-targets))
 (action
  (chdir
   test-cases/multiple-targets
   (progn (run dune-cram run run.t) (diff? run.t run.t.corrected)))))

(rule
 (alias name-field-validation)
 (deps (package dune) (source_tree test-cases/name-field-validation))
 (action
  (chdir
   test-cases/name-field-validation
   (progn (run dune-cram run run.t) (diff? run.t run.t.corrected)))))

(rule
 (alias no-installable-mode)
 (deps (package dune) (source_tree test-cases/no-installable-mode))
 (action
  (chdir
   test-cases/no-installable-mode
   (progn (run dune-cram run run.t) (diff? run.t run.t.corrected)))))

(rule
 (alias no-name-field)
 (deps (package dune) (source_tree test-cases/no-name-field))
 (action
  (chdir
   test-cases/no-name-field
   (progn (run dune-cram run run.t) (diff? run.t run.t.corrected)))))

(rule
 (alias null-dep)
 (deps (package dune) (source_tree test-cases/null-dep))
 (action
  (chdir
   test-cases/null-dep
   (progn (run dune-cram run run.t) (diff? run.t run.t.corrected)))))

(rule
 (alias ocaml-config-macro)
 (deps (package dune) (source_tree test-cases/ocaml-config-macro))
 (action
  (chdir
   test-cases/ocaml-config-macro
   (progn (run dune-cram run run.t) (diff? run.t run.t.corrected)))))

(rule
 (alias ocaml-syntax)
 (deps (package dune) (source_tree test-cases/ocaml-syntax))
 (action
  (chdir
   test-cases/ocaml-syntax
   (progn (run dune-cram run run.t) (diff? run.t run.t.corrected)))))

(rule
 (alias ocamldep-multi-stanzas)
 (deps (package dune) (source_tree test-cases/ocamldep-multi-stanzas))
 (action
  (chdir
   test-cases/ocamldep-multi-stanzas
   (progn (run dune-cram run run.t) (diff? run.t run.t.corrected)))))

(rule
 (alias odig-files-workspace)
 (deps (package dune) (source_tree test-cases/odig-files-workspace))
 (action
  (chdir
   test-cases/odig-files-workspace
   (progn (run dune-cram run run.t) (diff? run.t run.t.corrected)))))

(rule
 (alias odoc-github717-odoc-index)
 (deps (package dune) (source_tree test-cases/odoc/github717-odoc-index))
 (action
  (chdir
   test-cases/odoc/github717-odoc-index
   (progn (run dune-cram run run.t) (diff? run.t run.t.corrected))))
 (enabled_if (<> %{ocaml_version} 4.02.3)))

(rule
 (alias odoc-multiple-private-libs)
 (deps (package dune) (source_tree test-cases/odoc/multiple-private-libs))
 (action
  (chdir
   test-cases/odoc/multiple-private-libs
   (progn (run dune-cram run run.t) (diff? run.t run.t.corrected))))
 (enabled_if (<> %{ocaml_version} 4.02.3)))

(rule
 (alias odoc-odoc-package-mld-link)
 (deps (package dune) (source_tree test-cases/odoc/odoc-package-mld-link))
 (action
  (chdir
   test-cases/odoc/odoc-package-mld-link
   (progn (run dune-cram run run.t) (diff? run.t run.t.corrected))))
 (enabled_if (<> %{ocaml_version} 4.02.3)))

(rule
 (alias odoc-odoc-simple)
 (deps (package dune) (source_tree test-cases/odoc/odoc-simple))
 (action
  (chdir
   test-cases/odoc/odoc-simple
   (progn (run dune-cram run run.t) (diff? run.t run.t.corrected))))
 (enabled_if (<> %{ocaml_version} 4.02.3)))

(rule
 (alias odoc-odoc-unique-mlds)
 (deps (package dune) (source_tree test-cases/odoc/odoc-unique-mlds))
 (action
  (chdir
   test-cases/odoc/odoc-unique-mlds
   (progn (run dune-cram run run.t) (diff? run.t run.t.corrected))))
 (enabled_if (<> %{ocaml_version} 4.02.3)))

(rule
 (alias odoc-warnings)
 (deps (package dune) (source_tree test-cases/odoc/warnings))
 (action
  (chdir
   test-cases/odoc/warnings
   (progn (run dune-cram run run.t) (diff? run.t run.t.corrected))))
 (enabled_if (<> %{ocaml_version} 4.02.3)))

(rule
 (alias old-dune-subsystem)
 (deps (package dune) (source_tree test-cases/old-dune-subsystem))
 (action
  (chdir
   test-cases/old-dune-subsystem
   (progn (run dune-cram run run.t) (diff? run.t run.t.corrected)))))

(rule
 (alias optional)
 (deps (package dune) (source_tree test-cases/optional))
 (action
  (chdir
   test-cases/optional
   (progn (run dune-cram run run.t) (diff? run.t run.t.corrected)))))

(rule
 (alias optional-executable)
 (deps (package dune) (source_tree test-cases/optional-executable))
 (action
  (chdir
   test-cases/optional-executable
   (progn (run dune-cram run run.t) (diff? run.t run.t.corrected)))))

(rule
 (alias output-obj)
 (deps (package dune) (source_tree test-cases/output-obj))
 (action
  (chdir
   test-cases/output-obj
   (progn (run dune-cram run run.t) (diff? run.t run.t.corrected))))
 (enabled_if
  (and
   (and (<> %{ocaml-config:system} macosx) (<> %{ocaml-config:system} win))
   (>= %{ocaml_version} 4.06.0))))

(rule
 (alias package-dep)
 (deps (package dune) (source_tree test-cases/package-dep))
 (action
  (chdir
   test-cases/package-dep
   (progn (run dune-cram run run.t) (diff? run.t run.t.corrected)))))

(rule
 (alias path-variables)
 (deps (package dune) (source_tree test-cases/path-variables))
 (action
  (chdir
   test-cases/path-variables
   (progn (run dune-cram run run.t) (diff? run.t run.t.corrected)))))

(rule
 (alias pkg-config-quoting)
 (deps
  (package dune)
  (source_tree test-cases/pkg-config-quoting)
  (package dune-configurator))
 (action
  (chdir
   test-cases/pkg-config-quoting
   (progn (run dune-cram run run.t) (diff? run.t run.t.corrected)))))

(rule
 (alias plugin-mode)
 (deps (package dune) (source_tree test-cases/plugin-mode))
 (action
  (chdir
   test-cases/plugin-mode
   (progn (run dune-cram run run.t) (diff? run.t run.t.corrected)))))

(rule
 (alias ppx-rewriter)
 (deps (package dune) (source_tree test-cases/ppx-rewriter))
 (action
  (chdir
   test-cases/ppx-rewriter
   (progn (run dune-cram run run.t) (diff? run.t run.t.corrected))))
 (enabled_if (<> %{ocaml_version} 4.02.3)))

(rule
 (alias ppx-runtime-dependencies)
 (deps (package dune) (source_tree test-cases/ppx-runtime-dependencies))
 (action
  (chdir
   test-cases/ppx-runtime-dependencies
   (progn (run dune-cram run run.t) (diff? run.t run.t.corrected)))))

(rule
 (alias preprocess-with-action)
 (deps (package dune) (source_tree test-cases/preprocess-with-action))
 (action
  (chdir
   test-cases/preprocess-with-action
   (progn (run dune-cram run run.t) (diff? run.t run.t.corrected)))))

(rule
 (alias private-modules)
 (deps (package dune) (source_tree test-cases/private-modules))
 (action
  (chdir
   test-cases/private-modules
   (progn (run dune-cram run run.t) (diff? run.t run.t.corrected)))))

(rule
 (alias private-public-overlap)
 (deps (package dune) (source_tree test-cases/private-public-overlap))
 (action
  (chdir
   test-cases/private-public-overlap
   (progn (run dune-cram run run.t) (diff? run.t run.t.corrected)))))

(rule
 (alias project-root)
 (deps (package dune) (source_tree test-cases/project-root))
 (action
  (chdir
   test-cases/project-root
   (progn (run dune-cram run run.t) (diff? run.t run.t.corrected)))))

(rule
 (alias promote)
 (deps (package dune) (source_tree test-cases/promote))
 (action
  (chdir
   test-cases/promote
   (progn (run dune-cram run run.t) (diff? run.t run.t.corrected)))))

(rule
 (alias quoting)
 (deps (package dune) (source_tree test-cases/quoting))
 (action
  (chdir
   test-cases/quoting
   (progn (run dune-cram run run.t) (diff? run.t run.t.corrected)))))

(rule
 (alias re-exported-deps)
 (deps (package dune) (source_tree test-cases/re-exported-deps))
 (action
  (chdir
   test-cases/re-exported-deps
   (progn (run dune-cram run run.t) (diff? run.t run.t.corrected)))))

(rule
 (alias reason)
 (deps (package dune) (source_tree test-cases/reason))
 (action
  (chdir
   test-cases/reason
   (progn (run dune-cram run run.t) (diff? run.t run.t.corrected)))))

(rule
 (alias redirections)
 (deps (package dune) (source_tree test-cases/redirections))
 (action
  (chdir
   test-cases/redirections
   (progn (run dune-cram run run.t) (diff? run.t run.t.corrected)))))

(rule
 (alias reporting-of-cycles)
 (deps (package dune) (source_tree test-cases/reporting-of-cycles))
 (action
  (chdir
   test-cases/reporting-of-cycles
   (progn (run dune-cram run run.t) (diff? run.t run.t.corrected)))))

(rule
 (alias rule-target-external)
 (deps (package dune) (source_tree test-cases/rule-target-external))
 (action
  (chdir
   test-cases/rule-target-external
   (progn (run dune-cram run run.t) (diff? run.t run.t.corrected)))))

(rule
 (alias rule-target-inferrence)
 (deps (package dune) (source_tree test-cases/rule-target-inferrence))
 (action
  (chdir
   test-cases/rule-target-inferrence
   (progn (run dune-cram run run.t) (diff? run.t run.t.corrected)))))

(rule
 (alias sandboxing)
 (deps (package dune) (source_tree test-cases/sandboxing))
 (action
  (chdir
   test-cases/sandboxing
   (progn (run dune-cram run run.t) (diff? run.t run.t.corrected)))))

(rule
 (alias scope-bug)
 (deps (package dune) (source_tree test-cases/scope-bug))
 (action
  (chdir
   test-cases/scope-bug
   (progn (run dune-cram run run.t) (diff? run.t run.t.corrected)))))

(rule
 (alias scope-ppx-bug)
 (deps (package dune) (source_tree test-cases/scope-ppx-bug))
 (action
  (chdir
   test-cases/scope-ppx-bug
   (progn (run dune-cram run run.t) (diff? run.t run.t.corrected)))))

(rule
 (alias select)
 (deps (package dune) (source_tree test-cases/select))
 (action
  (chdir
   test-cases/select
   (progn (run dune-cram run run.t) (diff? run.t run.t.corrected)))))

(rule
 (alias select-2-0-rules)
 (deps (package dune) (source_tree test-cases/select-2-0-rules))
 (action
  (chdir
   test-cases/select-2-0-rules
   (progn (run dune-cram run run.t) (diff? run.t run.t.corrected)))))

(rule
 (alias several-packages)
 (deps (package dune) (source_tree test-cases/several-packages))
 (action
  (chdir
   test-cases/several-packages
   (progn (run dune-cram run run.t) (diff? run.t run.t.corrected)))))

(rule
 (alias shadow-bindings)
 (deps (package dune) (source_tree test-cases/shadow-bindings))
 (action
  (chdir
   test-cases/shadow-bindings
   (progn (run dune-cram run run.t) (diff? run.t run.t.corrected)))))

(rule
 (alias stale-artifact-removal)
 (deps (package dune) (source_tree test-cases/stale-artifact-removal))
 (action
  (chdir
   test-cases/stale-artifact-removal
   (progn (run dune-cram run run.t) (diff? run.t run.t.corrected)))))

(rule
 (alias stdlib-compilation)
 (deps (package dune) (source_tree test-cases/stdlib-compilation))
 (action
  (chdir
   test-cases/stdlib-compilation
   (progn (run dune-cram run run.t) (diff? run.t run.t.corrected)))))

(rule
 (alias strict-package-deps)
 (deps (package dune) (source_tree test-cases/strict-package-deps))
 (action
  (chdir
   test-cases/strict-package-deps
   (progn (run dune-cram run run.t) (diff? run.t run.t.corrected)))))

(rule
 (alias subdir-stanza)
 (deps (package dune) (source_tree test-cases/subdir-stanza))
 (action
  (chdir
   test-cases/subdir-stanza
   (progn (run dune-cram run run.t) (diff? run.t run.t.corrected)))))

(rule
 (alias subst)
 (deps (package dune) (source_tree test-cases/subst))
 (action
  (chdir
   test-cases/subst
   (progn (run dune-cram run run.t) (diff? run.t run.t.corrected)))))

(rule
 (alias syntax-versioning)
 (deps (package dune) (source_tree test-cases/syntax-versioning))
 (action
  (chdir
   test-cases/syntax-versioning
   (progn (run dune-cram run run.t) (diff? run.t run.t.corrected)))))

(rule
 (alias target-dir-alias)
 (deps (package dune) (source_tree test-cases/target-dir-alias))
 (action
  (chdir
   test-cases/target-dir-alias
   (progn (run dune-cram run run.t) (diff? run.t run.t.corrected)))))

(rule
 (alias targets-with-vars)
 (deps (package dune) (source_tree test-cases/targets-with-vars))
 (action
  (chdir
   test-cases/targets-with-vars
   (progn (run dune-cram run run.t) (diff? run.t run.t.corrected)))))

(rule
 (alias tests-stanza)
 (deps (package dune) (source_tree test-cases/tests-stanza))
 (action
  (chdir
   test-cases/tests-stanza
   (progn (run dune-cram run run.t) (diff? run.t run.t.corrected)))))

(rule
 (alias tests-stanza-action)
 (deps (package dune) (source_tree test-cases/tests-stanza-action))
 (action
  (chdir
   test-cases/tests-stanza-action
   (progn (run dune-cram run run.t) (diff? run.t run.t.corrected)))))

(rule
 (alias tests-stanza-action-syntax-version)
 (deps
  (package dune)
  (source_tree test-cases/tests-stanza-action-syntax-version))
 (action
  (chdir
   test-cases/tests-stanza-action-syntax-version
   (progn (run dune-cram run run.t) (diff? run.t run.t.corrected)))))

(rule
 (alias toplevel-integration)
 (deps (package dune) (source_tree test-cases/toplevel-integration))
 (action
  (chdir
   test-cases/toplevel-integration
   (progn (run dune-cram run run.t) (diff? run.t run.t.corrected)))))

(rule
 (alias toplevel-stanza)
 (deps (package dune) (source_tree test-cases/toplevel-stanza))
 (action
  (chdir
   test-cases/toplevel-stanza
   (progn (run dune-cram run run.t) (diff? run.t run.t.corrected))))
 (enabled_if (>= %{ocaml_version} 4.05.0)))

(rule
 (alias trace-file)
 (deps (package dune) (source_tree test-cases/trace-file))
 (action
  (chdir
   test-cases/trace-file
   (progn (run dune-cram run run.t) (diff? run.t run.t.corrected)))))

(rule
 (alias transitive-deps-mode)
 (deps (package dune) (source_tree test-cases/transitive-deps-mode))
 (action
  (chdir
   test-cases/transitive-deps-mode
   (progn (run dune-cram run run.t) (diff? run.t run.t.corrected)))))

(rule
 (alias unreadable-src)
 (deps (package dune) (source_tree test-cases/unreadable-src))
 (action
  (chdir
   test-cases/unreadable-src
   (progn (run dune-cram run run.t) (diff? run.t run.t.corrected)))))

(rule
 (alias unused-preprocessor-deps)
 (deps (package dune) (source_tree test-cases/unused-preprocessor-deps))
 (action
  (chdir
   test-cases/unused-preprocessor-deps
   (progn (run dune-cram run run.t) (diff? run.t run.t.corrected)))))

(rule
 (alias upgrader)
 (deps (package dune) (source_tree test-cases/upgrader))
 (action
  (chdir
   test-cases/upgrader
   (progn (run dune-cram run run.t) (diff? run.t run.t.corrected)))))

(rule
 (alias use-meta)
 (deps (package dune) (source_tree test-cases/use-meta))
 (action
  (chdir
   test-cases/use-meta
   (progn (run dune-cram run run.t) (diff? run.t run.t.corrected)))))

(rule
 (alias utop-utop-default)
 (deps (package dune) (source_tree test-cases/utop/utop-default))
 (action
  (chdir
   test-cases/utop/utop-default
   (progn (run dune-cram run run.t) (diff? run.t run.t.corrected))))
 (enabled_if (>= %{ocaml_version} 4.05.0)))

(rule
 (alias utop-utop-default-implementation)
 (deps
  (package dune)
  (source_tree test-cases/utop/utop-default-implementation))
 (action
  (chdir
   test-cases/utop/utop-default-implementation
   (progn (run dune-cram run run.t) (diff? run.t run.t.corrected))))
 (enabled_if (>= %{ocaml_version} 4.05.0)))

(rule
 (alias utop-utop-ppx-rewriters)
 (deps (package dune) (source_tree test-cases/utop/utop-ppx-rewriters))
 (action
  (chdir
   test-cases/utop/utop-ppx-rewriters
   (progn (run dune-cram run run.t) (diff? run.t run.t.corrected))))
 (enabled_if (>= %{ocaml_version} 4.05.0)))

(rule
 (alias utop-utop-simple)
 (deps (package dune) (source_tree test-cases/utop/utop-simple))
 (action
  (chdir
   test-cases/utop/utop-simple
   (progn (run dune-cram run run.t) (diff? run.t run.t.corrected))))
 (enabled_if (>= %{ocaml_version} 4.05.0)))

(rule
 (alias variables-for-artifacts)
 (deps (package dune) (source_tree test-cases/variables-for-artifacts))
 (action
  (chdir
   test-cases/variables-for-artifacts
   (progn (run dune-cram run run.t) (diff? run.t run.t.corrected)))))

(rule
 (alias variants)
 (deps (package dune) (source_tree test-cases/variants))
 (action
  (chdir
   test-cases/variants
   (progn (run dune-cram run run.t) (diff? run.t run.t.corrected)))))

(rule
 (alias variants-variant-dep-stack)
 (deps (package dune) (source_tree test-cases/variants/variant-dep-stack))
 (action
  (chdir
   test-cases/variants/variant-dep-stack
   (progn (run dune-cram run run.t) (diff? run.t run.t.corrected)))))

(rule
 (alias variants-variants-external-declaration)
 (deps
  (package dune)
  (source_tree test-cases/variants/variants-external-declaration))
 (action
  (chdir
   test-cases/variants/variants-external-declaration
   (progn (run dune-cram run run.t) (diff? run.t run.t.corrected)))))

(rule
 (alias variants-variants-external-declaration-conflict)
 (deps
  (package dune)
  (source_tree test-cases/variants/variants-external-declaration-conflict))
 (action
  (chdir
   test-cases/variants/variants-external-declaration-conflict
   (progn (run dune-cram run run.t) (diff? run.t run.t.corrected)))))

(rule
 (alias variants-variants-multi-project)
 (deps
  (package dune)
  (source_tree test-cases/variants/variants-multi-project))
 (action
  (chdir
   test-cases/variants/variants-multi-project
   (progn (run dune-cram run run.t) (diff? run.t run.t.corrected)))))

(rule
 (alias variants-variants-only-package)
 (deps (package dune) (source_tree test-cases/variants/variants-only-package))
 (action
  (chdir
   test-cases/variants/variants-only-package
   (progn (run dune-cram run run.t) (diff? run.t run.t.corrected)))))

(rule
 (alias variants-variants-rogue-impl)
 (deps (package dune) (source_tree test-cases/variants/variants-rogue-impl))
 (action
  (chdir
   test-cases/variants/variants-rogue-impl
   (progn (run dune-cram run run.t) (diff? run.t run.t.corrected)))))

(rule
 (alias variants-variants-wrong-external-declaration)
 (deps
  (package dune)
  (source_tree test-cases/variants/variants-wrong-external-declaration))
 (action
  (chdir
   test-cases/variants/variants-wrong-external-declaration
   (progn (run dune-cram run run.t) (diff? run.t run.t.corrected)))))

(rule
 (alias vendor)
 (deps (package dune) (source_tree test-cases/vendor))
 (action
  (chdir
   test-cases/vendor
   (progn (run dune-cram run run.t) (diff? run.t run.t.corrected)))))

(rule
 (alias vendor-dash-p-and-vendored-packages)
 (deps
  (package dune)
  (source_tree test-cases/vendor/dash-p-and-vendored-packages))
 (action
  (chdir
   test-cases/vendor/dash-p-and-vendored-packages
   (progn (run dune-cram run run.t) (diff? run.t run.t.corrected)))))

(rule
 (alias vendor-do-not-install-vendored-packages)
 (deps
  (package dune)
  (source_tree test-cases/vendor/do-not-install-vendored-packages))
 (action
  (chdir
   test-cases/vendor/do-not-install-vendored-packages
   (progn (run dune-cram run run.t) (diff? run.t run.t.corrected)))))

(rule
 (alias virtual-libraries-double-implementation)
 (deps
  (package dune)
  (source_tree test-cases/virtual-libraries/double-implementation))
 (action
  (chdir
   test-cases/virtual-libraries/double-implementation
   (progn (run dune-cram run run.t) (diff? run.t run.t.corrected)))))

(rule
 (alias virtual-libraries-dune-package-info)
 (deps
  (package dune)
  (source_tree test-cases/virtual-libraries/dune-package-info))
 (action
  (chdir
   test-cases/virtual-libraries/dune-package-info
   (progn (run dune-cram run run.t) (diff? run.t run.t.corrected)))))

(rule
 (alias virtual-libraries-github2896)
 (deps (package dune) (source_tree test-cases/virtual-libraries/github2896))
 (action
  (chdir
   test-cases/virtual-libraries/github2896
   (progn (run dune-cram run run.t) (diff? run.t run.t.corrected)))))

(rule
 (alias virtual-libraries-impl-lib-interface-module)
 (deps
  (package dune)
  (source_tree test-cases/virtual-libraries/impl-lib-interface-module))
 (action
  (chdir
   test-cases/virtual-libraries/impl-lib-interface-module
   (progn (run dune-cram run run.t) (diff? run.t run.t.corrected)))))

(rule
 (alias virtual-libraries-impl-not-virtual)
 (deps
  (package dune)
  (source_tree test-cases/virtual-libraries/impl-not-virtual))
 (action
  (chdir
   test-cases/virtual-libraries/impl-not-virtual
   (progn (run dune-cram run run.t) (diff? run.t run.t.corrected)))))

(rule
 (alias virtual-libraries-impl-not-virtual-external)
 (deps
  (package dune)
  (source_tree test-cases/virtual-libraries/impl-not-virtual-external)
  (package dune-configurator))
 (action
  (chdir
   test-cases/virtual-libraries/impl-not-virtual-external
   (progn (run dune-cram run run.t) (diff? run.t run.t.corrected)))))

(rule
 (alias virtual-libraries-impl-private-modules)
 (deps
  (package dune)
  (source_tree test-cases/virtual-libraries/impl-private-modules))
 (action
  (chdir
   test-cases/virtual-libraries/impl-private-modules
   (progn (run dune-cram run run.t) (diff? run.t run.t.corrected)))))

(rule
 (alias virtual-libraries-impl-public-modules)
 (deps
  (package dune)
  (source_tree test-cases/virtual-libraries/impl-public-modules))
 (action
  (chdir
   test-cases/virtual-libraries/impl-public-modules
   (progn (run dune-cram run run.t) (diff? run.t run.t.corrected)))))

(rule
 (alias virtual-libraries-impl-using-vlib-modules)
 (deps
  (package dune)
  (source_tree test-cases/virtual-libraries/impl-using-vlib-modules))
 (action
  (chdir
   test-cases/virtual-libraries/impl-using-vlib-modules
   (progn (run dune-cram run run.t) (diff? run.t run.t.corrected)))))

(rule
 (alias virtual-libraries-implements-external)
 (deps
  (package dune)
  (source_tree test-cases/virtual-libraries/implements-external))
 (action
  (chdir
   test-cases/virtual-libraries/implements-external
   (progn (run dune-cram run run.t) (diff? run.t run.t.corrected)))))

(rule
 (alias virtual-libraries-install-file)
 (deps (package dune) (source_tree test-cases/virtual-libraries/install-file))
 (action
  (chdir
   test-cases/virtual-libraries/install-file
   (progn (run dune-cram run run.t) (diff? run.t run.t.corrected)))))

(rule
 (alias virtual-libraries-invalid-virtual-lib)
 (deps
  (package dune)
  (source_tree test-cases/virtual-libraries/invalid-virtual-lib))
 (action
  (chdir
   test-cases/virtual-libraries/invalid-virtual-lib
   (progn (run dune-cram run run.t) (diff? run.t run.t.corrected)))))

(rule
 (alias virtual-libraries-missing-implementation)
 (deps
  (package dune)
  (source_tree test-cases/virtual-libraries/missing-implementation))
 (action
  (chdir
   test-cases/virtual-libraries/missing-implementation
   (progn (run dune-cram run run.t) (diff? run.t run.t.corrected)))))

(rule
 (alias virtual-libraries-module-fields)
 (deps
  (package dune)
  (source_tree test-cases/virtual-libraries/module-fields))
 (action
  (chdir
   test-cases/virtual-libraries/module-fields
   (progn (run dune-cram run run.t) (diff? run.t run.t.corrected)))))

(rule
 (alias virtual-libraries-no-vlib-present)
 (deps
  (package dune)
  (source_tree test-cases/virtual-libraries/no-vlib-present))
 (action
  (chdir
   test-cases/virtual-libraries/no-vlib-present
   (progn (run dune-cram run run.t) (diff? run.t run.t.corrected)))))

(rule
 (alias virtual-libraries-preprocess)
 (deps (package dune) (source_tree test-cases/virtual-libraries/preprocess))
 (action
  (chdir
   test-cases/virtual-libraries/preprocess
   (progn (run dune-cram run run.t) (diff? run.t run.t.corrected)))))

(rule
 (alias virtual-libraries-private-modules-overlapping-names)
 (deps
  (package dune)
  (source_tree test-cases/virtual-libraries/private-modules-overlapping-names))
 (action
  (chdir
   test-cases/virtual-libraries/private-modules-overlapping-names
   (progn (run dune-cram run run.t) (diff? run.t run.t.corrected)))))

(rule
 (alias virtual-libraries-unwrapped)
 (deps (package dune) (source_tree test-cases/virtual-libraries/unwrapped))
 (action
  (chdir
   test-cases/virtual-libraries/unwrapped
   (progn (run dune-cram run run.t) (diff? run.t run.t.corrected)))))

(rule
 (alias virtual-libraries-variants-simple)
 (deps
  (package dune)
  (source_tree test-cases/virtual-libraries/variants-simple))
 (action
  (chdir
   test-cases/virtual-libraries/variants-simple
   (progn (run dune-cram run run.t) (diff? run.t run.t.corrected)))))

(rule
 (alias virtual-libraries-variants-sub-module)
 (deps
  (package dune)
  (source_tree test-cases/virtual-libraries/variants-sub-module))
 (action
  (chdir
   test-cases/virtual-libraries/variants-sub-module
   (progn (run dune-cram run run.t) (diff? run.t run.t.corrected)))))

(rule
 (alias virtual-libraries-vlib-default-impl)
 (deps
  (package dune)
  (source_tree test-cases/virtual-libraries/vlib-default-impl))
 (action
  (chdir
   test-cases/virtual-libraries/vlib-default-impl
   (progn (run dune-cram run run.t) (diff? run.t run.t.corrected)))))

(rule
 (alias virtual-libraries-vlib-wrong-default-impl)
 (deps
  (package dune)
  (source_tree test-cases/virtual-libraries/vlib-wrong-default-impl))
 (action
  (chdir
   test-cases/virtual-libraries/vlib-wrong-default-impl
   (progn (run dune-cram run run.t) (diff? run.t run.t.corrected)))))

(rule
 (alias windows-diff)
 (deps (package dune) (source_tree test-cases/windows-diff))
 (action
  (chdir
   test-cases/windows-diff
   (progn (run dune-cram run run.t) (diff? run.t run.t.corrected)))))

(rule
 (alias with-exit-codes)
 (deps (package dune) (source_tree test-cases/with-exit-codes))
 (action
  (chdir
   test-cases/with-exit-codes
   (progn (run dune-cram run run.t) (diff? run.t run.t.corrected)))))

(rule
 (alias with-nested-exit-codes)
 (deps (package dune) (source_tree test-cases/with-nested-exit-codes))
 (action
  (chdir
   test-cases/with-nested-exit-codes
   (progn (run dune-cram run run.t) (diff? run.t run.t.corrected)))))

(rule
 (alias with-stdin-from)
 (deps (package dune) (source_tree test-cases/with-stdin-from))
 (action
  (chdir
   test-cases/with-stdin-from
   (progn (run dune-cram run run.t) (diff? run.t run.t.corrected)))))

(rule
 (alias with-unsupported-nested-exit-codes)
 (deps
  (package dune)
  (source_tree test-cases/with-unsupported-nested-exit-codes))
 (action
  (chdir
   test-cases/with-unsupported-nested-exit-codes
   (progn (run dune-cram run run.t) (diff? run.t run.t.corrected)))))

(rule
 (alias workspace-paths)
 (deps (package dune) (source_tree test-cases/workspace-paths))
 (action
  (chdir
   test-cases/workspace-paths
   (progn (run dune-cram run run.t) (diff? run.t run.t.corrected)))))

(rule
 (alias workspaces)
 (deps (package dune) (source_tree test-cases/workspaces))
 (action
  (chdir
   test-cases/workspaces
   (progn (run dune-cram run run.t) (diff? run.t run.t.corrected)))))

(rule
 (alias wrapped-false-main-module-name)
 (deps (package dune) (source_tree test-cases/wrapped-false-main-module-name))
 (action
  (chdir
   test-cases/wrapped-false-main-module-name
   (progn (run dune-cram run run.t) (diff? run.t run.t.corrected)))))

(rule
 (alias wrapped-transition)
 (deps (package dune) (source_tree test-cases/wrapped-transition))
 (action
  (chdir
   test-cases/wrapped-transition
   (progn (run dune-cram run run.t) (diff? run.t run.t.corrected))))
 (enabled_if (>= %{ocaml_version} 4.06.0)))

(rule
 (alias write-permissions)
 (deps (package dune) (source_tree test-cases/write-permissions))
 (action
  (chdir
   test-cases/write-permissions
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
  (alias cmdliner-dep-conf)
  (alias contents-depends-on-glob)
  (alias copy-files-non-sub-dir-error)
  (alias copy_files)
  (alias corrections)
  (alias cross-compilation)
  (alias custom-build-dir)
  (alias custom-cross-compilation)
  (alias cxx-extension)
  (alias cycle-detection-symlinks-2863)
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
  (alias dune-project-meta)
  (alias dune-project-meta-github2927)
  (alias dune-project-no-opam)
  (alias dune_memory-and-the-universe)
  (alias dup-fields)
  (alias duplicate-c-cxx)
  (alias duplicate-c-cxx-obj)
  (alias duplicate-project-names)
  (alias duplicate-target-no-loc)
  (alias embed-jbuild)
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
  (alias github3046)
  (alias github3180)
  (alias github3306)
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
  (alias inline_tests)
  (alias inline_tests-multi-mode)
  (alias install-dry-run)
  (alias install-libdir)
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
  (alias merlin-src-dirs-of-deps)
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
  (alias pkg-config-quoting)
  (alias plugin-mode)
  (alias ppx-rewriter)
  (alias ppx-runtime-dependencies)
  (alias preprocess-with-action)
  (alias private-modules)
  (alias private-public-overlap)
  (alias project-root)
  (alias promote)
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
  (alias variants)
  (alias variants-variant-dep-stack)
  (alias variants-variants-external-declaration)
  (alias variants-variants-external-declaration-conflict)
  (alias variants-variants-multi-project)
  (alias variants-variants-only-package)
  (alias variants-variants-rogue-impl)
  (alias variants-variants-wrong-external-declaration)
  (alias vendor)
  (alias vendor-dash-p-and-vendored-packages)
  (alias vendor-do-not-install-vendored-packages)
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
  (alias corrections)
  (alias custom-build-dir)
  (alias custom-cross-compilation)
  (alias cxx-extension)
  (alias cycle-detection-symlinks-2863)
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
  (alias dune-project-meta)
  (alias dune-project-meta-github2927)
  (alias dune-project-no-opam)
  (alias dune_memory-and-the-universe)
  (alias dup-fields)
  (alias duplicate-c-cxx)
  (alias duplicate-c-cxx-obj)
  (alias duplicate-project-names)
  (alias duplicate-target-no-loc)
  (alias embed-jbuild)
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
  (alias github3046)
  (alias github3180)
  (alias github3306)
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
  (alias inline_tests)
  (alias inline_tests-multi-mode)
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
  (alias lib)
  (alias lib-available)
  (alias lib-errors)
  (alias link-deps)
  (alias link-includes)
  (alias loop)
  (alias macro-expand-error)
  (alias menhir-env)
  (alias merlin-allow_approximate_merlin)
  (alias merlin-src-dirs-of-deps)
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
  (alias odig-files-workspace)
  (alias old-dune-subsystem)
  (alias optional)
  (alias optional-executable)
  (alias output-obj)
  (alias path-variables)
  (alias pkg-config-quoting)
  (alias plugin-mode)
  (alias preprocess-with-action)
  (alias private-modules)
  (alias project-root)
  (alias promote)
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
  (alias variants)
  (alias variants-variant-dep-stack)
  (alias variants-variants-external-declaration)
  (alias variants-variants-external-declaration-conflict)
  (alias variants-variants-multi-project)
  (alias variants-variants-only-package)
  (alias variants-variants-rogue-impl)
  (alias variants-variants-wrong-external-declaration)
  (alias vendor)
  (alias vendor-dash-p-and-vendored-packages)
  (alias vendor-do-not-install-vendored-packages)
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
 (deps (alias cinaps) (alias env-envs-and-contexts) (alias fdo)))

(alias (name runtest-js) (deps (alias explicit_js_mode) (alias js_of_ocaml)))

(alias (name runtest-coq) (deps (alias coq) (alias coq-extract)))