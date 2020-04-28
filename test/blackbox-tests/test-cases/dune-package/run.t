  $ dune build
  $ cat _build/install/default/lib/a/dune-package | sed "s/(lang dune .*)/(lang dune <version>)/"
  (lang dune <version>)
  (name a)
  (library
   (name a)
   (kind normal)
   (archives (byte lib.cma) (native lib.cmxa))
   (plugins (byte lib.cma) (native lib.cmxs))
   (native_archives lib$ext_lib)
   (main_module_name A)
   (modes byte native)
   (modules
    (wrapped
     (main_module_name A)
     (modules ((name X) (obj_name a__X) (visibility public) (impl)))
     (alias_module
      (name A)
      (obj_name a)
      (visibility public)
      (kind alias)
      (impl))
     (wrapped true))))
  (library
   (name a.b.c)
   (kind normal)
   (archives (byte b/c/lib.cma) (native b/c/lib.cmxa))
   (plugins (byte b/c/lib.cma) (native b/c/lib.cmxs))
   (native_archives b/c/lib$ext_lib)
   (main_module_name C)
   (modes byte native)
   (obj_dir (private_dir .private))
   (modules
    (wrapped
     (main_module_name C)
     (modules ((name Y) (obj_name c__Y) (visibility private) (impl) (intf)))
     (alias_module
      (name C)
      (obj_name c)
      (visibility public)
      (kind alias)
      (impl))
     (wrapped true))))

Build with "--store-orig-source-dir" profile
  $ dune build --store-orig-source-dir
  $ cat _build/install/default/lib/a/dune-package | grep -A 1 '(orig_src_dir'
   (orig_src_dir
    $TESTCASE_ROOT)
  --
   (orig_src_dir
    $TESTCASE_ROOT)

Build with "DUNE_STORE_ORIG_SOURCE_DIR=true" profile
  $ DUNE_STORE_ORIG_SOURCE_DIR=true dune build
  $ cat _build/install/default/lib/a/dune-package | grep -A 1 '(orig_src_dir'
   (orig_src_dir
    $TESTCASE_ROOT)
  --
   (orig_src_dir
    $TESTCASE_ROOT)
