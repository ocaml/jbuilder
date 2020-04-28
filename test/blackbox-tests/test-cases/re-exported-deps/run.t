dependencies can be exported transitively:
  $ dune exec ./foo.exe --root transitive
  Entering directory 'transitive'
  Entering directory 'transitive'

transtive deps expressed in the dune-package

  $ dune build @install --root transitive
  Entering directory 'transitive'
  $ cat transitive/_build/install/default/lib/pkg/dune-package | sed "s/(lang dune .*)/(lang dune <version>)/"
  (lang dune <version>)
  (name pkg)
  (library
   (name pkg.aaa)
   (kind normal)
   (archives (byte aaa/lib.cma) (native aaa/lib.cmxa))
   (plugins (byte aaa/lib.cma) (native aaa/lib.cmxs))
   (native_archives aaa/lib$ext_lib)
   (requires pkg.ccc (re_export pkg.bbb))
   (main_module_name Aaa)
   (modes byte native)
   (modules (singleton (name Aaa) (obj_name aaa) (visibility public) (impl))))
  (library
   (name pkg.bbb)
   (kind normal)
   (archives (byte bbb/lib.cma) (native bbb/lib.cmxa))
   (plugins (byte bbb/lib.cma) (native bbb/lib.cmxs))
   (native_archives bbb/lib$ext_lib)
   (requires (re_export pkg.ccc))
   (main_module_name Bbb)
   (modes byte native)
   (modules (singleton (name Bbb) (obj_name bbb) (visibility public) (impl))))
  (library
   (name pkg.ccc)
   (kind normal)
   (archives (byte ccc/lib.cma) (native ccc/lib.cmxa))
   (plugins (byte ccc/lib.cma) (native ccc/lib.cmxs))
   (native_archives ccc/lib$ext_lib)
   (main_module_name Ccc)
   (modes byte native)
   (modules (singleton (name Ccc) (obj_name ccc) (visibility public) (impl))))

Re-exporting deps in executables isn't allowed
  $ dune build --root re-export-exe @all
  Entering directory 're-export-exe'
  File "dune", line 7, characters 12-27:
  7 |  (libraries (re_export foo)))
                  ^^^^^^^^^^^^^^^
  Error: re_export is not allowed here
  [1]
