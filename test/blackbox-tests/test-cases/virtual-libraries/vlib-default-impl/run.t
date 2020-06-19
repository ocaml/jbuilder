Test that trying to specify a default implementation for a non-virtual library results
in an appropriate error message.
  $ dune build --root default-impl-not-virtual-lib
  Entering directory 'default-impl-not-virtual-lib'
  File "dune", line 4, characters 25-33:
  4 |  (default_implementation lib.impl))
                               ^^^^^^^^
  Error: Only virtual libraries can specify a default implementation.
  [1]

Basic sample selecting implementation according to default library.
  $ dune build --root default-impl
  Entering directory 'default-impl'
           bar alias default
  hi from lib.default

Check that default implementation data is installed in the dune package file.
  $ dune build --root dune-package
  Entering directory 'dune-package'
  $ dune_cmd cat dune-package/_build/install/default/lib/a/dune-package | sed "s/(lang dune .*)/(lang dune <version>)/"
  (lang dune <version>)
  (name a)
  (library
   (name a)
   (kind normal)
   (virtual)
   (native_archives a$ext_lib)
   (default_implementation a.default-impl)
   (main_module_name A)
   (modes byte native)
   (modules
    (wrapped
     (main_module_name A)
     (modules
      ((name X) (obj_name a__X) (visibility public) (kind virtual) (intf)))
     (alias_module
      (name A)
      (obj_name a)
      (visibility public)
      (kind alias)
      (impl))
     (wrapped true))))
  (library
   (name a.default-impl)
   (kind normal)
   (archives
    (byte default-impl/a_default.cma)
    (native default-impl/a_default.cmxa))
   (plugins
    (byte default-impl/a_default.cma)
    (native default-impl/a_default.cmxs))
   (native_archives default-impl/a_default$ext_lib)
   (implements a)
   (main_module_name A)
   (modes byte native)
   (modules
    (wrapped
     (main_module_name A)
     (modules
      ((name X) (obj_name a__X) (visibility public) (kind impl_vmodule) (impl)))
     (alias_module
      (name A__a_default__)
      (obj_name a__a_default__)
      (visibility public)
      (kind alias)
      (impl))
     (wrapped true))))

Test default implementation for an external library

First we create an external library and implementation
  $ dune build --root external/lib @install
  Entering directory 'external/lib'

Then we make sure that it works fine.
  $ env OCAMLPATH=external/lib/_build/install/default/lib dune build --root external/exe --debug-dependency-path
  Entering directory 'external/exe'
           bar alias default
  hey
