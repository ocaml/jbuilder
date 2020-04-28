Test that trying to specify a variant for not an implementation results in an
appropriate error message.
  $ dune build --root variant-not-implementation
  Entering directory 'variant-not-implementation'
  File "dune", line 4, characters 10-13:
  4 |  (variant foo))
                ^^^
  Error: Only implementations can specify a variant.
  [1]

Having multiple implementations of the same library with respect to selected
variants results in an appropriate error message.
  $ dune build --root multiple-implementations-for-variants
  Entering directory 'multiple-implementations-for-variants'
  File "dune", line 4, characters 11-14:
  4 |  (variants a b))
                 ^^^
  Error: Multiple solutions for the implementation of "vlib" with variants a
  and b:
  - "lib_b" in _build/default/lib.b (variant b)
  - "lib2_a" in _build/default/lib2$ext_lib (variant a)
  [1]

Basic sample using variants and a default library.
  $ dune build --root variants-base
  Entering directory 'variants-base'
           bar alias default
  hello from lib.test

Check that implementations are chosen according to manual specification, then
variants and finally default implementation.
  $ dune build --root resolution-priority
  Entering directory 'resolution-priority'
           bar alias default
  hi from direct.ocaml
  hi from variant.c
  hi from test.default

Check that variant data is installed in the dune package file.

  $ dune build @install --root dune-package
  Entering directory 'dune-package'
  $ cat  dune-package/_build/install/default/lib/a/dune-package | sed "s/(lang dune .*)/(lang dune <version>)/"
  (lang dune <version>)
  (name a)
  (library
   (name a)
   (kind normal)
   (archives (byte lib.cma) (native lib.cmxa))
   (plugins (byte lib.cma) (native lib.cmxs))
   (native_archives lib$ext_lib)
   (requires b)
   (implements b)
   (main_module_name B)
   (modes byte native)
   (modules
    (wrapped
     (main_module_name B)
     (modules
      ((name X) (obj_name b__X) (visibility public) (kind impl_vmodule) (impl)))
     (alias_module
      (name B__a__)
      (obj_name b__a__)
      (visibility public)
      (kind alias)
      (impl))
     (wrapped true))))
  $ cat  dune-package/_build/install/default/lib/b/dune-package | sed "s/(lang dune .*)/(lang dune <version>)/"
  (lang dune <version>)
  (name b)
  (library
   (name b)
   (kind normal)
   (virtual)
   (native_archives lib$ext_lib)
   (known_implementations (test a))
   (main_module_name B)
   (modes byte native)
   (modules
    (wrapped
     (main_module_name B)
     (modules
      ((name X) (obj_name b__X) (visibility public) (kind virtual) (intf)))
     (alias_module
      (name B)
      (obj_name b)
      (visibility public)
      (kind alias)
      (impl))
     (wrapped true))))

Test variants for an external library

First we create an external library and implementation
  $ dune build --root external/lib @install
  Entering directory 'external/lib'

Then we make sure that it works fine.
  $ env OCAMLPATH=external/lib/_build/install/default/lib dune build --root external/exe --debug-dependency-path
  Entering directory 'external/exe'
           bar alias default
  hey

Variant ambiguity is forbidden even if a concrete implementation is provided.
  $ dune build --root variant-with-concrete-impl
  Entering directory 'variant-with-concrete-impl'
  Error: Two implementations of vlib have the same variant "default":
  - lib2_default (lib2.default/dune:1)
  - lib_default (lib.default/dune:1)
  [1]

Don't fail when the same library is defined in multiple scopes.
  $ dune build --root same-lib-in-multiple-scopes
  Entering directory 'same-lib-in-multiple-scopes'
