Include variants and implementation information in dune-package
  $ dune build | sed "s/(lang dune .*)/(lang dune <version>)/"
  (lang dune <version>)
  (name foo)
  (library
   (name foo.impl)
   (kind normal)
   (archives (byte impl/lib.cma) (native impl/lib.cmxa))
   (plugins (byte impl/lib.cma) (native impl/lib.cmxs))
   (native_archives impl/lib$ext_lib)
   (requires foo.vlib)
   (implements foo.vlib)
   (main_module_name Vlib)
   (modes byte native)
   (modules
    (wrapped
     (main_module_name Vlib)
     (modules
      ((name Vmod)
       (obj_name vlib__Vmod)
       (visibility public)
       (kind impl_vmodule)
       (impl)))
     (alias_module
      (name Vlib__impl__)
      (obj_name vlib__impl__)
      (visibility public)
      (kind alias)
      (impl))
     (wrapped true))))
  (library
   (name foo.vlib)
   (kind normal)
   (virtual)
   (native_archives vlib/lib$ext_lib)
   (main_module_name Vlib)
   (modes byte native)
   (modules
    (wrapped
     (main_module_name Vlib)
     (modules
      ((name Vmod)
       (obj_name vlib__Vmod)
       (visibility public)
       (kind virtual)
       (intf)))
     (alias_module
      (name Vlib)
      (obj_name vlib)
      (visibility public)
      (kind alias)
      (impl))
     (wrapped true))))
