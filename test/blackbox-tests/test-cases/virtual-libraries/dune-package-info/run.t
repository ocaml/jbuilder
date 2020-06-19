Include variants and implementation information in dune-package
  $ dune build | sed "s/(lang dune .*)/(lang dune <version>)/"
  (lang dune <version>)
  (name foo)
  (library
   (name foo.impl)
   (kind normal)
   (archives (byte impl/impl.cma) (native impl/impl.cmxa))
   (plugins (byte impl/impl.cma) (native impl/impl.cmxs))
   (native_archives impl/impl$ext_lib)
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
   (native_archives vlib/vlib$ext_lib)
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
