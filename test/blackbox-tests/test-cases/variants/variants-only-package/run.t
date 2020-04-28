Check that local variant implementations are correctly exported in the list of
known_implementations implementations when using -p

  $ (cd project && dune build -p vlibfoo)

  $ cat project/_build/install/default/lib/vlibfoo/dune-package | sed "s/(lang dune .*)/(lang dune <version>)/"
  (lang dune <version>)
  (name vlibfoo)
  (library
   (name vlibfoo)
   (kind normal)
   (virtual)
   (native_archives lib$ext_lib)
   (known_implementations (somevariant implfoo))
   (main_module_name Vlibfoo)
   (modes byte native)
   (modules
    (singleton
     (name Vlibfoo)
     (obj_name vlibfoo)
     (visibility public)
     (kind virtual)
     (intf))))

Also check that the implementation correctly builds while using -p when part of the same project

  $ cp -r project/_build/ opam

  $ (cd project && env OCAMLPATH=../opam/install/default/lib dune build -p implfoo)

And fail if it's not part of the same project.

  $ (cd project-2 && env OCAMLPATH=../opam/install/default/lib dune build -p impl2foo)
  File "impl2foo/dune", line 4, characters 13-20:
  4 |  (implements vlibfoo)
                   ^^^^^^^
  Error: Virtual library "vlibfoo" does not know about implementation
  "impl2foo" with variant "somevariant2". Instead of using (variant
  somevariant2) here, you need to reference it in the virtual library project,
  using the external_variant stanza:
  (external_variant
    (virtual_library vlibfoo)
    (variant somevariant2)
    (implementation impl2foo))
  [1]
