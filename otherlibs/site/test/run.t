Test embedding of sites locations information
-----------------------------------

  $ mkdir -p a b c

  $ for i in a b d; do
  > mkdir -p $i
  > cat >$i/dune-project <<EOF
  > (lang dune 2.8)
  > (using dune_site 0.1)
  > (name $i)
  > (package (name $i) (sites (share data)))
  > EOF
  > done

  $ for i in c; do
  >   mkdir -p $i
  >   cat >$i/dune-project <<EOF
  > (lang dune 2.8)
  > (using dune_site 0.1)
  > (name $i)
  > (package (name $i) (sites (share data) (lib plugins)))
  > EOF
  > done

  $ cat >a/dune <<EOF
  > (library
  >  (public_name a)
  >  (libraries dune-site))
  > (generate_sites_module (module sites) (sites a))
  > EOF

  $ cat >a/a.ml <<EOF
  > let v = "a"
  > let () = Printf.printf "run a\n%!"
  > let () = List.iter (Printf.printf "a: %s\n%!") Sites.Sites.data
  > EOF

  $ cat >b/dune <<EOF
  > (library
  >  (public_name b)
  >  (libraries c.register dune-site))
  > (generate_sites_module (module sites) (sites b))
  > (plugin (name c-plugins-b) (libraries b) (site (c plugins)))
  > (install (section (site (b data))) (files info.txt))
  > EOF

  $ cat >b/b.ml <<EOF
  > let v = "b"
  > let () = Printf.printf "run b\n%!"
  > let () = C_register.b_registered := true
  > let () = List.iter (Printf.printf "b: %s\n%!") Sites.Sites.data
  > let () =
  >     let test d = Sys.file_exists (Filename.concat d "info.txt") in
  >     let found = List.exists test Sites.Sites.data in
  >     Printf.printf "info.txt is found: %b\n%!" found
  > EOF

  $ cat >b/info.txt <<EOF
  > Lorem
  > EOF

  $ cat >d/dune <<EOF
  > (library
  >  (public_name d)
  >  (libraries c.register dune-site non-existent-library)
  >  (optional))
  > (generate_sites_module (module sites) (sites d))
  > (plugin (name c-plugins-d) (libraries d) (site (c plugins)) (optional))
  > (install (section (site (d data))) (files info.txt))
  > EOF

  $ cat >d/d.ml <<EOF
  > let v = "d"
  > let () = Printf.printf "run d\n%!"
  > let () = C_register.d_registered := true
  > let () = List.iter (Printf.printf "d: %s\n%!") Sites.Sites.data
  > let () =
  >     let test d = Sys.file_exists (Filename.concat d "info.txt") in
  >     let found = List.exists test Sites.Sites.data in
  >     Printf.printf "info.txt is found: %d\n%!" found
  > EOF

  $ cat >d/info.txt <<EOF
  > Lorem
  > EOF


  $ cat >c/dune <<EOF
  > (executable
  >  (public_name c)
  >  (promote (until-clean))
  >  (modules c sites)
  >  (libraries a c.register dune-site dune-site.plugins))
  > (library
  >  (public_name c.register)
  >  (name c_register)
  >  (modules c_register))
  > (generate_sites_module (module sites) (sourceroot) (plugins (c plugins)))
  > (rule
  >  (targets out.log)
  >  (deps (package c) ../script.ml)
  >  (action (with-stdout-to out.log (chdir ".." (run %{bin:c})))))
  > (rule (alias print_ocamlopt) (action (echo %{ocamlopt})))
  > EOF

  $ cat >c/c_register.ml <<EOF
  > let b_registered = ref false
  > EOF

  $ cat >c/c.ml <<EOF
  > let () = Printf.printf "run c: %s linked b_registered:%b\n%!"
  >   A.v !C_register.b_registered
  > let () = match Sites.sourceroot with
  >       | Some d -> Printf.printf "sourceroot is %S\n%!" d
  >       | None -> Printf.printf "no sourceroot\n%!"
  > let () = List.iter (Printf.printf "c: %s\n%!") Sites.Sites.data
  > let () = Printf.printf "b is available: %b\n%!" (Dune_site_plugins.V1.available "b")
  > let () = Sites.Plugins.Plugins.load_all ()
  > let () = Printf.printf "run c: b_registered:%b\n%!" !C_register.b_registered
  > let res,stdout,stderr = Dune_site_plugins.V1.load_script "script.ml"
  > let () = match res with
  >   | \`Compilation_failed -> Printf.printf "Compilation_failed\n%s\n%s%!"
  > (String.concat "\n" stdout) (String.concat "\n" stderr)
  >   | \`Ok -> ()
  > EOF

  $ cat > dune-project << EOF
  > (lang dune 2.2)
  > EOF

  $ cat > script.ml <<EOF
  > let () = Printf.printf "I'm loaded and I'm accessing B.v: %s\n%!" B.v
  > EOF

  $ dune build


Test with a normal installation
--------------------------------

  $ dune install --prefix _install 2> /dev/null

Once installed, we have the sites information:

  $ OCAMLPATH=_install/lib:$OCAMLPATH _install/bin/c
  run a
  a: $TESTCASE_ROOT/_install/share/a/data
  run c: a linked b_registered:false
  no sourceroot
  c: $TESTCASE_ROOT/_install/share/c/data
  b is available: true
  run b
  b: $TESTCASE_ROOT/_install/share/b/data
  info.txt is found: true
  run c: b_registered:true
  I'm loaded and I'm accessing B.v: b

Test with a relocatable installation
--------------------------------

  $ dune install --prefix _install_relocatable --relocatable 2> /dev/null

Once installed, we have the sites information:

  $ ln -s $(dune build @print_ocamlopt) _install_relocatable/bin/ocamlopt

  $ _install_relocatable/bin/c
  run a
  a: $TESTCASE_ROOT/_install_relocatable/share/a/data
  run c: a linked b_registered:false
  no sourceroot
  c: $TESTCASE_ROOT/_install_relocatable/share/c/data
  b is available: true
  run b
  b: $TESTCASE_ROOT/_install_relocatable/share/b/data
  info.txt is found: true
  run c: b_registered:true
  I'm loaded and I'm accessing B.v: b

  $ 

Test after moving a relocatable installation
--------------------------------

  $ mv _install_relocatable  _install_relocatable2

Once installed, we have the sites information:

  $ _install_relocatable2/bin/c
  run a
  a: $TESTCASE_ROOT/_install_relocatable2/share/a/data
  run c: a linked b_registered:false
  no sourceroot
  c: $TESTCASE_ROOT/_install_relocatable2/share/c/data
  b is available: true
  run b
  b: $TESTCASE_ROOT/_install_relocatable2/share/b/data
  info.txt is found: true
  run c: b_registered:true
  I'm loaded and I'm accessing B.v: b

Test substitution when promoting
--------------------------------

It is wrong that info.txt is not found, but to make it work it is an important
development because b is not promoted

  $ c/c.exe
  run a
  a: $TESTCASE_ROOT/_build/install/default/share/a/data
  run c: a linked b_registered:false
  sourceroot is "$TESTCASE_ROOT"
  c: $TESTCASE_ROOT/_build/install/default/share/c/data
  b is available: true
  run b
  info.txt is found: false
  run c: b_registered:true
  I'm loaded and I'm accessing B.v: b

Test within dune rules
--------------------------------
  $ dune build c/out.log

  $ cat _build/default/c/out.log
  run a
  a: $TESTCASE_ROOT/_build/install/default/share/a/data
  run c: a linked b_registered:false
  sourceroot is "$TESTCASE_ROOT"
  c: $TESTCASE_ROOT/_build/install/default/share/c/data
  b is available: true
  run b
  b: $TESTCASE_ROOT/_build/install/default/share/b/data
  info.txt is found: true
  run c: b_registered:true
  I'm loaded and I'm accessing B.v: b


Test with dune exec
--------------------------------
  $ dune exec -- c/c.exe
  run a
  a: $TESTCASE_ROOT/_build/install/default/share/a/data
  run c: a linked b_registered:false
  sourceroot is "$TESTCASE_ROOT"
  c: $TESTCASE_ROOT/_build/install/default/share/c/data
  b is available: true
  run b
  b: $TESTCASE_ROOT/_build/install/default/share/b/data
  info.txt is found: true
  run c: b_registered:true
  I'm loaded and I'm accessing B.v: b
