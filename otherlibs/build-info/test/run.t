Test embedding of build information
-----------------------------------

  $ mkdir -p a b c

  $ for i in a b c d; do
  >   mkdir -p $i
  >   cat >$i/dune-project <<EOF
  > (lang dune 2.0)
  > (name $i)
  > (package (name $i))
  > EOF
  >   (cd $i;
  >    git init -q;
  >    git add .;
  >    git commit -q -m _;
  >    git tag -a 1.0+$i -m _)
  > done

  $ for i in a b; do
  >   cat >$i/dune <<EOF
  > (library
  >  (public_name $i))
  > EOF
  > done

  $ cat >c/dune <<EOF
  > (executable
  >  (public_name c)
  >  (promote (until-clean))
  >  (libraries a b dune-build-info))
  > EOF

  $ cat >c/c.ml <<EOF
  > module B = Build_info.V1
  > let pr fmt = Printf.printf (fmt ^^ "\n")
  > let get_version = function
  >   | Some v -> B.Version.to_string v
  >   | None -> "n/a"
  > let () =
  >   pr "%s" (get_version (B.version ()));
  >   let process_lib lib =
  >     let name = B.Statically_linked_library.name lib in
  >     let version = B.Statically_linked_library.version lib in
  >     pr "lib %s: %s" name (get_version version)
  >   in
  >   List.iter process_lib (B.Statically_linked_libraries.to_list ())
  > EOF

  $ cat > dune-project << EOF
  > (lang dune 2.0)
  > EOF

  $ dune build
  $ dune install --prefix _install 2> /dev/null

Inside _build, we have no version information:

  $ _build/default/c/c.exe | sed 's/build-info: .*/build-info: XXX/'
  n/a
  lib a: n/a
  lib b: n/a
  lib dune-build-info: XXX

  $ grep version _build/install/default/lib/a/dune-package
  [1]

  $ grep version _build/install/default/lib/a/META
  [1]

Once installed, we have the version information:

  $ _install/bin/c | sed 's/build-info: .*/build-info: XXX/'
  1.0+c
  lib a: 1.0+a
  lib b: 1.0+b
  lib dune-build-info: XXX

  $ grep version _install/lib/a/dune-package
  (version 1.0+a)

  $ grep version _install/lib/a/META
  version = "1.0+a"

Check what the generated build info module looks like:

  $ cat _build/default/c/.c.eobjs/build_info_data.ml-gen \
  >   | sed 's/"dune-build-info".*/"dune-build-info", Some "XXX"/'
  let eval s =
    let len = String.length s in
    if s.[0] = '=' then
      let colon_pos = String.index_from s 1 ':' in
      let vlen = int_of_string (String.sub s 1 (colon_pos - 1)) in
      (* This [min] is because the value might have been truncated
         if it was too large *)
      let vlen = min vlen (len - colon_pos - 1) in
      Some (String.sub s (colon_pos + 1) vlen)
    else
      None
  [@@inline never]
  
  let p1 = eval (Sys.opaque_identity "%%DUNE_PLACEHOLDER:64:vcs-describe:1:a%%%%%%%%%%%%%%%%%%%%%%%%%%")
  let p2 = eval (Sys.opaque_identity "%%DUNE_PLACEHOLDER:64:vcs-describe:1:b%%%%%%%%%%%%%%%%%%%%%%%%%%")
  let p0 = eval (Sys.opaque_identity "%%DUNE_PLACEHOLDER:64:vcs-describe:1:c%%%%%%%%%%%%%%%%%%%%%%%%%%")
  
  let version = p0
  
  let statically_linked_libraries =
    [ "a", p1
    ; "b", p2
    ; "dune-build-info", Some "XXX"
    ]
  
  let custom = None
  
  let lib_customs = []
  
  let custom_lib name = List.assoc name lib_customs
  

Test --debug-artifact-substitution
----------------------------------

The order of substitutions printed by `--debug-artifact-substitution`
is not stable across machines since it depends on the order in which
the string constant end up in the binary. To make the test stable, we
craft an example with a single placeholder to make the output stable:

  $ cat >d/dune <<EOF
  > (executable
  >  (public_name d)
  >  (promote (until-clean))
  >  (libraries dune-build-info))
  > EOF

  $ cp c/c.ml d/d.ml

  $ dune build d/d.install
  $ dune install d --prefix _install --debug-artifact-substitution 2>&1|grep -v '^\(Installing\|Deleting\)'
  Found placeholder in _build/install/default/bin/d:
  - placeholder: Vcs_describe "d"
  - evaluates to: "1.0+d"

Test substitution when promoting
--------------------------------

  $ c/c.exe | sed 's/build-info: .*/build-info: XXX/'
  1.0+c
  lib a: 1.0+a
  lib b: 1.0+b
  lib dune-build-info: XXX

Version is picked from dune-project if available
------------------------------------------------

  $ echo '(version project-version)' >> c/dune-project
  $ dune build
  $ dune install --prefix _install 2> /dev/null
  $ _install/bin/c | sed 's/build-info: .*/build-info: XXX/'
  project-version
  lib a: 1.0+a
  lib b: 1.0+b
  lib dune-build-info: XXX
