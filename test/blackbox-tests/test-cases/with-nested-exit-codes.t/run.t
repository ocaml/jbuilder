  $ cat > dune-project << EOF
  > (lang dune 2.2)
  > (using action-plugin 0.1)
  > EOF

  $ cat > dune <<EOF
  > (executable
  >  (name exit)
  >  (modules exit))
  > (rule (with-stdout-to exit.ml (echo "let () = exit (int_of_string Sys.argv.(1))")))
  > EOF

  $ cat >> dune <<EOF
  > (rule
  >  (alias f)
  >  (action (with-accepted-exit-codes 
  >           1
  >           (with-stdout-to out.txt 
  >            (run ./exit.exe 1)))))
  > EOF

  $ dune build --display=short --root . @f
        ocamlc .exit.eobjs/byte/dune__exe__Exit.{cmi,cmo,cmt}
      ocamlopt .exit.eobjs/native/dune__exe__Exit.{cmt,cmx,o}
      ocamlopt exit.exe
          exit out.txt

  $ cat >> dune <<EOF
  > (rule
  >  (alias f2)
  >  (action (with-accepted-exit-codes
  >           1
  >           (with-stdin-from input
  >            (chdir .
  >             (run ./exit.exe 1))))))
  > EOF

  $ echo "Hello, Dune!" > input
  $ dune build --display=short --root . @f2
          exit alias f2

  $ cat >> dune <<EOF
  > (rule
  >  (alias f3)
  >  (action (with-accepted-exit-codes
  >           0
  >           (setenv VAR myvar
  >            (chdir .
  >             (system "echo \$VAR"))))))
  > EOF

  $ dune build --display=short --root . @f3
            sh alias f3
  myvar

  $ cat >> dune <<EOF
  > (rule
  >  (alias f4)
  >  (action (with-accepted-exit-codes
  >           0
  >           (setenv VAR myvar
  >            (ignore-stdout
  >             (bash "echo \$VAR"))))))
  > EOF

  $ dune build --display=short --root . @f4
          bash alias f4

  $ cat >> dune <<EOF
  > (rule
  >  (alias f5)
  >  (action (with-accepted-exit-codes
  >           0
  >           (setenv VAR myvar
  >            (with-stdin-from input
  >             (chdir .
  >              (with-stdout-to out2.txt
  >               (run ./exit.exe 1))))))))
  > EOF
  $ echo "Hello, Dune!" > input
  $ dune build --display=short --root . @f5
          exit out2.txt (exit 1)
  (cd _build/default && ./exit.exe 1) < _build/default/input > _build/default/out2.txt
  [1]

  $ cat >> dune <<EOF
  > (rule
  >  (alias g)
  >  (action 
  >   (with-accepted-exit-codes 
  >    (not 0) 
  >    (setenv VAR myvar
  >     (chdir .
  >      (with-stdout-to out.txt
  >       (dynamic-run ./exit.exe 1)))))))
  > EOF

  $ dune build --display=short --root . @g
  File "dune", line 46, characters 3-98:
  46 |    (setenv VAR myvar
  47 |     (chdir .
  48 |      (with-stdout-to out.txt
  49 |       (dynamic-run ./exit.exe 1)))))))
  Error: Only "run", "bash", "system", "chdir", "setenv", "ignore-<outputs>",
  "with-stdin-from", "with-<outputs>-to" and "no-infer" can be nested under
  "with-accepted-exit-codes"
  [1]
