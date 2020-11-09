
open Cmdliner

let hey =
  let doc = "Equivalent to set $(opt)." in
  let env = Arg.env_var "TEST_ENV" ~doc in
  let doc = "Set hey." in
  Arg.(value & flag & info ["hey"; "y"] ~env ~doc)

let repodir =
  let doc = "See option $(opt)." in
  let env = Arg.env_var "TEST_REPODDIR" ~doc in
  let doc = "Run the program in repository directory $(docv)." in
  Arg.(value & opt file Filename.current_dir_name & info ["repodir"] ~env
         ~docv:"DIR" ~doc)

let id =
  let doc = "See option $(opt)." in
  let env = Arg.env_var "TEST_ID" ~doc in
  let doc = "Whatever $(docv) bla $(env) and $(opt)." in
  Arg.(value & opt int ~vopt:10 0 & info ["id"; "i"] ~env ~docv:"ID)" ~doc)

let miaouw =
  let doc = "See option $(opt). These are term names $(mname) $(tname)" in
  let docs = "MIAOUW SECTION (non-standard unpositioned do not do this)" in
  let env = Arg.env_var "TEST_MIAOUW" ~doc ~docs in
  let doc = "Whatever this is the doc var $(docv) this is the env var $(env) \
             this is the opt $(opt) and this is $(i,italic) and this is
             $(b,bold) and this $(b,\\$(opt\\)) is \\$(opt) in bold and this
             \\$ is a dollar. $(mname) is the main term name, $(tname) is the
             term name."
  in
  Arg.(value & opt string "miaouw" & info ["m";] ~env ~docv:"MIAOUW" ~doc)

let test hey repodir id miaouw =
  Format.printf "hey: %B@.repodir: %s@.id: %d@.miaouw: %s@."
    hey repodir id miaouw

let man_test_t = Term.(const test $ hey $ repodir $ id $ miaouw)

let info =
  let doc = "print a customizable message repeatedly" in
  let envs = [ Term.env_info "TEST_IT" ~doc:"This is $(env) for $(tname)" ] in
  let exits = [ Term.exit_info ~doc:"This is a $(status) for $(tname)" 1;
                Term.exit_info ~doc:"Ranges from $(status) to $(status_max)"
                  ~max:10 2; ] @ Term.default_exits
  in
  let man = [
    `S "THIS IS A SECTION FOR $(mname)";
    `P "$(mname) subst at begin and end $(mname)";
    `P "$(i,italic) and $(b,bold)";
    `P "\\$ escaped \\$\\$ escaped \\$";
    `P "This does not fail \\$(a)";
    `P ". this is a paragraph starting with a dot.";
    `P "' this is a paragraph starting with a quote.";
    `P "This: \\\\(rs is a backslash for groff and you should not see a \\\\";
    `P "This: \\\\N'46' is a quote for groff and you should not see a '";
    `P "This: \\\\\"  is a groff comment and it should not be one.";
    `P "This is a non preformatted paragraph, filling will occur. This will
        be properly layout on 80 columns.";
    `Pre "This is a preformatted paragraph for $(mname) no filling will \
          occur do the $(i,ASCII) art $(b,here) this will overflow on 80 \
          columns \n\
          01234556789\
          01234556789\
          01234556789\
          01234556789\
          01234556789\
          01234556789\
          01234556789\
          01234556789\n\n\
          ... Should not break\n\
          a... Should not break\n\
          +---+\n\
          |  /|\n\
          | / | ----> Let's swim to the moon.\n\
          |/  |\n\
          +---+";
    `P "These are escapes escaped \\$ \\( \\) \\\\";
    `P "() does not need to be escaped outside directives.";
    `Blocks [
      `P "The following to paragraphs are spliced in.";
      `P "This dollar needs escape \\$(var) this one aswell $(b,\\$(bla\\))";
      `P "This is another paragraph \\$(bla) $(i,\\$(bla\\)) $(b,\\$\\(bla\\))";
    ];
    `Noblank;
    `Pre "This is another preformatted paragraph.\n\
          There should be no blanks before and after it.";
    `Noblank;
    `P "Hey ho";
    `I ("label", "item label");
    `I ("lebal", "item lebal");
    `P "The last paragraph";
    `S Manpage.s_bugs;
    `P "Email bug reports to <hehey at example.org>.";]
  in
  let man_xrefs = [`Page ("ascii", 7); `Main; `Tool "grep";] in
  Term.info "man_test" ~version:"%%VERSION%%" ~doc ~envs ~exits ~man ~man_xrefs

let () = Term.exit @@ Term.eval (man_test_t, info)
