

- Fix default value rendering in manpages. These were not properly
  escaped. Thanks to David Allsopp for the patch.

v1.0.4 2019-06-14 Zagreb
------------------------

- Change the way `Error (_, e)` term evaluation results 
  are formatted. Instead of treating `e` as text, treat
  it as formatted lines.
- Fix 4.08 `Pervasives` deprecation.
- Fix 4.03 String deprecations.
- Fix bootstrap build in absence of dynlink.
- Make the `Makefile` bootstrap build reproducible.
  Thanks to Thomas Leonard for the patch.

v1.0.3 2018-11-26 Zagreb
------------------------

- Add `Term.with_used_args`. Thanks to Jeremie Dimino for
  the patch.
- Use `Makefile` bootstrap build in opam file.
- Drop ocamlbuild requirement for `Makefile` bootstrap build.
- Drop support for ocaml < 4.03.0
- Dune build support.

v1.0.2 2017-08-07 Zagreb
------------------------

- Don't remove the `Makefile` from the distribution.

v1.0.1 2017-08-03 Zagreb
------------------------

- Add a `Makefile` to build and install cmdliner without `topkg` and
  opam `.install` files. Helps bootstraping opam in OS package
  managers. Thanks to Hendrik Tews for the patches.

v1.0.0 2017-03-02 La Forclaz (VS)
---------------------------------

**IMPORTANT** The `Arg.converter` type is deprecated in favor of the
`Arg.conv` type. For this release both types are equal but the next
major release will drop the former and make the latter abstract. All
users are kindly requested to migrate to use the new type and **only**
via the new `Arg.[p]conv` and `Arg.conv_{parser,printer}` functions.

- Allow terms to be used more than once in terms without tripping out
  documentation generation (#77). Thanks to François Bobot and Gabriel
  Radanne.
- Disallow defining the same option (resp. command) name twice via two
  different arguments (resp. terms). Raises Invalid_argument, used
  to be undefined behaviour (in practice, an arbitrary one would be
  ignored).
- Improve converter API (see important message above).
- Add `Term.exit[_status]` and `Term.exit_status_of[_status]_result`.
  improves composition with `Pervasives.exit`.
- Add `Term.term_result` and `Term.cli_parse_result` improves composition
  with terms evaluating to `result` types.
- Add `Arg.parser_of_kind_of_string`.
- Change semantics of `Arg.pos_left` (see #76 for details).
- Deprecate `Term.man_format` in favor of `Arg.man_format`.
- Reserve the `--cmdliner` option for library use. This is unused for now
  but will be in the future.
- Relicense from BSD3 to ISC.
- Safe-string support.
- Build depend on topkg.

### End-user visible changes

The following changes affect the end-user behaviour of all binaries using
cmdliner.

- Required positional arguments. All missing required position
  arguments are now reported to the end-user, in the correct
  order (#39). Thanks to Dmitrii Kashin for the report.
- Optional arguments. All unknown and ambiguous optional argument
  arguments are now reported to the end-user (instead of only
  the first one).
- Change default behaviour of `--help[=FMT]` option. `FMT` no longer
  defaults to `pager` if unspecified.  It defaults to the new value
  `auto` which prints the help as `pager` or `plain` whenever the
  `TERM` environment variable is `dumb` or undefined (#43). At the API
  level this changes the signature of the type `Term.ret` and values
  `Term.ret`, `Term.man_format` (deprecated) and `Manpage.print` to add the
  new `` `Auto`` case to manual formats. These are now represented by the
  `Manpage.format` type rather than inlined polyvars.

### Doc specification improvements and fixes

- Add `?envs` optional argument to `Term.info`. Documents environment
  variables that influence a term's evaluation and automatically
  integrate them in the manual.
- Add `?exits` optional argument to `Term.info`. Documents exit statuses of
  the program. Use `Term.default_exits` if you are using the new `Term.exit`
  functions.
- Add `?man_xrefs` optional argument to `Term.info`. Documents
  references to other manpages. Automatically formats a `SEE ALSO` section
  in the manual.
- Add `Manpage.escape` to escape a string from the documentation markup
  language.
- Add `Manpage.s_*` constants for standard man page section names.
- Add a `` `Blocks`` case to `Manpage.blocks` to allow block splicing
  (#69).  This avoids having to concatenate block lists at the
  toplevel of your program.
- `Arg.env_var`, change default environment variable section to the
   standard `ENVIRONMENT` manual section rather than `ENVIRONMENT
   VARIABLES`.  If you previously manually positioned that section in
   your man page you will have to change the name. See also next point.
- Fix automatic placement of default environment variable section (#44)
  whenever unspecified in the man page.
- Better automatic insertions of man page sections (#73). See the API
  docs about manual specification. As a side effect the `NAME` section
  can now also be overriden manually.
- Fix repeated environment variable printing for flags (#64). Thanks to
  Thomas Gazagnaire for the report.
- Fix rendering of env vars in man pages, bold is standard (#71).
- Fix plain help formatting for commands with empty
  description. Thanks to Maciek Starzyk for the patch.
- Fix (implement really) groff man page escaping (#48).
- Request `an` macros directly in the man page via `.mso` this
  makes man pages self-describing and avoids having to call `groff` with
  the `-man` option.
- Document required optional arguments as such (#82). Thanks to Isaac Hodes
  for the report.

### Doc language sanitization

This release tries to bring sanity to the doc language. This may break
the rendering of some of your man pages. Thanks to Gabriel Scherer,
Ivan Gotovchits and Nicolás Ojeda Bär for the feedback.

- It is only allowed to use the variables `$(var)` that are mentioned in
  the docs (`$(docv)`, `$(opt)`, etc.) and the markup directives
  `$({i,b},text)`. Any other unknown `$(var)` will generate errors
  on standard error during documentation generation.
- Markup directives `$({i,b},text)` treat `text` as is, modulo escapes;
  see next point.
- Characters `$`, `(`, `)` and `\` can respectively be escaped by `\$`,
  `\(`, `\)` and `\\`. Escaping `$` and `\` is mandatory everywhere.
  Escaping `)` is mandatory only in markup directives. Escaping `(`
  is only here for your symmetric pleasure. Any other sequence of
  character starting with a `\` is an illegal sequence.
- Variables `$(mname)` and `$(tname)` are now marked up with bold when
  substituted. If you used to write `$(b,$(tname))` this will generate
  an error on standard output, since `$` is not escaped in the markup
  directive. Simply replace these by `$(tname)`.

v0.9.8 2015-10-11 Cambridge (UK)
--------------------------------

- Bring back support for OCaml 3.12.0
- Support for pre-formatted paragraphs in man pages. This adds a
  ```Pre`` case to the `Manpage.block` type which can break existing
  programs. Thanks to Guillaume Bury for suggesting and help.
- Support for environment variables. If an argument is absent from the
  command line, its value can be read and parsed from an environment
  variable. This adds an `env` optional argument to the `Arg.info`
  function which can break existing programs.
- Support for new variables in option documentation strings. `$(opt)`
  can be used to refer to the name of the option being documented and
  `$(env)` for the name of the option's the environment variable.
- Deprecate `Term.pure` in favor of `Term.const`.
- Man page generation. Keep undefined variables untouched. Previously
  a `$(undef)` would be turned into `undef`.
- Turn a few misterious and spurious `Not_found` exceptions into
  `Invalid_arg`. These can be triggered by client programming errors
  (e.g. an unclosed variable in a documentation string).
- Positional arguments. Invoke the printer on the default (absent)
  value only if needed. See Optional arguments in the release notes of
  v0.9.6.

v0.9.7 2015-02-06 La Forclaz (VS)
---------------------------------

- Build system, don't depend on `ocamlfind`. The package no longer
  depends on ocamlfind. Thanks to Louis Gesbert for the patch. 

v0.9.6 2014-11-18 La Forclaz (VS)
---------------------------------

- Optional arguments. Invoke the printer on the default (absent) value
  only if needed, i.e. if help is shown. Strictly speaking an
  interface breaking change – for example if the absent value was lazy
  it would be forced on each run. This is no longer the case.
- Parsed command line syntax: allow short flags to be specified
  together under a single dash, possibly ending with a short option.
  This allows to specify e.g. `tar -xvzf archive.tgz` or `tar
  -xvzfarchive.tgz`. Previously this resulted in an error, all the
  short flags had to be specified separately. Backward compatible in
  the sense that only more command lines are parsed. Thanks to Hugo
  Heuzard for the patch.
- End user error message improvements using heuristics and edit
  distance search in the optional argument and sub command name
  spaces. Thanks to Hugo Heuzard for the patch.
- Adds `Arg.doc_{quote,alts,alts_enum}`, documentation string
  helpers.
- Adds the `Term.eval_peek_opts` function for advanced usage scenarios.
- The function `Arg.enum` now raises `Invalid_argument` if the
  enumeration is empty.
- Improves help paging behaviour on Windows. Thanks to Romain Bardou
  for the help.


v0.9.5 2014-07-04 Cambridge (UK)
--------------------------------

- Add variance annotation to Term.t. Thanks to Peter Zotov for suggesting.
- Fix section name formatting in plain text output. Thanks to Mikhail
  Sobolev for reporting.


v0.9.4 2014-02-09 La Forclaz (VS)
---------------------------------

- Remove temporary files created for paged help. Thanks to Kaustuv Chaudhuri
  for the suggestion.
- Avoid linking against `Oo` (was used to get program uuid).
- Check the environment for `$MANPAGER` aswell. Thanks to Raphaël Proust
  for the patch.
- OPAM friendly workflow and drop OASIS support.


v0.9.3 2013-01-04 La Forclaz (VS)
---------------------------------

- Allow user specified `SYNOPSIS` sections.


v0.9.2 2012-08-05 Lausanne
--------------------------

- OASIS 0.3.0 support.


v0.9.1 2012-03-17 La Forclaz (VS)
---------------------------------

- OASIS support.
- Fixed broken `Arg.pos_right`.
- Variables `$(tname)` and `$(mname)` can be used in a term's man
  page to respectively refer to the term's name and the main term
  name.
- Support for custom variable substitution in `Manpage.print`.
- Adds `Term.man_format`, to facilitate the definition of help commands.
- Rewrote the examples with a better and consistent style.

Incompatible API changes:

- The signature of `Term.eval` and `Term.eval_choice` changed to make
  it more regular: the given term and its info must be tupled together
  even for the main term and the tuple order was swapped to make it
  consistent with the one used for arguments.


v0.9.0 2011-05-27 Lausanne
--------------------------

- First release.
