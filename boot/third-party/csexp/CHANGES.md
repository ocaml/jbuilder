# 1.3.2

- The project now builds with dune 1.11.0 and onward (#12, @voodoos)

# 1.3.1

- Fix compatibility with 4.02.3

# 1.3.0

- Add a "feed" API for parsing. This new API let the user feed
  characters one by one to the parser. It gives more control to the
  user and the handling of IO errors is simpler and more
  explicit. Finally, it allocates less (#9, @jeremiedimino)

- Fixes `input_opt`; it was could never return [None] (#9, fixes #7,
  @jeremiedimino)

- Fixes `parse_many`; it was returning s-expressions in the wrong
  order (#10, @rgrinberg)

# 1.2.3

- Fix `parse_string_many`; it used to fail on all inputs (#6, @rgrinberg)

# 1.2.2

- Fix compatibility with 4.02.3

# 1.2.1

- Remove inclusion of the `Result` module, which was accidentally
  added in a previous PR. (#3, @rgrinberg)

# 1.2.0

- Expose low level, monad agnostic parser. (#2, @mefyl)

# 1.1.0

- Add compatibility up-to OCaml 4.02.3 (with disabled tests). (#1, @voodoos)

# 1.0.0

- Initial release
