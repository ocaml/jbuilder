Description
===========

Re is a regular expression library for OCaml.
[![Build Status](https://travis-ci.org/ocaml/ocaml-re.svg?branch=master)](https://travis-ci.org/ocaml/ocaml-re)

Contact
=======

This library has been written by Jerome Vouillon
(Jerome.Vouillon@pps.univ-paris-diderot.fr).
It can be downloaded from <https://github.com/ocaml/ocaml-re>

Bug reports, suggestions and contributions are welcome.

Features
========

The following styles of regular expressions are supported:
- Perl-style regular expressions (module `Re.Perl`);
- Posix extended regular expressions (module `Re.Posix`);
- Emacs-style regular expressions (module `Re.Emacs`);
- Shell-style file globbing (module `Re.Glob`).

It is also possible to build regular expressions by combining simpler regular
expressions (module `Re`).

The most notable missing features are **back-references** and
look-ahead/look-behind **assertions**.

There is also a subset of the PCRE interface available in the `Re.Pcre` module.
This makes it easier to port code from that library to Re minimal changes.

Performances
============

The matches are performed by lazily building a DFA (deterministic
finite automaton) from the regular expression. As a consequence,
matching takes linear time in the length of the matched string.

The compilation of patterns is slower than with libraries using
back-tracking, such as PCRE.  But, once a large enough part of the
DFA is built, matching is extremely fast.

Of course, for some combinations of regular expression and string, the
part of the DFA that needs to be build is so large that this point is
never reached, and matching will be slow.  This is not expected to
happen often in practice, and actually a lot of expressions that
behaves badly with a backtracking implementation are very efficient
with this implementation.

The library is at the moment entirely written in OCaml.  As a
consequence, regular expression matching is much slower when the
library is compiled to bytecode than when it is compiled to native
code.

Here are some timing results (Pentium III 500Mhz):
* Scanning a 1Mb string containing only `a`s, except for the last
  character which is a `b`, searching for the pattern `aa?b`
  (repeated 100 times):
    - RE: 2.6s
    - PCRE: 68s
* Regular expression example from http://www.bagley.org/~doug/shootout/ [1]
    - RE: 0.43s
    - PCRE: 3.68s

  [1] this page is no longer up but is available via the Internet Archive 
  http://web.archive.org/web/20010429190941/http://www.bagley.org/~doug/shootout/bench/regexmatch/

* The large regular expression (about 2000 characters long) that
  Unison uses with my preference file to decide whether a file should
  be ignored or not.  This expression is matched against a filename
  about 20000 times.
    - RE: 0.31s
    - PCRE: 3.7s
  However, RE is only faster than PCRE when there are more than about
  300 filenames.
