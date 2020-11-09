Unreleased
----------

* Add the `[:alpha:]` character class in `Re.Perl` (#169)
* Double asterisk (`**`) in `Re.Glob` (#172)
  Like `*` but also match `/` characters when `pathname` is set.

1.9.0 (05-Apr-2019)
-------------------

* Fix regression in `Re.exec_partial` (#164)
* Mov gen related functions to `Re.Gen` and deprecate the old names (#167)
* Introduce `Re.View` that exposes the internal representation (#163)

1.8.0 (04-Aug-2018)
-------------------

* Fix index-out-of-bounds exception in Re.Perl.re (#160)
* Add seq based iterators (#170)

1.7.3 (05-Mar-2018)
-------------------

* Remove dependency on bytes package (#155)

1.7.2 (01-Mar-2018)
-------------------

* Deprecate all Re_* modules. Re_x is now available as Re.X
* Deprecate all re.x sub libraries. Those are all available as Re.X
* Make all function in Re.Str tail recursive.

1.7.1 (19-Oct-2016)
-------------------

* Fix Re_str.global_replace (#132)

1.7.0 (18-Sep-2016)
-------------------

* Fix stack overflow in Re_str.full_split
* Use correct exceptions in Re_str group functions
* Add experimental Re.witness
* Add experimental Re.Group.nb_groups

1.6.1 (20-Jun-2016)
-------------------

* Fix Re.pp (#101)
* Add Re.Group.pp (#102)

1.6.0 (30-May-2016)
-------------------

* Add Re.pp and Re.pp_re (#55)
* Fix ocamldoc syntax (#87)

1.5.0 (04-Jan-2016)
-------------------

* Add Re.exec_opt. Like exec but doesn't raise
* Add Group module. Old group accessors are deprecated.
* Add Mark module
* Improve docs of Re.repn
* Improve docs of Re_pcre
* Fix doc of Re_pcre.match
* Consolidate variants of Re.glob that takes options to modify its behavior
  (?period, ?expand_braces). Old variants are deprecated.
* New option ?pathname added for Re_glob.glob. Controls how the `/` character
  is matched

1.4.1 (06-Jun-2015)
-------------------

* Fix 4.00.1 compatibilty with tests.

1.4.0 (12-May-2015)
-------------------

* Add Re.{mark,marked,mark_set}. Regexps can now be "marked" to query post
  execution if they matched.

1.3.2 (14-Apr-2015)
-------------------

* Fix replacing 0 length matches (#55)

1.3.1 (13-Mar-2015)
-------------------

* Rename {Cset, Automata} to {Re_cset, Re_automata}

1.3.0 (02-Feb-2015)
-------------------

* Add Re.split{,_gen,_token,_full,_full_gen}
* Add Re.replace{,_string}
* Add Re.all{,_gen}
* Add posix classes of the form [:xxx:]
* Add complement suport for posix classes
* Add Multiline and anchored flag to Re_pcre
* Add Re_pcre.full_split

1.2.2 (05-May-2014)
-------------------

* Add a Re.whole_string convenience function to only match whole strings
* Add a ?anchored parameter to functions in Re_glob to specify whole
  string matching
* Document Re_glob module
* Fix compilation of submatches occurring inside a Kleen star
* Fix word boundary matching
* Fix definition of Re.xdigit
* Fix Re.exec_partial function
* Fix compilation of patterns of the shape r1r2|r1r3
* Fixed compilation of re.cmxs (Vincent Bernardoff)
* Improved matching of anchored regular expressions: stop as soon as
  we know there cannot possibly be any match.
* Updated to OASIS 0.4.x (Vincent Bernardoff)
* Add the linking exception to the license

1.2.1 (07-Apr-2013)
-------------------

* Correct OASIS metadata (Christophe Troestler).
* Fix typo in Invalid_arg error message (Jeremy Yallop).

1.2.0 (15-Jan-2012)
-------------------

* Rename Pcre module to `Re_pcre` to make it more suitable for
  upstream packaging (it currently conflicts with the `Pcre` package).
  (Mehdi Dogguy).

1.1.0 (05-Sep-2012)
-------------------

* Add a basic Pcre wrapper around Re_perl for porting applications using that
  API (Thomas Gazagnaire).

1.0.0 (01-Aug-2012)
-------------------

* Initial public release.
