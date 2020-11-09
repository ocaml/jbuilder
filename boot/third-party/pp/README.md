Pp - Pretty printing
====================

This library provides a lean alternative to the [Format][format]
module of the OCaml standard library. It aims to make it easy for
users to do the right thing. If you have tried `Format` before but
find its API complicated and difficult to use, then `Pp` might be a
good choice for you.

`Pp` uses the same concepts of boxes and break hints, and the final
rendering is done to formatter from the `Format` module. However it
defines its own algebra which I personaly find easier to work with and
reason about. No previous knowledge is required to start using this
library, however the various guides for the `Format` module such as
[this one][format-guide] should be applicable to `Pp` as well.

Examples
--------

```ocaml
# #require "pp";;
# let print pp = Format.printf "%a@." Pp.to_fmt pp;;
val print : 'a Pp.t -> unit = <fun>
# print (Pp.enumerate (List.init 10 Fun.id) ~f:(Pp.textf "%d"));;
- 0
- 1
- 2
- 3
- 4
- 5
- 6
- 7
- 8
- 9
# print (Pp.box ~indent:2 (Pp.text
         "Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed \
          do eiusmod tempor incididunt ut labore et dolore magna \
          aliqua. Ut enim ad minim veniam, quis nostrud exercitation \
          ullamco laboris nisi ut aliquip ex ea commodo \
          consequat. Duis aute irure dolor in reprehenderit in \
          voluptate velit esse cillum dolore eu fugiat nulla \
          pariatur. Excepteur sint occaecat cupidatat non proident, \
          sunt in culpa qui officia deserunt mollit anim id est \
          laborum."));;
Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod tempor
  incididunt ut labore et dolore magna aliqua. Ut enim ad minim veniam, quis
  nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat.
  Duis aute irure dolor in reprehenderit in voluptate velit esse cillum dolore
  eu fugiat nulla pariatur. Excepteur sint occaecat cupidatat non proident,
  sunt in culpa qui officia deserunt mollit anim id est laborum.
- : unit = ()
# print
    (Pp.vbox
       ( Pp.box (Pp.text "Error: something went wrong!")
       ++ Pp.cut
       ++ Pp.box (Pp.text "Here are a few things you can do:")
       ++ Pp.cut
       ++ Pp.enumerate ~f:Fun.id
            [ Pp.text
                "read the documentation, double check the way you are using \
                 this software to make sure you are not doing something wrong, \
                 and hopefully fix the problem on your side and move on"
            ; Pp.text
                "strace furiously the program to try and understand why \
                 exactly it is trying to do what it is doing"
            ; Pp.text "report an issue upstream"
            ; Pp.text "if all else fails"
              ++ Pp.cut
              ++ Pp.enumerate ~f:Pp.text
                   [ "scream loudly at your computer"
                   ; "take a break from your keyboard"
                   ; "clear your head and try again"
                   ]
            ] ));;
Error: something went wrong!
Here are a few things you can do:
- read the documentation, double check the way you are using this software to
  make sure you are not doing something wrong, and hopefully fix the problem on
  your side and move on
- strace furiously the program to try and understand why exactly it is trying
  to do what it is doing
- report an issue upstream
- if all else fails
  - scream loudly at your computer
  - take a break from your keyboard
  - clear your head and try again
- : unit = ()
```

Resources
---------

As mentioned earlier, [this Format guide][format-guide] can be a good
starting point to understand the pretty-printing mechanics of `Pp`.
Additionally, [Format Unraveled][format-unraveled] is a great resource
for understanding the core mental model of `Format`. And since `Pp`
uses the same concepts as `Format`, it can be a good resource for `Pp`
too.

Note that the Format Unraveled paper discuss some limitations of
`Format` that are due to the fact that it never has the full document
in-memory before rendering it. This does not apply to `Pp` since `Pp`
clearly always construct the full document in-memory. However, since
right now the only way to render a `Pp.t` is via the `Format` module,
the same limitations that apply to `Format` apply to `Pp` as well. We
might add another renderer in the future that does not have these
limitations if there is sufficient incentive to do so.

History
-------

This library comes from the [dune build system][dune]. Initially, to
construct the various messages displayed to the user on the terminal,
dune was mostly using the `Format` module, and in particular the
`Format.fprintf` style format strings. The `Format` API, its concepts
and printf-like format strings are quite complicated and not easy to
grasp at all.  It requires quite a bit of learning and practice before
one can be fluent with `Format`.

What is more, it is well known that programmers absolutely "love"
spending time writing good error messages. Hint: this is sarcastic.

The result of all this was terrible and most messages printed by DUne
where badly formatted. So to remedy to the situation we introduced a
`Pp` module in `stdune`, the mini-standard library inside Dune. `Pp`
is completely detached from `Format`, and there is no mention of
`formatter` until the rendering stage. While in the `Pp` world, all we
do is construct a document with various formatting hints.

In the end, the API of `Pp` just makes it easy for someone to do the
right thing. And this makes all the difference. Since then, it has
been easy to construct well formatted error messages for Dune and the
formatting of existing error messages has generally improved.

Once `Pp` was mature enough, we extracted it into its own library so
that it can benefit others.

Interoperability
----------------

It is easy to integrate `Pp` with `Format`. For that, simply use the
`Pp.to_fmt` function. For instance, if you have a value `pp` of type
`_ Pp.t` you can do:

```ocaml
Format.fprintf "... %a ..." Pp.to_fmt pp
```

If you are familiar with the [fmt library][fmt], `Pp.to_fmt` basically
allows you to go from a `'a Pp.t` to a `'a Fmt.t`. The opposite is not
possible; it is not possible to inject arbitrary side-effecting
formatting functions into a `Pp.t`.

If you want to convert `Pp` tags fot `Format` tags, you can use the
function `Pp.to_fmt_with_tags`.

Comparison with other libraries
-------------------------------

This is not an in-depth comparison as I haven't used these libraries
much myself, so this is to be taken with a grain of salt. The below is
basically what I can tell from a quick look at their API. If you know
more and would like to contribute to this comparison, please do so by
opening a PR :)

### Comparison with fmt

The main difference with [fmt][fmt] is that `Fmt.t` is am alias for
`Format.formatter -> 'a -> unit`, while `Pp.t` is an abstract type.

### Comparison with easy-format

The [easy-format library][easy-format] looks much higher-level than
`Pp`. `Pp` still works with boxes and break hints like the `Format`
module, while `easy-format` works with atoms, lists and labelled
nodes.

[format]: https://caml.inria.fr/pub/docs/manual-ocaml/libref/Format.html
[format-unraveled]: https://hal.archives-ouvertes.fr/hal-01503081/file/format-unraveled.pdf
[dune]: https://dune.build
[fmt]: https://erratique.ch/software/fmt
[format-guide]: http://caml.inria.fr/resources/doc/guides/format.en.html
[easy-format]: https://github.com/mjambon/easy-format
