****************
General concepts
****************

.. _scopes:

Scopes
======

Any directory containing at least one ``<package>.opam`` file defines
a scope. This scope is the sub-tree starting from this directory,
excluding any other scopes rooted in sub-directories.

Typically, any given project will define a single scope. Libraries and
executables that are not meant to be installed will be visible inside
this scope only.

Because scopes are exclusive, if you wish to include the dependencies
of the project you are currently working on into your workspace, you
may copy them in a ``vendor`` directory, or any other name of your
choice. Dune will look for them there rather than in the installed
world and there will be no overlap between the various scopes.

.. _ordered-set-language:

Ordered set language
====================

A few fields take as argument an ordered set and can be specified using a small
DSL.

This DSL is interpreted by dune into an ordered set of strings using the
following rules:

- ``:standard`` denotes the standard value of the field when it is absent
- an atom not starting with a ``:`` is a singleton containing only this atom
- a list of sets is the concatenation of its inner sets
- ``(<sets1> \ <sets2>)`` is the set composed of elements of ``<sets1>`` that do
  not appear in ``<sets2>``

In addition, some fields support the inclusion of an external file using the
syntax ``(:include <filename>)``. This is useful for instance when you need to
run a script to figure out some compilation flags. ``<filename>`` is expected to
contain a single S-expression and cannot contain ``(:include ...)`` forms.

Note that inside an ordered set, the first element of a list cannot be
an atom except if it starts with `-` or `:`. The reason for this is
that we are planning to add simple programmatic features in the
futures so that one may write:

.. code::

   (flags (if (>= %{ocaml_version} 4.06) ...))

This restriction will allow to add this feature without introducing a
breaking changes. If you want to write a list where the first element
doesn't start by `-`, you can simply quote it: ``("x" y z)``.

Most fields using the ordered set language also support :ref:`variables`.
Variables are expanded after the set language is interpreted.

.. _blang:

Boolean language
================

The boolean language allows the user to define simple boolean expressions that
dune can evaluate. Here's a semi formal specification of the language:

.. code::

   op := '=' | '<' | '>' | '<>' | '>=' | '<='

   expr := (and <expr>+)
         | (or <expr>+)
         | (<op> <template> <template>)
         | <template>

After an expression is evaluated, it must be exactly the string ``true`` or
``false`` to be considered as a boolean. Any other value will be treated as an
error.

Here's a simple example of a condition that expresses running on OSX and having
an flambda compiler with the help of variable expansion:

.. code:: lisp

   (and %{ocamlc-config:flambda} (= %{ocamlc-config:system} macosx))

.. _predicate-lang:

Predicate language
==================

The predicate language allows the user to define simple predicates
(boolean-valued functions) that dune can evaluate. Here is a semi formal
specification of the language:

.. code::

   pred := (and <pred> <pred>)
         | (or <pred> <pred>)
         | (not <pred>)
         | :standard
         | <element>

The exact meaning of ``:standard`` and the nature of ``<element>`` depends on
the context. For example, in the case of the :ref:`dune-subdirs`, an
``<element>`` corresponds to file glob patterns. Another example is the user
action :ref:`(with-accepted-exit-codes ...) <user-actions>`, where an ``<element>``
corresponds to a literal integer.

.. _variables:

Variables
=========

Some fields can contains variables that are expanded by dune.
The syntax of variables is as follows:

.. code::

   %{var}

or, for more complex forms that take an argument:

.. code::

   %{fun:arg}

In order to write a plain ``%{``, you need to write ``\%{`` in a
string.

Dune supports the following variables:

- ``project_root`` is the root of the current project. It is typically the root
  of your project and as long as you have a ``dune-project`` file there,
  ``project_root`` is independent of the workspace configuration
- ``workspace_root`` is the root of the current workspace. Note that
  the value of ``workspace_root`` is not constant and depends on
  whether your project is vendored or not
-  ``CC`` is the C compiler command line (list made of the compiler
   name followed by its flags) that will be used to compile foreign code. For more details about its content see :ref:`this section <flags-flow>`.
-  ``CXX`` is the C++ compiler command line being used in the
   current build context
-  ``ocaml_bin`` is the path where ``ocamlc`` lives
-  ``ocaml`` is the ``ocaml`` binary
-  ``ocamlc`` is the ``ocamlc`` binary
-  ``ocamlopt`` is the ``ocamlopt`` binary
-  ``ocaml_version`` is the version of the compiler used in the
   current build context
-  ``ocaml_where`` is the output of ``ocamlc -where``
-  ``arch_sixtyfour`` is ``true`` if using a compiler targeting a
   64 bit architecture and ``false`` otherwise
-  ``null`` is ``/dev/null`` on Unix or ``nul`` on Windows
-  ``ext_obj``, ``ext_asm``, ``ext_lib``, ``ext_dll`` and ``ext_exe``
   are the file extension used for various artifacts
- ``ext_plugin`` is ``.cmxs`` if ``natdynlink`` is supported and
  ``.cma`` otherwise.
- ``ocaml-config:v`` for every variable ``v`` in the output of
  ``ocamlc -config``. Note that dune processes the output
  of ``ocamlc -config`` in order to make it a bit more stable across
  versions, so the exact set of variables accessible this way might
  not be exactly the same as what you can see in the output of
  ``ocamlc -config``. In particular, variables added in new versions
  of OCaml needs to be registered in dune before they can be used
- ``profile`` the profile selected via ``--profile``
- ``context_name`` the name of the context (``default`` or defined in the
  workspace file)
- ``os_type`` is the type of the OS the build is targeting. This is
  the same as ``ocaml-config:os_type``
- ``architecture`` is the type of the architecture the build is targeting. This
  is the same as ``ocaml-config:architecture``
- ``model`` is the type of the CPU the build is targeting. This is
  the same as ``ocaml-config:model``
- ``system`` is the name of the OS the build is targeting. This is the same as
  ``ocaml-config:system``
- ``ignoring_promoted_rule`` is ``true`` if
  ``--ignore-promoted-rules`` was passed on the command line and
  ``false`` otherwise
- ``<ext>:<path>`` where ``<ext>`` is one of ``cmo``, ``cmi``, ``cma``,
  ``cmx``, or ``cmxa``. See :ref:`variables-for-artifacts`.

In addition, ``(action ...)`` fields support the following special variables:

- ``target`` expands to the one target
- ``targets`` expands to the list of target
- ``deps`` expands to the list of dependencies
- ``^`` expands to the list of dependencies, separated by spaces
- ``dep:<path>`` expands to ``<path>`` (and adds ``<path>`` as a dependency of
  the action)
- ``exe:<path>`` is the same as ``<path>``, except when cross-compiling, in
  which case it will expand to ``<path>`` from the host build context
- ``bin:<program>`` expands to a path to ``program``. If ``program``
  is built by a package in the workspace (see :ref:`install`, :ref:`env` and
  :ref:`executable` stanzas),
  the locally built binary will be used, otherwise it will be searched
  in the ``PATH`` of the current build context. Note that ``(run
  %{bin:program} ...)`` and ``(run program ...)`` behave in the same
  way. ``%{bin:...}`` is only necessary when you are using ``(bash
  ...)`` or ``(system ...)``
- ``lib:<public-library-name>:<file>`` expands to the installation path of
  the file ``<file>`` in the library ``<public-library-name>``. If
  ``<public-library-name>`` is available in the current workspace, the local
  file will be used, otherwise the one from the installed world will be used.
- ``lib-private:<library-name>:<file>`` expands to the build path of the file
  ``<file>`` in the library ``<library-name>``. Both public and private library
  names are allowed as long as they refer to libraries within the same project.
- ``libexec:<public-library-name>:<file>`` is the same as ``lib:...`` except
  when cross-compiling, in which case it will expand to the file from the host
  build context.
- ``libexec-private:<library-name>:<file>`` is the same as ``lib-private:...``
  except when cross-compiling, in which case it will expand to the file from the
  host build context.
- ``lib-available:<library-name>`` expands to ``true`` or ``false`` depending on
  whether the library is available or not. A library is available iff at least
  one of the following conditions holds:

  -  it is part the installed worlds
  -  it is available locally and is not optional
  -  it is available locally and all its library dependencies are
     available

- ``version:<package>`` expands to the version of the given
  package. Note that this is only supported for packages that are
  being defined in the current scope. How dune determines the version
  of a package is described :ref:`here <package-version>`
- ``read:<path>`` expands to the contents of the given file
- ``read-lines:<path>`` expands to the list of lines in the given
  file
- ``read-strings:<path>`` expands to the list of lines in the given
  file, unescaped using OCaml lexical convention

The ``%{<kind>:...}`` forms are what allows you to write custom rules that work
transparently whether things are installed or not.

Note that aliases are ignored by ``%{deps}``

The intent of this last form is to reliably read a list of strings
generated by an OCaml program via:

.. code:: ocaml

    List.iter (fun s -> print_string (String.escaped s)) l

#. Expansion of lists

Forms that expands to list of items, such as ``%{cc}``, ``%{deps}``,
``%{targets}`` or ``%{read-lines:...}``, are suitable to be used in, say,
``(run <prog> <arguments>)``.  For instance in:

.. code:: lisp

    (run foo %{deps})

if there are two dependencies ``a`` and ``b``, the produced command
will be equivalent to the shell command:

.. code:: shell

    $ foo "a" "b"

If you want the two dependencies to be passed as a single argument,
you have to quote the variable as in:

.. code:: scheme

    (run foo "%{deps}")

which is equivalent to the following shell command:

.. code:: shell

    $ foo "a b"

(the items of the list are concatenated with space).
Note that, since ``%{deps}`` is a list of items, the first one may be
used as a program name, for instance:

.. code:: lisp

    (rule
     (targets result.txt)
     (deps    foo.exe (glob_files *.txt))
     (action  (run %{deps})))

Here is another example:

.. code:: lisp

    (rule
     (target foo.exe)
     (deps   foo.c)
     (action (run %{cc} -o %{target} %{deps} -lfoolib)))

.. _library-deps:

Library dependencies
====================

Dependencies on libraries are specified using ``(libraries ...)`` fields in
``library`` and ``executables`` stanzas.

For libraries defined in the current scope, you can use either the real name or
the public name. For libraries that are part of the installed world, or for
libraries that are part of the current workspace but in another scope, you need
to use the public name. For instance: ``(libraries base re)``.

When resolving libraries, libraries that are part of the workspace are always
preferred to ones that are part of the installed world.

.. _alternative-deps:

Alternative dependencies
------------------------

In addition to direct dependencies you can specify alternative dependencies.
This is described in the :ref:`Alternative dependencies <alternative-deps>`
section

It is sometimes the case that one wants to not depend on a specific library, but
instead on whatever is already installed. For instance to use a different
backend depending on the target.

Dune allows this by using a ``(select ... from ...)`` form inside the list
of library dependencies.

Select forms are specified as follows:

.. code:: scheme

    (select <target-filename> from
     (<literals> -> <filename>)
     (<literals> -> <filename>)
     ...)

``<literals>`` are lists of literals, where each literal is one of:

- ``<library-name>``, which will evaluate to true if ``<library-name>`` is
  available, either in the workspace or in the installed world
- ``!<library-name>``, which will evaluate to true if ``<library-name>`` is not
  available in the workspace or in the installed world

When evaluating a select form, dune will create ``<target-filename>`` by
copying the file given by the first ``(<literals> -> <filename>)`` case where
all the literals evaluate to true. It is an error if none of the clauses are
selectable. You can add a fallback by adding a clause of the form ``(->
<file>)`` at the end of the list.

Re-exported dependencies
------------------------

A dependency ``foo`` may be marked as always *re-exported* using the
following syntax:

.. code:: scheme

   (re_export foo)

For instance:

.. code:: scheme

   (library
    (name bar)
    (libraries (re_export foo)))

This states that this library explicitly re-exports the interface of
``foo``.  Concretely, when something depends on ``bar`` it will also
be able to see ``foo`` independently of whether :ref:`implicit
transitive dependencies<implicit_transitive_deps>` are allowed or
not. When they are allowed, which is the default, all transitive
dependencies are visible whether they are marked as re-exported or
not.

.. _preprocessing-spec:

Preprocessing specification
===========================

Dune accepts three kinds of preprocessing:

- ``no_preprocessing``, meaning that files are given as it to the compiler, this
  is the default
- ``(action <action>)`` to preprocess files using the given action
- ``(pps <ppx-rewriters-and-flags>)`` to preprocess files using the given list
  of ppx rewriters
- ``(staged_pps <ppx-rewriters-and-flags>)`` is similar to ``(pps ...)``
  but behave slightly differently and is needed for certain ppx rewriters
  (see below for details)
- ``future_syntax`` is a special value that brings some of the newer
  OCaml syntaxes to older compilers. See :ref:`Future syntax
  <future-syntax>` for more details

Dune normally assumes that the compilation pipeline is sequenced as
follow:

- code generation (including preprocessing)
- dependency analysis
- compilation

Dune uses this fact to optimize the pipeline and in particular share
the result of code generation and preprocessing between the dependency
analysis and compilation phases. However, some specific code
generators or preprocessors require feedback from the compilation
phase. As a result they must be applied in stages as follows:

- first stage of code generation
- dependency analysis
- second step of code generation in parallel with compilation

This is the case for ppx rewriters using the OCaml typer for
instance. When using such ppx rewriters, you must use ``staged_pps``
instead of ``pps`` in order to force Dune to use the second pipeline,
which is slower but necessary in this case.

.. _preprocessing-actions:

Preprocessing with actions
--------------------------

``<action>`` uses the same DSL as described in the `User actions`_
section, and for the same reason given in that section, it will be
executed from the root of the current build context. It is expected to
be an action that reads the file given as only dependency named
``input-file`` and outputs the preprocessed file on its standard output.

More precisely, ``(preprocess (action <action>))`` acts as if
you had setup a rule for every file of the form:

   .. code:: lisp

       (rule
        (target file.pp.ml)
        (deps   file.ml)
        (action (with-stdout-to %{target}
                 (chdir %{workspace_root} <action>))))

The equivalent of a ``-pp <command>`` option passed to the OCaml compiler is
``(system "<command> %{input-file}")``.

Preprocessing with ppx rewriters
--------------------------------

``<ppx-rewriters-and-flags>`` is expected to be a sequence where each
element is either a command line flag if starting with a ``-`` or the
name of a library.  If you want to pass command line flags that do not
start with a ``-``, you can separate library names from flags using
``--``. So for instance from the following ``preprocess`` field:

   .. code:: scheme

       (preprocess (pps ppx1 -foo ppx2 -- -bar 42))

The list of libraries will be ``ppx1`` and ``ppx2`` and the command line
arguments will be: ``-foo -bar 42``.

Libraries listed here should be libraries implementing an OCaml AST rewriter and
registering themselves using the `ocaml-migrate-parsetree.driver API
<https://github.com/let-def/ocaml-migrate-parsetree>`__.

Dune will build a single executable by linking all these libraries and their
dependencies. Note that it is important that all these libraries are linked with
``-linkall``. Dune automatically uses ``-linkall`` when the ``(kind ...)``
field is set to ``ppx_rewriter`` or ``ppx_deriver``.

Per module preprocessing specification
--------------------------------------

By default a preprocessing specification will apply to all modules in the
library/set of executables. It is possible to select the preprocessing on a
module-by-module basis by using the following syntax:

 .. code:: scheme

    (preprocess (per_module
                 (<spec1> <module-list1>)
                 (<spec2> <module-list2>)
                 ...))

Where ``<spec1>``, ``<spec2>``, ... are preprocessing specifications
and ``<module-list1>``, ``<module-list2>``, ... are list of module
names.

For instance:

 .. code:: lisp

    (preprocess (per_module
                 (((action (run ./pp.sh X=1 %{input-file})) foo bar))
                 (((action (run ./pp.sh X=2 %{input-file})) baz))))

.. _future-syntax:

Future syntax
-------------

The ``future_syntax`` preprocessing specification is equivalent to
``no_preprocessing`` when using one of the most recent versions of the
compiler. When using an older one, it is a shim preprocessor that
backports some of the newer syntax elements. This allows you to use some of
the new OCaml features while keeping compatibility with older
compilers.

One example of supported syntax is the custom let-syntax that was
introduced in 4.08, allowing the user to define custom let operators.

Note that this feature is implemented by the third-party
`ocaml-syntax-shims project
<https://github.com/ocaml-ppx/ocaml-syntax-shims>`_, so if you use
this feature you must also declare a dependency on this package.

.. _preprocessor-deps:

Preprocessor dependencies
-------------------------

If your preprocessor needs extra dependencies you should use the
``preprocessor_deps`` field available in the ``library``, ``executable`` and
``executables`` stanzas.

.. _deps-field:

Dependency specification
========================

Dependencies in ``dune`` files can be specified using one of the following:

.. _source_tree:

- ``(:name <dependencies>)`` will bind the list of dependencies to the
  ``name`` variable. This variable will be available as ``%{name}`` in actions.
- ``(file <filename>)`` or simply ``<filename>``: depend on this file
- ``(alias <alias-name>)``: depend on the construction of this alias, for
  instance: ``(alias src/runtest)``
- ``(alias_rec <alias-name>)``: depend on the construction of this
  alias recursively in all children directories wherever it is
  defined. For instance: ``(alias_rec src/runtest)`` might depend on
  ``(alias src/runtest)``, ``(alias src/foo/bar/runtest)``, ...
- ``(glob_files <glob>)``: depend on all files matched by ``<glob>``, see the
  :ref:`glob <glob>` for details
- ``(source_tree <dir>)``: depend on all source files in the subtree with root
  ``<dir>``

- ``(universe)``: depend on everything in the universe. This is for
  cases where dependencies are too hard to specify. Note that dune
  will not be able to cache the result of actions that depend on the
  universe. In any case, this is only for dependencies in the
  installed world, you must still specify all dependencies that come
  from the workspace.
- ``(package <pkg>)`` depend on all files installed by ``<package>``, as well
  as on the transitive package dependencies of ``<package>``. This can be used
  to test a command against the files that will be installed
- ``(env_var <var>)``: depend on the value of the environment variable ``<var>``.
  If this variable becomes set, becomes unset, or changes value, the target
  will be rebuilt.
- ``(sandbox <config>)``: require a particular sandboxing configuration.
  ``<config>`` can be one (or many) of:

  - ``always``: the action requires a clean environment.
  - ``none``: the action must run in the build directory.
  - ``preserve_file_kind``: the action needs the files it reads to look
    like normal files (so dune won't use symlinks for sandboxing)

In all these cases, the argument supports :ref:`variables`.

Named Dependencies
------------------

dune allows a user to organize dependency lists by naming them. The user is
allowed to assign a group of dependencies a name that can later be referred to
in actions (like the ``%{deps}``, ``%{target}`` and ``%{targets}`` built in variables).

One instance where this is useful is for naming globs. Here's an
example of an imaginary bundle command:

.. code:: lisp

   (rule
    (target archive.tar)
    (deps
     index.html
     (:css (glob_files *.css))
     (:js foo.js bar.js)
     (:img (glob_files *.png) (glob_files *.jpg)))
    (action
     (run %{bin:bundle} index.html -css %{css} -js %{js} -img %{img} -o %{target})))

Note that such named dependency list can also include unnamed
dependencies (like ``index.html`` in the example above). Also, such
user defined names will shadow built in variables. So
``(:workspace_root x)`` will shadow the built in ``%{workspace_root}``
variable.

.. _glob:

Glob
----

You can use globs to declare dependencies on a set of files. Note that globs
will match files that exist in the source tree as well as buildable targets, so
for instance you can depend on ``*.cmi``.

Currently dune only supports globbing files in a single directory. And in
particular the glob is interpreted as follows:

- anything before the last ``/`` is taken as a literal path
- anything after the last ``/``, or everything if the glob contains no ``/``, is
  interpreted using the glob syntax

The glob syntax is interpreted as follows:

- ``\<char>`` matches exactly ``<char>``, even if it is a special character
  (``*``, ``?``, ...)
- ``*`` matches any sequence of characters, except if it comes first in which
  case it matches any character that is not ``.`` followed by anything
- ``**`` matches any character that is not ``.`` followed by anything, except if
  it comes first in which case it matches anything
- ``?`` matches any single character
- ``[<set>]`` matches any character that is part of ``<set>``
- ``[!<set>]`` matches any character that is not part of ``<set>``
- ``{<glob1>,<glob2>,...,<globn>}`` matches any string that is matched by one of
  ``<glob1>``, ``<glob2>``, ...

.. _ocaml-flags:

OCaml flags
===========

In ``library``, ``executable``, ``executables`` and ``env`` stanzas,
you can specify OCaml compilation flags using the following fields:

- ``(flags <flags>)`` to specify flags passed to both ``ocamlc`` and
  ``ocamlopt``
- ``(ocamlc_flags <flags>)`` to specify flags passed to ``ocamlc`` only
- ``(ocamlopt_flags <flags>)`` to specify flags passed to ``ocamlopt`` only

For all these fields, ``<flags>`` is specified in the `Ordered set language`_.
These fields all support ``(:include ...)`` forms.

The default value for ``(flags ...)`` is taken from the environment,
as a result it is recommended to write ``(flags ...)`` fields as
follows:

.. code:: scheme

    (flags (:standard <my options>))

.. _user-actions:

User actions
============

``(action ...)`` fields describe user actions.

User actions are always run from the same subdirectory of the current build
context as the dune file they are defined in. So for instance an action defined
in ``src/foo/dune`` will be run from ``$build/<context>/src/foo``.

The argument of ``(action ...)`` fields is a small DSL that is interpreted by
dune directly and doesn't require an external shell. All atoms in the DSL
support :ref:`variables`. Moreover, you don't need to specify dependencies
explicitly for the special ``%{<kind>:...}`` forms, these are recognized and
automatically handled by dune.

The DSL is currently quite limited, so if you want to do something complicated
it is recommended to write a small OCaml program and use the DSL to invoke it.
You can use `shexp <https://github.com/janestreet/shexp>`__ to write portable
scripts or :ref:`configurator` for configuration related tasks. You can also
use :ref:`dune-action-plugin` to express program dependencies directly in the
source code.

The following constructions are available:

- ``(run <prog> <args>)`` executes ``bin:<prog>`` with arguments ``<args>``.
  ``<prog>`` is resolved.
  ``<prog>`` is resolved locally if it is available in the current workspace, otherwise it is
  resolved using the ``PATH``. However, ``<prog>`` can only be resolved locally
  if it is declared to dune. See :ref:`variables` for discussion of how
  ``bin:<prog>`` is resolved and how it must be declared to dune.
- ``(dynamic-run <prog> <args>)`` to execute a program that was linked
  against ``dune-action-plugin`` library. ``<prog>`` is resolved in
  the same way as in ``run``
- ``(chdir <dir> <DSL>)`` to change the current directory
- ``(setenv <var> <value> <DSL>)`` to set an environment variable
- ``(with-<outputs>-to <file> <DSL>)`` to redirect the output to a file, where
  ``<outputs>`` is one of: ``stdout``, ``stderr`` or ``outputs`` (for both
  ``stdout`` and ``stderr``)
- ``(ignore-<outputs> <DSL>)`` to ignore the output, where
  ``<outputs>`` is one of: ``stdout``, ``stderr`` or ``outputs``
- ``(with-stdin-from <file> <DSL>)`` to redirect the input from a file
- ``(with-accepted-exit-codes <pred> <DSL>)`` specifies the list of expected exit codes
  for the programs executed in ``<DSL>``. ``<pred>`` is a predicate on integer
  values, and is specified using the :ref:`predicate-lang`. ``<DSL>`` can only
  contain nested occurrences of ``run``, ``bash``, ``system``, ``chdir``,
  ``setenv``, ``ignore-<outputs>``, ``with-stdin-from`` and
  ``with-<outputs>-to``. This action is available since dune 2.0.
- ``(progn <DSL>...)`` to execute several commands in sequence
- ``(echo <string>)`` to output a string on stdout
- ``(write-file <file> <string>)`` writes ``<string>`` to ``<file>``
- ``(cat <file>)`` to print the contents of a file to stdout
- ``(copy <src> <dst>)`` to copy a file
- ``(copy# <src> <dst>)`` to copy a file and add a line directive at
  the beginning
- ``(system <cmd>)`` to execute a command using the system shell: ``sh`` on Unix
  and ``cmd`` on Windows
- ``(bash <cmd>)`` to execute a command using ``/bin/bash``. This is obviously
  not very portable
- ``(diff <file1> <file2>)`` is similar to ``(run diff <file1>
  <file2>)`` but is better and allows promotion.  See `Diffing and
  promotion`_ for more details
- ``(diff? <file1> <file2>)`` is similar to ``(diff <file1>
  <file2>)`` except that ``<file2>`` should be produced by a part of the
  same action rather than be a dependency, is optional and will
  be consumed by ``diff?``.
- ``(cmp <file1> <file2>)`` is similar to ``(run cmp <file1>
  <file2>)`` but allows promotion.  See `Diffing and promotion`_ for
  more details
- ``(no-infer <DSL>)`` to perform an action without inference of dependencies
  and targets. This is useful if you are generating dependencies in a way
  that Dune doesn't know about, for instance by calling an external build system.
- ``(pipe-<outputs> <DSL> <DSL> <DSL>...)`` to execute several actions (at least two)
  in sequence, filtering the ``<outputs>`` of the first command through the other
  command, piping the standard output of each one into the input of the next.
  This action is available since dune 2.7.

As mentioned ``copy#`` inserts a line directive at the beginning of
the destination file. More precisely, it inserts the following line:

.. code:: ocaml

    # 1 "<source file name>"

Most languages recognize such lines and update their current location,
in order to report errors in the original file rather than the
copy. This is important as the copy exists only under the ``_build``
directory and in order for editors to jump to errors when parsing the
output of the build system, errors must point to files that exist in
the source tree. In the beta versions of dune, ``copy#`` was
called ``copy-and-add-line-directive``. However, most of time one
wants this behavior rather than a bare copy, so it was renamed to
something shorter.

Note: expansion of the special ``%{<kind>:...}`` is done relative to the current
working directory of the part of the DSL being executed. So for instance if you
have this action in a ``src/foo/dune``:

.. code:: lisp

    (action (chdir ../../.. (echo %{dep:dune})))

Then ``%{dep:dune}`` will expand to ``src/foo/dune``. When you run various
tools, they often use the filename given on the command line in error messages.
As a result, if you execute the command from the original directory, it will
only see the basename.

To understand why this is important, let's consider this dune file living in
``src/foo``:

::

    (rule
     (target blah.ml)
     (deps   blah.mll)
     (action (run ocamllex -o %{target} %{deps})))

Here the command that will be executed is:

.. code:: bash

    ocamllex -o blah.ml blah.mll

And it will be executed in ``_build/<context>/src/foo``. As a result, if there
is an error in the generated ``blah.ml`` file it will be reported as:

::

    File "blah.ml", line 42, characters 5-10:
    Error: ...

Which can be a problem as you editor might think that ``blah.ml`` is at the root
of your project. What you should write instead is:

::

    (rule
     (target blah.ml)
     (deps   blah.mll)
     (action (chdir %{workspace_root} (run ocamllex -o %{target} %{deps}))))

.. _dune-action-plugin:

Sandboxing
==========

The user actions that run external commands (``run``, ``bash``, ``system``)
are opaque to dune, so dune has to rely on manual specification of dependencies
and targets. One problem with manual specification is that it's error-prone.
It's often hard to know in advance what files the command will read.
And knowing a correct set of dependencies is very important for build
reproducibility and incremental build correctness.

To help with this problem dune supports sandboxing.
An idealized view of sandboxing is that it runs the action in an environment
where it can't access anything except for its declared dependencies.

In practice we have to make compromises and have some trade-offs between
simplicity, information leakage, performance and portability.

The way sandboxing is currently implemented is that for each sandboxed action
we build a separate directory tree (sandbox directory) that mirrors the build
directory, filtering it to only contain the files that were declared as
dependencies. Then we run the action in that directory, and then we copy
the targets back to the build directory.

You can configure dune to use sandboxing modes ``symlink`` or ``copy``, which
determines how the individual files are populated (they will be symlinked or
copied into the sandbox directory).

This approach is very simple and portable, but that comes with
certain limitations:

- The actions in the sandbox can use absolute paths to refer to anywhere outside
  the sandbox. This means that only dependencies on relative paths in the build
  tree can be enforced/detected by sandboxing.
- The sandboxed actions still run with full permissions of dune itself so
  sandboxing is not a security feature. It won't prevent network access either.
- We don't erase the environment variables of the sandboxed
  commands. This is something we want to change.
- Performance impact is usually small, but it can get noticeable for
  fast actions with very large sets of dependencies.

Per-action sandboxing configuration
-----------------------------------

Some actions may rely on sandboxing to work correctly.
For example an action may need the input directory to contain nothing
except the input files, or the action might create temporary files that
break other build actions.

Some other actions may refuse to work with sandboxing, for example
if they rely on absolute path to the build directory staying fixed,
or if they deliberately use some files without declaring dependencies
(this is usually a very bad idea, by the way).

Generally it's better to improve the action so it works with or without
sandboxing (especially with), but sometimes you just can't do that.

Things like this can be described using the "sandbox" field in the dependency
specification language (see :ref:`deps-field`).


Global sandboxing configuration
-------------------------------

Dune always respects per-action sandboxing specification.
You can configure it globally to prefer a certain sandboxing mode if
the action allows it.

This is controlled by:

- ``dune --sandbox <...>`` cli flag (see ``man dune-build``)
- ``DUNE_SANDBOX`` environment (see ``man dune-build``)
- ``(sandboxing_preference ..)`` field in the dune config (see ``man dune-config``)

.. _locks:

Locks
=====

Given two rules that are independent, dune will assume that there
associated action can be run concurrently. Two rules are considered
independent if none of them depend on the other, either directly or
through a chain of dependencies. This basic assumption allows dune to
parallelize the build.

However, it is sometimes the case that two independent rules cannot be
executed concurrently. For instance this can happen for more
complicated tests. In order to prevent dune from running the
actions at the same time, you can specify that both actions take the
same lock:

.. code:: lisp

    (rule
     (alias  runtest)
     (deps   foo)
     (locks  m)
     (action (run test.exe %{deps})))

    (alias
     (rule   runtest)
     (deps   bar)
     (locks  m)
     (action (run test.exe %{deps})))

Dune will make sure that the executions of ``test.exe foo`` and
``test.exe bar`` are serialized.

Although they don't live in the filesystem, lock names are interpreted as file
names. So for instance ``(with-lock m ...)`` in ``src/dune`` and ``(with-lock
../src/m)`` in ``test/dune`` refer to the same lock.

Note also that locks are per build context. So if your workspace has two build
contexts setup, the same rule might still be executed concurrently between the
two build contexts. If you want a lock that is global to all build contexts,
simply use an absolute filename:

.. code:: lisp

    (rule
     (alias   runtest)
     (deps   foo)
     (locks  /tcp-port/1042)
     (action (run test.exe %{deps})))

Diffing and promotion
=====================

``(diff <file1> <file2>)`` is very similar to ``(run diff <file1>
<file2>)``. In particular it behaves in the same way:

- when ``<file1>`` and ``<file2>`` are equal, it does nothing
- when they are not, the differences are shown and the action fails

However, it is different for the following reason:

- the exact command used to diff files can be configured via the
  ``--diff-command`` command line argument. Note that it is only
  called when the files are not byte equals

- by default, it will use ``patdiff`` if it is installed. ``patdiff``
  is a better diffing program. You can install it via opam with:

  .. code:: sh

     $ opam install patdiff

- on Windows, both ``(diff a b)`` and ``(diff? a b)`` normalize
  end-of-line characters before comparing the files

- since ``(diff a b)`` is a builtin action, dune knows that ``a``
  and ``b`` are needed and so you don't need to specify them
  explicitly as dependencies

- you can use ``(diff? a b)`` after a command that might or might not
  produce ``b``. For cases where commands optionally produce a
  *corrected* file

- if ``<file1>`` doesn't exists it will compare with the empty file

- it allows promotion. See below

Note that ``(cmp a b)`` does no end-of-line normalization and doesn't
print a diff when the files differ. ``cmp`` is meant to be used with
binary files.

Promotion
---------

Whenever an action ``(diff <file1> <file2>)`` or ``(diff?  <file1>
<file2>)`` fails because the two files are different, dune allows
you to promote ``<file2>`` as ``<file1>`` if ``<file1>`` is a source
file and ``<file2>`` is a generated file.

More precisely, let's consider the following dune file:

.. code:: scheme

   (rule
    (with-stdout-to data.out (run ./test.exe)))

   (rule
    (alias   runtest)
    (action (diff data.expected data.out)))

Where ``data.expected`` is a file committed in the source
repository. You can use the following workflow to update your test:

- update the code of your test
- run ``dune runtest``. The diff action will fail and a diff will
  be printed
- check the diff to make sure it is what you expect
- run ``dune promote``. This will copy the generated ``data.out``
  file to ``data.expected`` directly in the source tree

You can also use ``dune runtest --auto-promote`` which will
automatically do the promotion.

Package specification
=====================

Installation is the process of copying freshly built libraries,
binaries and other files from the build directory to the system.  Dune
offers two way of doing this: via opam or directly via the ``install``
command.  In particular, the installation model implemented by Dune
was copied from opam. Opam is the standard OCaml package manager.

In both cases, Dune only know how to install whole packages.  A
package being a collection of executables, libraries and other files.
In this section, we will describe how to define a package, how to
"attach" various elements to it and how to proceed with installing it
on the system.

.. _declaring-a-package:

Declaring a package
-------------------

To declare a package, simply add a ``package`` stanza to your
``dune-project`` file:

.. code:: lisp

          (package
           (name mypackage)
           (synopsis "My first Dune package!")
           (description "\| This is my first attempt at creating
                        "\| a project with Dune.
          ))

Once you have done this, Dune will know about the package named
``mypackage`` and you will be able to attach various elements to it.
The ``package`` stanza accepts more fields, such as dependencies.

Note that package names are in a global namespace so the name you choose must
be universally unique.  In particular, package managers never allow to
release two packages with the same name.

.. TODO: describe this more in details

In older projects using Dune, packages were defined by manually writing a file
called ``<package-name>.opam`` at the root of the project. However, it is not
recommended to use this method in new projects as we expect to deprecate it in
the future. The right way to define a package is with a ``package`` stanza in
the ``dune-project`` file.

See :ref:`opam-generation` for instructions on configuring dune to automatically
generate ``.opam`` files based on the ``package`` stanzas.

Attaching elements to a package
-------------------------------

Attaching an element to a package means declaring to Dune that this
element is part of the said package.  The method to attach an element
to a package depends on the kind of the element.  In this sub-section
we will go through the various kinds of elements and describe how to
attach each of them to a package.

In the rest of this section, ``<prefix>`` refers to the directory in
which the user chooses to install packages.  When installing via opam,
it is opam who sets this directory.  When calling ``dune install``,
the installation directory is either guessed or can be manually
specified by the user.  This is described more in detail in the last
section of this page.

Sites of a package
------------------

When packages need additional resources outside their binary, their location
could be hard to find. Moreover some packages could add resources to another
package, for example in the case of plugins. These location are called sites in
dune. One package can define them. During execution one site corresponds to a
list of directories. They are like layers, the first directories have an higher
priority. Examples and precisions are available at :ref:`sites`.


Libraries
^^^^^^^^^

In order to attach a library to a package all you need to do is add a
``public_name`` field to your library.  This is the name that external
users of your libraries must use in order to refer to it.  Dune
requires that the public name of a library is either the name of the
package it is part of or start with the package name followed by a dot
character.

For instance:

.. code:: scheme

   (library
    (name mylib)
    (public_name mypackage.mylib))

After you have added a public name to a library, Dune will know to
install it as part of the package it is attached to.  Dune installs
the library files in a directory ``<prefix>/lib/<package-name>``.

If the library name contains dots, the full directory in which the
library files are installed is ``lib/<comp1>/<comp2/.../<compn>``
where ``<comp1>``, ``<comp2>``, ... ``<compn>`` are the dot separated
component of the public library name.  By definition, ``<comp1>`` is
always the package name.

Executables
^^^^^^^^^^^

Similarly to libraries, to attach an executable to a package simply
add a ``public_name`` field to your ``executable`` stanza, or a
``public_names`` field for ``executables`` stanzas.  The name that
goes in there is the name under which the executables will be
available through the ``PATH`` once installed, i.e. the name users
will need to type in there shell to execute the program.  Because Dune
cannot guess which package an executable is part of from its public
name, you also need to add a ``package`` field unless the project
contains a single package, in which case the executable will be
attached to this package.

For instance:

.. code:: scheme

          (executable
           (name main)
           (public_name myprog)
           (package mypackage))

Once ``mypackage`` is installed on the system, the user will be able
to type the following in their shell:

::

   $ myprog

to execute the program.

Other files
^^^^^^^^^^^

For all other kinds of elements, you need to attach them manually via
an :ref:`install` stanza.


.. _foreign-sources-and-archives:

Foreign sources and archives
============================

Dune provides basic support for including foreign source files as well
as archives of foreign object files into OCaml projects via the
``foreign_stubs`` and ``foreign_archives`` fields.

.. _foreign-stubs:

Foreign stubs
-------------

You can specify foreign sources using the ``foreign_stubs`` field of the
``library`` and ``executable`` stanzas. For example:

.. code:: scheme

    (library
     (name lib)
     (foreign_stubs (language c) (names src1 src2))
     (foreign_stubs (language cxx) (names src3) (flags -O2)))

Here we declare an OCaml library ``lib``, which contains two C sources
``src1`` and ``src2``, and one C++ source ``src3`` that needs to be
compiled with ``-O2``. These source files will be compiled and packaged
with the library, along with the link-time flags to be used when
linking the final executables. When matching ``names`` to source files,
Dune treats ``*.c`` files as C sources, and ``*.cpp``, ``*.cc`` and
``*.cxx`` files as C++ sources.

Here is a complete list of supported subfields:

- ``language`` specifies the source language, where ``c`` means C and
  ``cxx`` means C++. In future, more languages may be supported.
- ``names`` specifies the *names* of source files. When specifying a source
  file, you should omit the extension and any relative parts of the path;
  Dune will scan all library directories, finding all matching files and
  raising an error if multiple source files map to the same object name.
  If you need to have multiple object files with the same name, you can
  package them into different :ref:`foreign-archives` via the
  ``foreign_archives`` field. This field uses the :ref:`ordered-set-language`
  where the ``:standard`` value corresponds to the set of names of all
  source files whose extensions match the specified ``language``.
- ``flags`` are passed when compiling source files. This field is specified
  using the :ref:`ordered-set-language`, where the ``:standard`` value comes
  from the environment settings ``c_flags`` and ``cxx_flags``, respectively.
  Note that, for C subs, Dune unconditionally adds the flags present in the
  fields ``ocamlc_cflags`` and ``ocamlc_cppflags`` of the OCaml config to the
  compiler command line. This behavior can be disabled since Dune 2.8 via the
  ``dune-project`` option :ref:`always-add-cflags`.
- ``include_dirs`` are tracked as dependencies and passed to the compiler
  via the ``-I`` flag. You can use :ref:`variables` in this field, and
  refer to a library source directory using the ``(lib library-name)`` syntax.
  For example, ``(include_dirs dir1 (lib lib1) (lib lib2) dir2)`` specifies
  the directory ``dir1``, the source directories of ``lib1`` and ``lib2``,
  and the directory ``dir2``, in this order. The contents of included
  directories is tracked recursively, e.g. if you use ``(include_dir dir)``
  and have headers ``dir/base.h`` and ``dir/lib/lib.h`` then they both will
  be tracked as dependencies.
- ``extra_deps`` specifies any other dependencies that should be tracked.
  This is useful when dealing with ``#include`` statements that escape into
  a parent directory like ``#include "../a.h"``.


.. _foreign-archives:

Foreign archives
----------------

You can also specify archives of separately compiled foreign object files
that need to be packaged with an OCaml library or linked into an OCaml
executable. To do that, use the ``foreign_archives`` field of the
corresponding ``library`` or ``executable`` stanza. For example:

.. code:: scheme

    (library
     (name lib)
     (foreign_stubs (language c) (names src1 src2))
     (foreign_stubs (language cxx) (names src3) (flags -O2))
     (foreign_archives arch1 some/dir/arch2))

Here, in addition to :ref:`foreign-stubs`, we also specify foreign archives
``arch1`` and ``arch2``, where the latter is stored in a subdirectory
``some/dir``.

You can build a foreign archive manually, e.g. using a custom ``rule`` as
described in :ref:`foreign-sandboxing`, or ask Dune to build it via the
``foreign_library`` stanza:

.. code:: scheme

    (foreign_library
     (archive_name arch1)
     (language c)
     (names src4 src5)
     (include_dir headers))

This asks Dune to compile C source files ``src4`` and ``src5`` with
headers tracked in the ``headers`` directory, and put the resulting
object files into an archive ``arch1``, whose full name is typically
``libarch1.a`` for static linking and ``dllarch1.so`` for dynamic
linking.

The ``foreign_library`` stanza supports all :ref:`foreign-stubs` fields plus
the ``archive_name`` field, which specifies the archive's name. You can refer
to the same archive name from multiple OCaml libraries and executables, so a
foreign archive is a bit like a foreign library, hence the name of the stanza.

Foreign archives are particularly useful when embedding a library written in
a foreign language and/or built with another build system. See
:ref:`foreign-sandboxing` for more details.

.. _flags-flow:

Flags
-----

Depending on the :ref:`always-add-cflags` option, the base `:standard` set of
flags for C will contain only ``ocamlc_cflags`` or both ``ocamlc_cflags`` and
`ocamlc_cflags`.

There are multiple levels where one can declare custom flags (using the
:ref:`ordered-set-language`), and each level inherits the flags of the previous
one in its `:standard` set:

- In the global `env` definition of a `dune-workspace` file
- In the per-context `env` definitions in a `dune-workspace` file
- In the env definition of a `dune` file
- In a `foreign_` field of an executable or a library

The ``%{cc}`` :ref:`variable <variables>` will contain the flags from the first
three levels only.
