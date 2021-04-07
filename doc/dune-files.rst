****************
Stanza reference
****************

dune-project
============

These files are used to mark the root of projects as well as define project-wide
parameters. The first line of ``dune-project`` must be a ``lang`` stanza with no
extra whitespace or comments. The ``lang`` stanza controls the names and
contents of all configuration files read by Dune and looks like:

.. code:: scheme

   (lang dune 3.0)

Additionally, they can contains the following stanzas.

name
----

Sets the name of the project. This is used by :ref:`dune subst <dune-subst>`
and error messages.

.. code:: scheme

    (name <name>)

version
-------

Sets the version of the project:

.. code:: scheme

    (version <version>)

.. _implicit_transitive_deps:

implicit_transitive_deps
------------------------

By default, dune allows transitive dependencies of dependencies to be used
directly when compiling OCaml. However, this setting can be controlled per
project:

.. code:: scheme

    (implicit_transitive_deps <bool>)

When set to ``false``, all dependencies that are directly used by a library
or an executable must be directly added in the ``libraries`` field. We
recommend users to experiment with this mode and report any problems.

Note that you must use ``threads.posix`` instead of ``threads`` when using this
mode. This is not an important limitation as ``threads.vm`` are deprecated
anyways.

In some situations, it's desirable to selectively preserve the
behavior of transitive dependencies being available to users of a
library. For example, if we define a library ``foo_more``, that
extends ``foo``, we might want users of ``foo_more`` to immediately
have ``foo`` available as well. To do this, we must define the
dependency on ``foo`` as re-exported:

.. code:: scheme

   (library
    (name foo_more)
    (libraries (re_export foo)))

.. _wrapped-executables:

wrapped_executables
-------------------

Executables are made of compilation units whose names may collide with the
compilation units of libraries. To avoid this possibility, dune prefixes these
compilation unit names with ``Dune__exe__``. This is entirely transparent to
users except for when such executables are debugged. In which case the mangled
names will be visible in the debugger.

Starting from dune 1.11, an option is available to turn on/off name mangling for
executables on a per project basis:

.. code:: scheme

    (wrapped_executables <bool>)

Starting from dune 2.0, dune mangles compilation units of executables by
default. However, this can still be turned off using ``(wrapped_executables
false)``

.. _executables_implicit_empty_intf:

executables_implicit_empty_intf
-------------------------------

By default, executables defined via ``(executables(s) ...)`` or ``(test(s)
...)`` stanzas are compiled with the interface file provided (e.g. ``.mli`` or
``rei``). Since these modules cannot be used as library dependencies, it's
common to give them empty interface files to strengthen the compiler's ability
to detect unused values in these modules.

Starting from dune 2.9, an option is available to automatically generate empty
interface files for executables and tests that don't already have them:

.. code:: scheme

    (executables_implicit_empty_intf true)

.. _explicit-js-mode:

explicit_js_mode
----------------

Traditionally, JavaScript targets were defined for every bytecode executable.
This was not very precise and did not interact well with the ``@all`` alias.

You can opt out of this behaviour by using:

.. code:: scheme

    (explicit_js_mode)

When this mode is enabled, an explicit ``js`` mode needs to be added to the
``(modes ...)`` field of executables in order to trigger JavaScript
compilation. Explicit JS targets declared like this will be attached to the
``@all`` alias.

Starting from dune 2.0 this behaviour is the default, and there is no way to
disable it.

.. _dialect:

dialect
-------

A dialect is an alternative frontend to OCaml (such as ReasonML). It is
described by a pair of file extensions, one corresponding to interfaces and one
to implementations.

A dialect can use the standard OCaml syntax or it can specify an action to
convert from a custom syntax to a binary OCaml abstract syntax tree.

Similarly, a dialect can specify a custom formatter to implement the ``@fmt``
alias, see :ref:`formatting-main`.

When not using a custom syntax or formatting action, a dialect is nothing but a
way to specify custom file extensions for OCaml code.

.. code:: scheme

    (dialect
     (name <name>)
     (implementation
      (extension <string>)
      <optional fields>)
     (interface
      (extension <string>)
      <optional fields>))

``<name>`` is the name of the dialect being defined. It must be unique in a
given project.

``(extension <string>)`` specifies the file extension used for this dialect, for
interfaces and implementations. The extension string must not contain any dots,
and be unique in a given project (so that a given extension can be mapped back
to a corresponding dialect).

``<optional fields>`` are:

- ``(preprocess <action>)`` is the action to run to produce a valid OCaml
  abstract syntax tree. It is expected to read the file given in the variable
  named ``input-file`` and output a *binary* abstract syntax tree on its
  standard output. See :ref:`preprocessing-actions` for more information.

  If the field is not present, it is assumed that the corresponding source code
  is already valid OCaml code and can be passed to the OCaml compiler as-is.


- ``(format <action>)`` is the action to run to format source code for this
  dialect. The action is expected to read the file given in the variable named
  ``input-file`` and output the formatted source code on its standard
  output. For more information. See :ref:`formatting-main` for more information.

  If the field is not present, then if ``(preprocess <action>)`` is not present
  (so that the dialect consists of valid OCaml code), then by default the
  dialect will be formatted as any other OCaml code. Otherwise no special
  formatting will be done.

.. _formatting:

formatting
----------

Starting in dune 2.0, :ref:`formatting-main` is automatically enabled. This can be
controlled by using

.. code:: scheme

    (formatting <setting>)

where ``<setting>`` is one of:

- ``disabled``, meaning that automatic formatting is disabled

- ``(enabled_for <languages>)`` can be used to restrict the languages that are
  considered for formatting.

generate_opam_files
-------------------

Dune is able to use metadata specified in the ``dune-project`` file to generate
``.opam`` files, see :ref:`opam-generation`. To enable this integration, add the
following field to the ``dune-project`` file:

.. code:: scheme

   (generate_opam_files true)

Dune uses the following global fields to set the metadata for all packages
defined in the project:

- ``(license <name>)`` - Specifies the license of the project, ideally as an
  identifier from the `SPDX License List <https://spdx.org/licenses/>`__

- ``(authors <authors>)`` - A list of authors

- ``(maintainers <maintainers>)`` - A list of maintainers

- ``(source <source>)`` - where the source is specified two ways:
  ``(github <user/repo>)`` or ``(uri <uri>)``

- ``(bug_reports <url>)`` - Where to report bugs. This defaults to the GitHub
  issue tracker if the source is specified as a GitHub repository

- ``(homepage <url>)`` - The homepage of the project

- ``(documentation <url>)`` - Where the documentation is hosted

With this fields in, every time dune is called to execute some rules (either via
``dune build``, ``dune runtest`` or something else), the opam files get
generated.

Some or all of these fields may be overridden for each package of the project, see
:ref:`package`.

.. _package:

package
-------

Package specific information is specified in the ``(package <package>)`` stanza.
It contains the following fields:

- ``(name <string>)`` is the name of the package. This must be specified.

- ``(synopsis <string>)`` is a short package description

- ``(description <string>)`` is a longer package description

- ``(depends <dep-specification>)`` are package dependencies

- ``(conflicts <dep-specification)`` are package conflicts

- ``(depopts <dep-specification)`` are optional package dependencies

- ``(tags <tags>)`` are the list of tags for the package

- ``(deprecated_package_names <name list>)`` is a list of names that can be used
  with the :ref:`deprecated-library-name` stanza to migrate legacy libraries
  from other build systems which do not follow Dune's convention of prefixing
  the public name of the library with the package name.

- ``(license <name>)``, ``(authors <authors>)``, ``(maintainers
  <maintainers>)``, ``(source <source>)``, ``(bug_reports <url>)``, ``(homepage
  <url>)``, ``(documentation <url>)`` are the same (and take precedence over)
  the corresponding global fields. These fields are available since Dune 2.0.

- ``(sites (<section> <name>) ...)`` define a site named ``<name>`` in the
  section ``<section>``.

Adding libraries to different packages is done via  ``public_name`` field. See
:ref:`library` section for details.

The list of dependencies ``<dep-specification>`` is modeled after opam's own
language: The syntax is as a list of the following elements:

.. code::

   op := '=' | '<' | '>' | '<>' | '>=' | '<='

   stage := :with-test | :build | :dev

   constr := (<op> <version>)

   logop := or | and

   dep := (name <stage>)
        | (name <constr>)
        | (name (<logop> (<stage> | <constr>)*))

   dep-specification = dep+

.. _always-add-cflags:

use_standard_c_and_cxx_flags
----------------------------

Since Dune 2.8, it is possible to deactivate the systematic prepending of flags
coming from ``ocamlc -config`` to the C compiler command line. This is done
adding the following field to the ``dune-project`` file:

.. code:: scheme

    (use_standard_c_and_cxx_flags true)

In this mode, dune will populate the ``:standard`` set of C flags with the
content of ``ocamlc_cflags`` and  ``ocamlc_cppflags``. These flags can be
completed or overridden using the :ref:`ordered-set-language`.

dune
====

``dune`` files are the main part of dune. They are used to describe libraries,
executables, tests, and everything dune needs to know about.

The syntax of ``dune`` files is described in :ref:`metadata-format` section.

``dune`` files are composed of stanzas. For instance a typical
``dune`` looks like:

.. code:: lisp

    (library
     (name mylib)
     (libraries base lwt))

    (rule
     (target foo.ml)
     (deps   generator/gen.exe)
     (action (run %{deps} -o %{target})))

The following sections describe the available stanzas and their meaning.

jbuild_version
--------------

Deprecated. This stanza is no longer used and will be removed in the
future.

.. _library:

library
-------

The ``library`` stanza must be used to describe OCaml libraries. The
format of library stanzas is as follows:

.. code:: scheme

    (library
     (name <library-name>)
     <optional-fields>)

``<library-name>`` is the real name of the library. It determines the
names of the archive files generated for the library as well as the
module name under which the library will be available, unless
``(wrapped false)`` is used (see below). It must be a valid OCaml
module name but doesn't need to start with a uppercase letter.

For instance, the modules of a library named ``foo`` will be
available as ``Foo.XXX`` outside of ``foo`` itself. It is however
allowed to write an explicit ``Foo`` module, in which case this will
be the interface of the library and you are free to expose only the
modules you want.

Note that by default libraries and other things that consume
OCaml/Reason modules only consume modules from the directory where the
stanza appear. In order to declare a multi-directory library, you need
to use the :ref:`include_subdirs` stanza.

``<optional-fields>`` are:

- ``(public_name <name>)`` this is the name under which the library can be
  referred to as a dependency when it is not part of the current workspace,
  i.e. when it is installed. Without a ``(public_name ...)`` field, the library
  will not be installed by dune. The public name must start by the package
  name it is part of and optionally followed by a dot and anything else you
  want. The package name must be one of the packages that dune knows about,
  as determined by the :ref:`opam-files`

- ``(package <package>)`` Install private library under the specified package.
  Such a library is now usable by public libraries defined in the same project.
  The findlib name for this library will be ``<package>.__private__.<name>``,
  however the library's interface will be hidden from consumers outside the
  project.

- ``(synopsis <string>)`` should give a one-line description of the library.
  This is used by tools that list installed libraries

- ``(modules <modules>)`` specifies what modules are part of the library. By
  default dune will use all the .ml/.re files in the same directory as the
  ``dune`` file. This include ones that are present in the file system as well
  as ones generated by user rules. You can restrict this list by using a
  ``(modules <modules>)`` field. ``<modules>`` uses the :ref:`ordered-set-language`
  where elements are module names and don't need to start with a uppercase
  letter. For instance to exclude module ``Foo``: ``(modules (:standard \
  foo))``

- ``(libraries <library-dependencies>)`` is used to specify the dependencies
  of the library. See the section about :ref:`library-deps` for more details

- ``(wrapped <boolean>)`` specifies whether the modules of the library should be
  available only through the top-level library module, or should all be exposed
  at the top level. The default is ``true`` and it is highly recommended to keep
  it this way. Because OCaml top-level modules must all be unique when linking
  an executables, polluting the top-level namespace will make your library
  unusable with other libraries if there is a module name clash. This option is
  only intended for libraries that manually prefix all their modules by the
  library name and to ease porting of existing projects to dune

- ``(wrapped (transition <message>))`` Is the same as ``(wrapped true)`` except
  that it will also generate unwrapped (not prefixed by the library name)
  modules to preserve compatibility. This is useful for libraries that would
  like to transition from ``(wrapped false)`` to ``(wrapped true)`` without
  breaking compatibility for users. The ``<message>`` will be included in the
  deprecation notice for the unwrapped modules.

- ``(preprocess <preprocess-spec>)`` specifies how to preprocess files if
  needed. The default is ``no_preprocessing``. Other options are described in the
  :ref:`preprocessing-spec` section

- ``(preprocessor_deps (<deps-conf list>))`` specifies extra dependencies of the
  preprocessor, for instance if the preprocessor reads a generated file. The
  specification of dependencies is described in the :ref:`deps-field`
  section

- ``(optional)``, if present it indicates that the library should only be built
  and installed if all the dependencies are available, either in the workspace
  or in the installed world. You can use this to provide extra features without
  adding hard dependencies to your project

- ``(foreign_stubs <foreign-stubs-spec>)`` specifies foreign source files, e.g.
  C or C++ stubs, to be compiled and packaged together with the library. See
  the section :ref:`foreign-sources-and-archives` for more details. This field
  replaces the now deleted fields ``c_names``, ``c_flags``, ``cxx_names``
  and ``cxx_flags``.

- ``(foreign_archives <foreign-archives-list>)`` specifies archives of foreign
  object files to be packaged with the library. See the section
  :ref:`foreign-archives` for more details. This field replaces the now
  deleted field ``self_build_stubs_archive``.

- ``(install_c_headers (<names>))``, if your library has public C header files
  that must be installed, you must list them in this field, without the ``.h``
  extension

- ``(modes <modes>)`` modes which should be built by default. The
  most common use for this feature is to disable native compilation
  when writing libraries for the OCaml toplevel. The following modes
  are available: ``byte``, ``native`` and ``best``. ``best`` is
  ``native`` or ``byte`` when native compilation is not available

- ``(no_dynlink)`` is to disable dynamic linking of the library. This is for
  advanced use only, by default you shouldn't set this option

- ``(kind <kind>)`` is the kind of the library. The default is ``normal``, other
  available choices are ``ppx_rewriter`` and ``ppx_deriver`` and must be set
  when the library is intended to be used as a ppx rewriter or a ``[@@deriving
  ...]`` plugin. The reason why ``ppx_rewriter`` and ``ppx_deriver`` are split
  is historical and hopefully we won't need two options soon. Both ppx kinds
  support an optional field ``(cookies <cookies>)`` where ``<cookies>`` is a
  list of pairs ``(<name> <value>)`` with ``<name>`` being the cookie name and
  ``<value>`` is a string that supports :ref:`variables` evaluated
  by each invocation of the preprocessor (note: libraries that share
  cookies with the same name should agree on their expanded value)

- ``(ppx_runtime_libraries (<library-names>))`` is for when the library is a ppx
  rewriter or a ``[@@deriving ...]`` plugin and has runtime dependencies. You
  need to specify these runtime dependencies here

- ``(virtual_deps (<opam-packages>)``. Sometimes opam packages enable a specific
  feature only if another package is installed. This is for instance the case of
  ``ctypes`` which will only install ``ctypes.foreign`` if the dummy
  ``ctypes-foreign`` package is installed. You can specify such virtual
  dependencies here. You don't need to do so unless you use dune to
  synthesize the ``depends`` and ``depopts`` sections of your opam file

- ``js_of_ocaml`` sets options for JavaScript compilation, see :ref:`jsoo-field`

- ``flags``, ``ocamlc_flags`` and ``ocamlopt_flags``. See the section about
  :ref:`ocaml-flags`

- ``(library_flags (<flags>))`` is a list of flags that are passed as it to
  ``ocamlc`` and ``ocamlopt`` when building the library archive files. You can
  use this to specify ``-linkall`` for instance. ``<flags>`` is a list of
  strings supporting :ref:`variables`

- ``(c_library_flags <flags>)`` specifies the flags to pass to the C compiler
  when constructing the library archive file for the C stubs. ``<flags>`` uses
  the :ref:`ordered-set-language` and supports ``(:include ...)`` forms. When you
  are writing bindings for a C library named ``bar``, you should typically write
  ``-lbar`` here, or whatever flags are necessary to link against this
  library

- ``(modules_without_implementation <modules>)`` specifies a list of
  modules that have only a ``.mli`` or ``.rei`` but no ``.ml`` or
  ``.re`` file. Such modules are usually referred as *mli only
  modules*. They are not officially supported by the OCaml compiler,
  however they are commonly used. Such modules must only define
  types. Since it is not reasonably possible for dune to check
  that this is the case, dune requires the user to explicitly list
  such modules to avoid surprises.  Note that the
  ``modules_without_implementation`` field is not merged in ``modules``, which
  represents the total set of modules in a library. If a directory has more
  than one stanza and thus a ``modules`` field must be specified, ``<modules>``
  still need to be added in ``modules``.

- ``(private_modules <modules>)`` specifies a list of modules that will be
  marked as private. Private modules are inaccessible from outside the libraries
  they are defined in. Note that the ``private_modules`` field is not merged in
  ``modules``, which represents the total set of modules in a library. If a
  directory has more than one stanza and thus a ``modules`` field must be
  specified, ``<modules>`` still need to be added in ``modules``.

- ``(allow_overlapping_dependencies)`` allows external dependencies to
  overlap with libraries that are present in the workspace

- ``(enabled_if <blang expression>)`` conditionally disables
  a library. A disabled library cannot be built and will not be
  installed. The condition is specified using the :ref:`blang`, and the
  field allows for the ``%{os_type}`` variable, which is expanded to
  the type of OS being targeted by the current build. Its value is
  the same as the value of the ``os_type`` parameter in the output of
  ``ocamlc -config``

- ``(inline_tests)`` enables inline tests for this library. They can be
  configured through options using ``(inline_tests <options>)``. See
  :ref:`inline_tests` for a reference of corresponding options.

- ``(root_module <module>)`` this field instructs dune to generate a module that
  will contain module aliases for every library specified in dependencies. This
  is useful whenever a library is shadowed by a local module. The library may
  then still be accessible via this root module

Note that when binding C libraries, dune doesn't provide special support for
tools such as ``pkg-config``, however it integrates easily with
:ref:`configurator` by
using ``(c_flags (:include ...))`` and ``(c_library_flags (:include ...))``.

.. _foreign_library:

foreign_library
---------------

The ``foreign_library`` stanza describes archives of separately compiled
foreign object files that can be packaged with an OCaml library or linked
into an OCaml executable. See :ref:`foreign-sources-and-archives` for
further details and examples.

.. _jsoo-field:

js_of_ocaml
~~~~~~~~~~~

In ``library`` and ``executables`` stanzas, you can specify ``js_of_ocaml``
options using ``(js_of_ocaml (<js_of_ocaml-options>))``.

``<js_of_ocaml-options>`` are all optional:

- ``(flags <flags>)`` to specify flags passed to ``js_of_ocaml``. This field
  supports ``(:include ...)`` forms

- ``(javascript_files (<files-list>))`` to specify ``js_of_ocaml`` JavaScript
  runtime files.

``<flags>`` is specified in the :ref:`ordered-set-language`.

The default value for ``(flags ...)`` depends on the selected build profile. The
build profile ``dev`` (the default) will enable sourcemap and the pretty
JavaScript output.

See :ref:`jsoo` for more information.

.. _deprecated-library-name:

deprecated_library_name
-----------------------

The ``deprecated_library_name`` stanza enables redirecting an old
deprecated name after a library has been renamed. It's syntax is as
follows:

.. code:: scheme

    (deprecated_library_name
     (old_public_name <name>)
     (new_public_name <name>))

When a developer uses the old public name in a list of library
dependencies, it will be transparently replaced by the new name. Note
that it is not necessary for the new name to exist at definition time
as it is only resolved at the point where the old name is used.

The ``old_public_name`` can also be one of the names declared in the
``deprecated_package_names`` field of the package declaration in
``dune-project`` file. In this case, the "old" library is understood to be a
library whose name is not prefixed by the package name. Such a library cannot be
defined in Dune, but other build systems allow it and this feature is meant to
help migration from those systems.

executable
----------

The ``executable`` stanza must be used to describe an executable. The
format of executable stanzas is as follows:

.. code:: scheme

    (executable
     (name <name>)
     <optional-fields>)

``<name>`` is a module name that contains the main entry point of the
executable. There can be additional modules in the current directory,
you only need to specify the entry point. Given an ``executable``
stanza with ``(name <name>)``, dune will know how to build
``<name>.exe``. If requested, it will also know how to build
``<name>.bc`` and ``<name>.bc.js`` (dune 2.0 and up also need specific
configuration, see the ``modes`` optional field below). ``<name>.exe``
is a native code executable, ``<name>.bc`` is a bytecode executable
which requires ``ocamlrun`` to run and ``<name>.bc.js`` is a JavaScript
generated using js_of_ocaml.

Note that in case native compilation is not available, ``<name>.exe``
will in fact be a custom byte-code executable. Custom in the sense of
``ocamlc -custom``, meaning that it is a native executable that embeds
the ``ocamlrun`` virtual machine as well as the byte code. As such you
can always rely on ``<name>.exe`` being available. Moreover, it is
usually preferable to use ``<name>.exe`` in custom rules or when
calling the executable by hand. This is because running a byte-code
executable often requires loading shared libraries that are locally
built, and so requires additional setup such as setting specific
environment variables and dune doesn't do at the moment.

Native compilation is considered not available when there is no ``ocamlopt``
binary at the same place as where ``ocamlc`` was found.

Executables can also be linked as object or shared object files. See
`linking modes`_ for more information.

Starting from dune 2.9, it's possible to automatically generate empty interface
files for executables. See `executables_implicit_empty_intf`_.

``<optional-fields>`` are:

- ``(public_name <public-name>)`` specifies that the executable should be
  installed under that name. It is the same as adding the following stanza to
  your ``dune`` file:

   .. code:: scheme

       (install
        (section bin)
        (files (<name>.exe as <public-name>)))

.. _shared-exe-fields:

- ``(package <package>)`` if there is a ``(public_name ...)`` field, this
  specifies the package the executables are part of

- ``(libraries <library-dependencies>)`` specifies the library dependencies.
  See the section about :ref:`library-deps` for more details

- ``(link_flags <flags>)`` specifies additional flags to pass to the linker.
  This field supports ``(:include ...)`` forms

- ``(link_deps (<deps-conf list>))`` specifies the dependencies used only by the
  linker, for example when using a version script. See the :ref:`deps-field`
  section for more details.

- ``(modules <modules>)`` specifies which modules in the current directory
  dune should consider when building this executable. Modules not listed
  here will be ignored and cannot be used inside the executable described by
  the current stanza. It is interpreted in the same way as the ``(modules
  ...)`` field of `library`_

- ``(root_module <module>)`` specifies a ``root_module`` that collects all
  dependencies specified in ``libraries``. See the documentation for
  ``root_module`` in the library stanza.

- ``(modes (<modes>))`` sets the `linking modes`_. The default is
  ``(exe)``. Before 2.0, it used to be ``(byte exe)``.

- ``(preprocess <preprocess-spec>)`` is the same as the ``(preprocess ...)``
  field of `library`_

- ``(preprocessor_deps (<deps-conf list>))`` is the same as the
  ``(preprocessor_deps ...)`` field of `library`_

- ``js_of_ocaml``. See the section about :ref:`jsoo-field`

- ``flags``, ``ocamlc_flags`` and ``ocamlopt_flags``. See the section about
  specifying :ref:`ocaml-flags`

- ``(modules_without_implementation <modules>)`` is the same as the
  corresponding field of `library`_

- ``(allow_overlapping_dependencies)`` is the same as the
  corresponding field of `library`_

- ``(optional)`` is the same as the corresponding field of `library`_

- ``(enabled_if <blang expression>)`` is the same as the corresponding field of `library`_

- ``(promote <options>)`` allows promoting the linked executables to
  the source tree. The options are the same as for the :ref:`rule
  promote mode <promote>`. Adding ``(promote (until-clean))`` to an
  ``executable`` stanza will cause Dune to copy the ``.exe`` files to
  the source tree and ``dune clean`` to delete them

- ``(foreign_stubs <foreign-stubs-spec>)`` specifies foreign source
  files, e.g. C or C++ stubs, to be linked into the executable. See the
  section :ref:`foreign-sources-and-archives` for more details.

- ``(foreign_archives <foreign-archives-list>)`` specifies archives of
  foreign object files to be linked into the executable. See the section
  :ref:`foreign-archives` for more details.

- ``(forbidden_libraries <libraries>)`` ensures that the given
  libraries are not linked in the resulting executable. If they end up
  being pulled in, either through a direct or transitive dependency,
  Dune fails with an error message explaining how the library was
  pulled in. This field is available since the 2.0 version of the dune
  language.

- ``(embed_in_plugin_libraries <library-list>)`` specifies a list of libraries
  to link statically when using ``plugin`` linking mode. By default, no
  libraries are linked in. Note that you may need to also use the ``-linkall``
  flag if some of the libraries listed here are not referenced from any of the
  plugin modules.

Linking modes
~~~~~~~~~~~~~

The ``modes`` field allows selecting what linking modes should be used
to link executables. Each mode is a pair ``(<compilation-mode>
<binary-kind>)`` where ``<compilation-mode>`` describes whether the
byte code or native code backend of the OCaml compiler should be used
and ``<binary-kind>`` describes what kind of file should be produced.

``<compilation-mode>`` must be ``byte``, ``native`` or ``best``, where
``best`` is ``native`` with a fallback to byte-code when native
compilation is not available.

``<binary-kind>`` is one of:

- ``c`` for producing OCaml bytecode embedded in a C file
- ``exe`` for normal executables
- ``object`` for producing static object files that can be manually
  linked into C applications
- ``shared_object`` for producing object files that can be dynamically
  loaded into an application. This mode can be used to write a plugin
  in OCaml for a non-OCaml application.
- ``js`` for producing JavaScript from bytecode executables, see
  :ref:`explicit-js-mode`.
- ``plugin`` for producing a plugin (``.cmxs`` if native or ``.cma``
  if bytecode).

For instance the following ``executables`` stanza will produce byte
code executables and native shared objects:

.. code:: scheme

          (executables
            (names a b c)
            (modes (byte exe) (native shared_object)))

Additionally, you can use the following short-hands:

- ``c`` for ``(byte c)``
- ``exe`` for ``(best exe)``
- ``object`` for ``(best object)``
- ``shared_object`` for ``(best shared_object)``
- ``byte`` for ``(byte exe)``
- ``native`` for ``(native exe)``
- ``js`` for ``(byte js)``
- ``plugin`` for ``(best plugin)``

For instance the following ``modes`` fields are all equivalent:

.. code:: scheme

          (modes (exe object shared_object))
          (modes ((best exe)
                  (best object)
                  (best shared_object)))

And finally, you can use the special mode ``byte_complete`` for
building a bytecode executable as a native self-contained
executable. I.e. an executable that does not require the ``ocamlrun``
program to run and does not requires the C stubs to be installed as
shared object files.

The extensions for the various linking modes are chosen as follows:

=========================== =================
linking mode                extensions
--------------------------- -----------------
byte                        .bc
native/best                 .exe
byte_complete               .bc.exe
(byte object)               .bc%{ext_obj}
(native/best object)        .exe%{ext_obj}
(byte shared_object)        .bc%{ext_dll}
(native/best shared_object) %{ext_dll}
c                           .bc.c
js                          .bc.js
(best plugin)               %{ext_plugin}
(byte plugin)               .cma
(native plugin)             .cmxs
=========================== =================

Where ``%{ext_obj}`` and ``%{ext_dll}`` are the extensions for object
and shared object files. Their value depends on the OS, for instance
on Unix ``%{ext_obj}`` is usually ``.o`` and ``%{ext_dll}`` is usually
``.so`` while on Windows ``%{ext_obj}`` is ``.obj`` and ``%{ext_dll}``
is ``.dll``.

Up to version 3.0 of the dune language, when ``byte`` is specified but
none of ``native``, ``exe`` or ``byte_complete`` are specified Dune
implicitly adds a linking mode that is the same as ``byte_complete``
but using the extension ``.exe``. ``.bc`` files require additional
files at runtime that are not currently tracked by Dune, so you should
not run ``.bc`` files during the build. Run the ``.bc.exe`` or
``.exe`` ones instead as these are self-contained.

Lastly, note that ``.bc`` executables cannot contain C stubs. If your
executable contains C stubs you may want to use ``(modes exe)``.

executables
-----------

The ``executables`` stanza is the same as the ``executable`` stanza, except that
it is used to describe several executables sharing the same configuration.

It shares the same fields as the ``executable`` stanza, except that instead of
``(name ...)`` and ``(public_name ...)`` you must use:

- ``(names <names>)`` where ``<names>`` is a list of entry point names. As for
  ``executable`` you only need to specify the modules containing the entry point
  of each executable

- ``(public_names <names>)`` describes under what name each executable should
  be installed. The list of names must be of the same length as the list in the
  ``(names ...)`` field. Moreover you can use ``-`` for executables that
  shouldn't be installed

rule
----

The ``rule`` stanza is used to create custom user rules. It tells dune how
to generate a specific set of files from a specific set of dependencies.

The syntax is as follows:

.. code:: scheme

    (rule
     (target[s] <filenames>)
     (action  <action>)
     <optional-fields>)

``<filenames>`` is a list of file names (if defined with ``targets``)
or exactly one file name (if defined with ``target``). Note that
currently dune only supports user rules with targets in the current
directory.

``<action>`` is the action to run to produce the targets from the dependencies.
See the :ref:`user-actions` section for more details.

``<optional-fields>`` are:

- ``(deps <deps-conf list>)`` to specify the dependencies of the
  rule. See the :ref:`deps-field` section for more details.

- ``(mode <mode>)`` to specify how to handle the targets, see `modes`_
  for details

- ``(fallback)`` is deprecated and is the same as ``(mode fallback)``

- ``(locks (<lock-names>))`` specify that the action must be run while
  holding the following locks. See the :ref:`locks` section for more details.

- ``(alias <alias-name>)`` specify the alias this rule belongs to. Building this
  alias means building the targets of this rule.

- ``(package <package>)`` specify the package this rule belongs to. This rule
  will be unavailable when installing other packages in release mode.

- ``(enabled_if <blang expression>)`` specifies the boolean condition that must
  be true for the rule to be considered. The condition is specified using the :ref:`blang`, and
  the field allows for :ref:`variables` to appear in the expressions.

Note that contrary to makefiles or other build systems, user rules currently
don't support patterns, such as a rule to produce ``%.y`` from ``%.x`` for any
given ``%``. This might be supported in the future.

modes
~~~~~

By default, the target of a rule must not exist in the source tree and
dune will error out when this is the case.

However, it is possible to change this behavior using the ``mode``
field. The following modes are available:

- ``standard``, this is the standard mode

- ``fallback``, in this mode, when the targets are already present in
  the source tree, dune will ignore the rule. It is an error if
  only a subset of the targets are present in the tree. The common use
  of fallback rules is to generate default configuration files that
  may be generated by a configure script.

.. _promote:

- ``promote`` or ``(promote <options>)``, in this mode, the files
  in the source tree will be ignored. Once the rule has been executed,
  the targets will be copied back to the source tree
  The following options are available:

  - ``(until-clean)`` means that ``dune clean`` will remove the promoted files
    from the source tree.
  - ``(into <dir>)`` means that the files are promoted in ``<dir>`` instead of
    the current directory. This feature is available since Dune 1.8.
  - ``(only <predicate>)`` means that only a subset of the targets should be
    promoted. The argument is similar to the argument of :ref:`(dirs ...)
    <dune-subdirs>`, specified using the :ref:`predicate-lang`. This feature is
    available since dune 1.10.

- ``promote-until-clean`` is the same as ``(promote (until-clean))``
- ``(promote-into <dir>)`` is the same as ``(promote (into <dir>))``
- ``(promote-until-clean-into <dir>)`` is the same as ``(promote
  (until-clean) (into <dir>))``

The ``(promote <options>)`` form is only available since Dune
1.10. Before Dune 1.10, you need to use one of the ``promote-...``
forms. The ``promote-...`` forms should disappear in Dune 2.0, so
using the more generic ``(promote <options>)`` form should be preferred
in new projects.

There are two use cases for promote rules. The first one is when the
generated code is easier to review than the generator, so it's easier
to commit the generated code and review it. The second is to cut down
dependencies during releases: by passing ``--ignore-promoted-rules``
to dune, rules with ``(mode promote)`` will be ignored and the source
files will be used instead. The ``-p/--for-release-of-packages`` flag
implies ``--ignore-promote-rules``. However, rules that promotes only
a subset of their targets via ``(only ...)`` are never ignored.

inferred rules
~~~~~~~~~~~~~~

When using the action DSL (see :ref:`user-actions`), it is most of the
time obvious what are the dependencies and targets.

For instance:

.. code:: lisp

    (rule
     (target b)
     (deps   a)
     (action (copy %{deps} %{target})))

In this example it is obvious by inspecting the action what the
dependencies and targets are. When this is the case you can use the
following shorter syntax, where dune infers dependencies and
targets for you:

.. code:: scheme

    (rule <action>)

For instance:

.. code:: scheme

    (rule (copy a b))

Note that in dune, targets must always be known
statically. For instance, this ``(rule ...)``
stanza is rejected by dune:

.. code:: lisp

    (rule (copy a b.%{read:file}))

ocamllex
--------

``(ocamllex <names>)`` is essentially a shorthand for:

.. code:: lisp

    (rule
     (target <name>.ml)
     (deps   <name>.mll)
     (action (chdir %{workspace_root}
              (run %{bin:ocamllex} -q -o %{target} %{deps}))))

To use a different rule mode, use the long form:

.. code:: scheme

    (ocamllex
     (modules <names>)
     (mode    <mode>))

.. _ocamlyacc:

ocamlyacc
---------

``(ocamlyacc <names>)`` is essentially a shorthand for:

.. code:: lisp

    (rule
     (targets <name>.ml <name>.mli)
     (deps    <name>.mly)
     (action  (chdir %{workspace_root}
               (run %{bin:ocamlyacc} %{deps}))))

To use a different rule mode, use the long form:

.. code:: scheme

    (ocamlyacc
     (modules <names>)
     (mode    <mode>))

.. _menhir:

menhir
------

A ``menhir`` stanza is available to support the menhir_ parser generator.

To use menhir in a dune project, the language version should be selected in the
``dune-project`` file. For example:

.. code:: scheme

  (using menhir 2.0)

This will enable support for menhir stanzas in the current project. If the
language version is absent, dune will automatically add this line with the
latest menhir version to the project file once a menhir stanza is used anywhere.

The basic form for defining menhir-git_ parsers (analogous to :ref:`ocamlyacc`) is:

.. code:: scheme

    (menhir
     (modules <parser1> <parser2> ...)
     <optional-fields>)

``<optional-fields>`` are:

- ``(merge_into <base_name>)`` is used to define modular parsers. This
  correspond to the ``--base`` command line option of ``menhir``. With this
  option, a single parser named ``base_name`` is generated.

- ``(flags <option1> <option2> ...)`` can be used to pass extra flags can be
  passed to menhir.

- ``(infer <bool>)`` can be used to enable using menhir with type
  inference. This option is enabled by default with Menhir language 2.0.

Menhir supports writing the grammar and automaton to ``.cmly`` file. Therefore,
if this is flag is passed to menhir, dune will know to introduce a ``.cmly``
target for the module.

.. _menhir-git: https://gitlab.inria.fr/fpottier/menhir


cinaps
------

A ``cinaps`` stanza is available to support the ``cinaps`` tool.  See
the `cinaps website <https://github.com/janestreet/cinaps>`_ for more
details.

.. _documentation-stanza:

documentation
-------------

Additional manual pages may be attached to packages using the ``documentation``
stanza. These ``.mld`` files must contain text in the same syntax as ocamldoc
comments.

.. code-block:: scheme

  (documentation (<optional-fields>))

Where ``<optional-fields>`` are:

- ``(package <name>)`` the package this documentation should be attached to. If
  this absent, dune will try to infer it based on the location of the
  stanza.

- ``(mld_files <arg>)`` where ``<arg>`` field follows the
  :ref:`ordered-set-language`. This is a set of extension-less, mld file base
  names that are attached to the package. Where ``:standard`` refers to all the
  ``.mld`` files in the stanza's directory.

For more information, see :ref:`documentation`.

.. _alias-stanza:

alias
-----

The ``alias`` stanza lets you add dependencies to an alias, or specify an action
to run to construct the alias.

The syntax is as follows:

.. code:: scheme

    (alias
     (name    <alias-name>)
     (deps    <deps-conf list>)
     <optional-fields>)

``<name>`` is an alias name such as ``runtest``.

.. _alias-fields:

``<deps-conf list>`` specifies the dependencies of the alias. See the
:ref:`deps-field` section for more details.

``<optional-fields>`` are:

- ``<action>``, an action to run when constructing the alias. See the
  :ref:`user-actions` section for more details. Note that this is removed in the
  2.0 version of the dune language. Users should port their code to use the
  ``rule`` stanza with the ``alias`` field instead.

- ``(package <name>)`` indicates that this alias stanza is part of package
  ``<name>`` and should be filtered out if ``<name>`` is filtered out from the
  command line, either with ``--only-packages <pkgs>`` or ``-p <pkgs>``

- ``(locks (<lock-names>))`` specify that the action must be run while
  holding the following locks. See the :ref:`locks` section for more details.

- ``(enabled_if <blang expression>)`` specifies the boolean condition that must
  be true for the tests to run. The condition is specified using the :ref:`blang`, and
  the field allows for :ref:`variables` to appear in the expressions.

The typical use of the ``alias`` stanza is to define tests:

.. code:: lisp

    (rule
     (alias   runtest)
     (action (run %{exe:my-test-program.exe} blah)))

See the section about :ref:`running-tests` for details.

Note that if your project contains several packages and you run the tests
from the opam file using a ``build-test`` field, then all your ``runtest`` alias
stanzas should have a ``(package ...)`` field in order to partition the set of
tests.

.. _install:

install
-------

Dune supports installing packages on the system, i.e. copying freshly built
artifacts from the workspace to the system. The ``install`` stanza takes three
pieces of information:

- the list of files to install
- the package to attach these files to. This field is optional if your
  project contains a single package
- the section in which the files will be installed

For instance:

.. code::

   (install
    (files hello.txt)
    (section share)
    (package mypackage))

Indicate that the file ``hello.txt`` in the current directory is to be
installed in ``<prefix>/share/mypackage``.

The following sections are available:

- ``lib`` installs to ``<prefix>/lib/<pkgname>/``
- ``lib_root`` installs to ``<prefix>/lib/``
- ``libexec`` installs to ``<prefix>/lib/<pkgname>/`` with the
  executable bit set
- ``libexec_root`` installs to ``<prefix>/lib/`` with the executable
  bit set
- ``bin`` installs to ``<prefix>/bin/`` with the executable bit set
- ``sbin`` installs to ``<prefix>/sbin/`` with the executable bit set
- ``toplevel`` installs to ``<prefix>/lib/toplevel/``
- ``share`` installs to ``<prefix>/share/<pkgname>/``
- ``share_root`` installs to ``<prefix>/share/``
- ``etc`` installs to ``<prefix>/etc/<pkgname>/``
- ``etc_root`` installs to ``<prefix>/etc/``
- ``doc`` installs to ``<prefix>/doc/<pkgname>/``
- ``stublibs`` installs to ``<prefix>/lib/stublibs/`` with the
  executable bit set
- ``man`` installs relative to ``<prefix>/man`` with the destination
  directory extracted from the extension of the source file (so that
  installing ``foo.1`` is equivalent to a destination of
  ``man1/foo.1``)
- ``misc`` requires files to specify an absolute destination, and the
  user will be prompted before the installation when it is done via
  opam. Only use this for advanced cases.
- ``(site (<package> <site>))`` install in the ``<site>`` directory of
  ``<package>``. If the prefix is not the same than the one used when installing
  ``<package>``, ``<package>`` will not find the files.

Normally, Dune uses the basename of the file to install to determine
the name of the file once installed.  However, you can change that
fact by using the form ``(<filename> as <destination>)`` in the
``files`` field. For instance, to install a file ``mylib.el`` as
``<prefix>/share/emacs/site-lisp/mylib.el`` you must write the following:

.. code:: scheme

    (install
     (section share_root)
     (files   (mylib.el as emacs/site-lisp/mylib.el)))


Handling of the .exe extension on Windows
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Under Microsoft Windows, executables must be suffixed with
``.exe``. Dune tries to make sure that executables are always
installed with this extension on Windows.

More precisely, when installing a file via an ``(install ...)``
stanza, if the source file has extension ``.exe`` or ``.bc``, then
dune implicitly adds the ``.exe`` extension to the destination, if
not already present.

copy_files
----------

The ``copy_files`` and ``copy_files#`` stanzas allow to specify that
files from another directory could be copied if needed to the current
directory.

The syntax is as follows:

.. code:: scheme

    (copy_files
     <optional-fields>
     (files <glob>))

``<glob>`` represents the set of files to copy, see the :ref:`glob
<glob>` for details.

``<optional-fields>`` are:

- ``(alias <alias-name>)`` to specify an alias to which to attach the targets.

- ``(mode <mode>)`` to specify how to handle the targets, see `modes`_
  for details.

- ``(enabled_if <blang expression>)`` conditionally disables this stanza. The
  condition is specified using the :ref:`blang`.

The short form

.. code:: scheme

    (copy_files <glob>)

is equivalent to

.. code:: scheme

    (copy_files (files <glob>))

The difference between ``copy_files`` and ``copy_files#`` is the same
as the difference between the ``copy`` and ``copy#`` action. See the
:ref:`user-actions` section for more details.

include
-------

The ``include`` stanza allows including the contents of another file in the
current dune file. Currently, the included file cannot be generated and must be
present in the source tree. This feature is intended to be used in conjunction
with promotion, when parts of a dune file are to be generated.

For instance:

.. code:: scheme

    (include dune.inc)

    (rule (with-stdout-to dune.inc.gen (run ./gen-dune.exe)))

    (rule
     (alias  runtest)
     (action (diff dune.inc dune.inc.gen)))

With this dune file, running dune as follows will replace the
``dune.inc`` file in the source tree by the generated one:

.. code:: shell

    $ dune build @runtest --auto-promote

.. _tests-stanza:

tests
-----

The ``tests`` stanza allows one to easily define multiple tests. For example we
can define two tests at once with:

.. code:: scheme

   (tests
    (names mytest expect_test)
    <optional fields>)

This will define an executable named ``mytest.exe`` that will be executed as
part of the ``runtest`` alias. If the directory also contains an
``expect_test.expected`` file, then ``expect_test`` will be used to define an
expect test. That is, the test will be executed and its output will be compared
to ``expect_test.expected``.

The optional fields that are supported are a subset of the alias and executables
fields. In particular, all fields except for ``public_names`` are supported from
the :ref:`executables stanza <shared-exe-fields>`. Alias fields apart from
``name`` are allowed.

By default the test binaries are run without options.  The ``action`` field can
be used to override the test binary invocation, for example if you're using
alcotest and wish to see all the test failures on the standard output when
running dune runtest you can use the following stanza:

.. code:: lisp

   (tests
    (names mytest)
    (libraries alcotest mylib)
    (action (run %{test} -e)))

Starting from dune 2.9, it's possible to automatically generate empty interface
files for test executables. See `executables_implicit_empty_intf`_.

test
----

The ``test`` stanza is the singular form of ``tests``. The only difference is
that it's of the form:

.. code:: scheme

   (test
    (name foo)
    <optional fields>)

where the ``name`` field is singular. The same optional fields are supported.

.. _dune-env:

env
---

The ``env`` stanza allows one to modify the environment. The syntax is as
follow:

.. code:: scheme

     (env
      (<profile1> <settings1>)
      (<profile2> <settings2>)
      ...
      (<profilen> <settingsn>))

The first form ``(<profile> <settings>)`` that correspond to the
selected build profile will be used to modify the environment in this
directory. You can use ``_`` to match any build profile.

Fields supported in ``<settings>`` are:

- any OCaml flags field, see :ref:`ocaml-flags` for more details.

- ``(c_flags <flags>)`` and ``(cxx_flags <flags>)``
  to specify compilation flags for C and C++ stubs, respectively.
  See `library`_ for more details.

- ``(env-vars (<var1> <val1>) .. (<varN> <valN>))``. This will add the
  corresponding variables to the environment in which the build commands are
  executed, and under which ``dune exec`` runs.

- ``(menhir_flags <flags>))`` to specify flags for menhir stanzas.

- ``(binaries <binaries>)`` where ``<binaries>`` is a list of entries
  of the form ``(<filepath> as <name>)``. ``(<filepath> as <name>)``
  makes the binary ``<filepath>`` available in the command search as
  just ``<name>``. For instance in a ``(run <name> ...)`` action
  ``<name>`` will resolve to this file path. You can also write just
  the file path, in which case the name will be inferred from the
  basename of ``<filepath>`` by dropping the ``.exe`` suffix if it
  exists. For instance ``(binaries bin/foo.exe (bin/main.exe as
  bar))`` would add the commands ``foo`` and ``bar`` to the search
  path.

- ``(inline_tests <state>)`` where state is either ``enabled``, ``disabled`` or
  ``ignored``. This field is available since Dune 1.11. It controls the value
  of the variable ``%{inline_tests}`` that is read by the inline test framework.
  The default value is ``disabled`` for the ``release`` profile and ``enabled``
  otherwise.

- ``(odoc <fields>)``. This allows to pass options to Odoc, see
  :ref:`odoc-options` for more details.

- ``(coq (flags <flags>))``. This allows to pass options to Coq, see
  :ref:`coq-theory` for more details.

- ``(formatting <settings>)``. This allows to set auto-formatting in the current
  directory subtree, see :ref:`formatting`.

.. _dune-subdirs:

dirs (since 1.6)
-------------------

The ``dirs`` stanza allows specifying the sub-directories dune will
include in a build. The syntax is based on dune's :ref:`predicate-lang` and allows
the user the following operations:

- The special value ``:standard`` which refers to the default set of used
  directories. These are the directories that don't start with ``.`` or ``_``.

- Set operations. Differences are expressed with backslash: ``* \ bar``, unions
  are done by listing multiple items.

- Sets can be defined using globs.

Examples:

.. code:: lisp

   (dirs *) ;; include all directories
   (dirs :standard \ ocaml) ;; include all directories except ocaml
   (dirs :standard \ test* foo*) ;; exclude all directories that start with test or foo

A directory that is not included by this stanza will not be eagerly scanned by
Dune. Any ``dune`` or other special files in it won't be interpreted either and
will be treated as raw data. It is however possible to depend on files inside
ignored sub-directories.

.. _dune-data_only_dirs:

data_only_dirs (since 1.6)
--------------------------

Dune allows the user to treat directories as *data only*. Dune files in these
directories will not be evaluated for their rules, but the contents of these
directories will still be usable as dependencies for other rules.

The syntax is the same as for the ``dirs`` stanza except that ``:standard``
is by default empty.

Example:

.. code:: scheme

   ;; dune files in fixtures_* dirs are ignored
   (data_only_dirs fixtures_*)

.. _dune-ignored_subdirs:

ignored_subdirs (deprecated in 1.6)
-----------------------------------

One may also specify *data only* directories using the ``ignored_subdirs``
stanza. The meaning is the same as ``data_only_dirs`` but the syntax isn't as
flexible and only accepts a list of directory names. It is advised to switch to
the new ``data_only_dirs`` stanza.

Example:

.. code:: scheme

     (ignored_subdirs (<sub-dir1> <sub-dir2> ...))

All of the specified ``<sub-dirn>`` will be ignored by dune. Note that users
should rely on the ``dirs`` stanza along with the appropriate set operations
instead of this stanza. For example:

.. code:: lisp

  (dirs :standard \ <sub-dir1> <sub-dir2> ...)

.. _dune-vendored_dirs:

vendored_dirs (since 1.11)
--------------------------

Dune supports vendoring of other dune-based projects natively since simply
copying a project into a subdirectory of your own project will work. Simply
doing that has a few limitations though. You can workaround those by explicitly
marking such directories as containing vendored code.

Example:

.. code:: scheme

   (vendored_dirs vendor)


Dune will not resolve aliases in vendored directories meaning by default it will
not build all installable targets, run the test, format or lint the code located
in such a directory while still building the parts your project depend upon.
Libraries and executable in vendored directories will also be built with a ``-w
-a`` flag to suppress all warnings and prevent pollution of your build output.


.. _include_subdirs:

include_subdirs
---------------

The ``include_subdirs`` stanza is used to control how dune considers
sub-directories of the current directory. The syntax is as follows:

.. code:: scheme

     (include_subdirs <mode>)

Where ``<mode>`` maybe be one of:

- ``no``, the default
- ``unqualified``

When the ``include_subdirs`` stanza is not present or ``<mode>`` is
``no``, dune considers sub-directories as independent. When ``<mode>``
is ``unqualified``, dune will assume that the sub-directories of the
current directory are part of the same group of directories. In
particular, dune will scan all these directories at once when looking
for OCaml/Reason files. This allows you to split a library between
several directories. ``unqualified`` means that modules in
sub-directories are seen as if they were all in the same directory. In
particular, you cannot have two modules with the same name in two
different directories. It is planned to add a ``qualified`` mode in
the future.

Note that sub-directories are included recursively, however the
recursion will stop when encountering a sub-directory that contains
another ``include_subdirs`` stanza. Additionally, it is not allowed
for a sub-directory of a directory with ``(include_subdirs <x>)``
where ``<x>`` is not ``no`` to contain one of the following stanzas:

- ``library``
- ``executable(s)``
- ``test(s)``

toplevel
--------

The ``toplevel`` stanza allows one to define custom toplevels. Custom toplevels
automatically load a set of specified libraries and are runnable like normal
executables. Example:

.. code:: scheme

   (toplevel
    (name tt)
    (libraries str))

This will create a toplevel with the ``str`` library loaded. We may build and
run this toplevel with:

.. code:: shell

   $ dune exec ./tt.exe

``(preprocess (pps ...))`` is the same as the ``(preprocess (pps ...))`` field
of `library`_. Currently, ``action`` and ``future_syntax`` are not supported
in the toplevel.

subdir
------

The ``subdir`` stanza can be used to evaluate stanzas in sub directories. This is
useful for generated files or to override stanzas in vendored directories
without editing vendored dune files.

In this example, a ``bar`` target is created in the ``foo`` directory, and a bar
target will be created in ``a/b/bar``:

.. code:: scheme

   (subdir foo (rule (with-stdout-to bar (echo baz))))
   (subdir a/b (rule (with-stdout-to bar (echo baz))))

external_variant
-----------------

This stanza was experimental and removed in dune 2.6. see :ref:`dune-variants`

.. _coq-theory:

coq.theory
----------

Dune is also able to build Coq developments. A Coq project is a mix of
Coq ``.v`` files and (optionally) OCaml libraries linking to the Coq
API (in which case we say the project is a *Coq plugin*). To enable
Coq support in a dune project, the language version should be selected
in the ``dune-project`` file. For example:

.. code:: scheme

    (using coq 0.2)

This will enable support for the ``coq.theory`` stanza in the current project. If the
language version is absent, dune will automatically add this line with the
latest Coq version to the project file once a ``(coq.theory ...)`` stanza is used anywhere.

The supported Coq language versions are:

- ``0.1``: basic Coq theory support,
- ``0.2``: support for the ``theories`` field, and composition of theories in the same scope,
- ``0.3``: support for ``(mode native)``, requires Coq >= 8.10.

Guarantees with respect to stability are not provided yet,
however, as implementation of features progresses, we hope to reach
``1.0`` soon. The ``1.0`` version will commit to a stable set of
functionality; all the features below are expected to reach ``1.0``
unchanged or minimally modified.

The basic form for defining Coq libraries is very similar to the OCaml form:

.. code:: scheme

    (coq.theory
     (name <module_prefix>)
     (package <package>)
     (synopsis <text>)
     (modules <ordered_set_lang>)
     (libraries <ocaml_libraries>)
     (flags <coq_flags>)
     (mode <coq_native_mode>)
     (theories <coq_theories>))

The stanza will build all ``.v`` files on the given directory. The semantics of fields is:

- ``<module_prefix>`` is a dot-separated list of valid Coq module
  names and determines the module scope under which the theory is
  compiled [``-R`` option]. For example, if ``<module_prefix>`` is
  ``foo.Bar``, the theory modules will be named as
  ``foo.Bar.module1``, ``foo.Bar.module2``, etc... Note that modules
  in the same theory don't see the ``foo.Bar`` prefix, in the same
  way that OCaml ``wrapped`` libraries do. For compatibility reasons,
  the 1.0 version of the Coq language installs a theory named
  ``foo.Bar`` under ``foo/Bar``. Also note that Coq supports composing
  a module path from different theories, thus you can name a theory
  ``foo.Bar`` and a second one ``foo.Baz`` and things will work
  properly,
- the ``modules`` field enables constraining the set of modules
  included in the theory, similarly to its OCaml counterpart. Modules
  are specified in Coq notation, that is to say ``A/b.v`` is written
  ``A.b`` in this field,
- if ``package`` is present, Dune will generate install rules for the
  ``.vo`` files on the theory. ``pkg_name`` must be a valid package
  name. Note that the 1.0 version of the language uses the Coq legacy
  install setup, where all packages share a common root namespace and
  install directory, ``lib/coq/user-contrib/<module_prefix>``, as
  customary in the make-based Coq package ecosystem. For
  compatibility, we also install under the ``user-contrib`` prefix the
  ``.cmxs`` files appearing in ``<ocaml_libraries>``,
- ``<coq_flags>`` will be passed to ``coqc`` as command-line
  options. ``:standard`` is taken from the value set in the ``(coq (flags <flags>))``
  field in ``env`` profile. See :ref:`dune-env` for more information.
- the path to installed locations of ``<ocaml_libraries>`` will be passed to
  ``coqdep`` and ``coqc`` using Coq's ``-I`` flag; this allows for a Coq
  theory to depend on a ML plugin,
- your Coq theory can depend on other theories by specifying them in
  the ``<coq_theories>`` field. Dune will then pass to Coq the
  corresponding flags for everything to compile correctly [ ``-Q``
  ]. As of today, we only support composition with libraries defined
  in the same scope (that is to say, under the same ``dune-project``
  domain). We will lift this restriction in the future. Note that
  composition with the Coq's standard library is supported, but in
  this case the ``Coq`` prefix will be made available in a qualified
  way. Since Coq's lang version ``0.2``.
- you can enable the production of Coq's native compiler object files
  by setting ``<coq_native_mode>`` to ``native``, this will pass
  ``-native-compiler on`` to Coq and install the corresponding object
  files under ``.coq-native`` when in ``release`` profile. The regular
  ``dev`` profile will skip native compilation to make the build
  faster. Since Coq's lang version ``0.3``. Note that the support for
  native compute is **experimental**, and requires Coq >= 8.12.1;
  moreover, depending libraries *must* be built with ``(mode native)``
  too for this to work; also Coq must be configured to support native
  compilation. Note that Dune will explicitly disable output of native
  compilation objects when ``(mode vo)`` even if the default Coq's
  configure flag enabled it. This will be improved in the future.

Recursive qualification of modules
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

If you add:

.. code:: scheme

    (include_subdirs qualified)

to a ``dune`` file, Dune will consider all the modules in the
directory and its sub-directories, adding a prefix to the module name in the usual
Coq style for sub-directories. For example, file ``A/b/C.v`` will be module
``A.b.C``.

Limitations
~~~~~~~~~~~

- ``.v`` files always depend on the native version of Coq / plugins,
- a ``foo.mlpack`` file must the present in directories of locally
  defined plugins for things to work, this is a limitation of
  ``coqdep``, see the template at
  <https://github.com/ejgallego/coq-plugin-template>

coq.pp
------

Coq plugin writers usually need to write ``.mlg`` files to extend Coq
grammar. Such files are pre-processed with `coqpp`; to help plugin
writers avoid boilerplate we provide a `(coqpp ...)` stanza:

.. code:: scheme

    (coq.pp (modules <mlg_list>))

which for each ``g_mod`` in ``<mlg_list>`` is equivalent to:

.. code:: lisp

    (rule
     (targets g_mod.ml)
     (deps (:mlg-file g_mod.mlg))
     (action (run coqpp %{mlg-file})))

coq.extraction
--------------

Coq may be instructed to *extract* OCaml sources as part of the compilation
process. This is done using the ``coq.extraction`` stanza:

.. code:: lisp

   (coq.extraction
    (prelude <name>)
    (extracted_modules <names>)
    <optional-fields>)

- ``(prelude <name>)`` refers to the Coq source that contains the extraction
  commands.

- ``(extracted_modules <names>)`` is an exhaustive list of OCaml modules
  extracted.

- ``<optional-fields>`` are ``flags``, ``theories``, and ``libraries``. All of
  these fields have the same meaning as in the ``coq.theory`` stanza.

The extracted sources can then be used in ``executable`` or ``library`` stanzas
as any other sources.

Note that the sources are extracted to the directory where the
``prelude`` file is; thus the common placement for the ``OCaml``
stanzas is in the same ``dune`` file. **warning** using Coq's ``Cd``
command to workaround problems with the output directory is not
allowed when using extraction from Dune; moreover the ``Cd`` command
will be deprecated in Coq 8.12.

mdx (since 2.4)
---------------

MDX is a tool that helps you keep your markdown documentation up to date by
checking that the code examples it contains are correct. When setting an MDX
stanza, the checks carried out by MDX are automatically attached to the
``runtest`` alias of the stanza's directory.

See `MDX's repository <https://github.com/realworldocaml/mdx>`__ for more details.

You can define an MDX stanza to specify which files you want checked.

Note that this feature is still experimental and needs to be enabled in your
``dune-project`` with the following ``using`` stanza:

.. code:: scheme

  (using mdx 0.1)

The syntax is as follows:

.. code:: scheme

  (mdx <optional-fields>)

Where ``<optional-fields>`` are:

- ``(files <globs>)`` are the files that you want MDX to check, described as a
  list of globs (see the :ref:`Glob language specification <glob>` ).
  It defaults to ``*.md``.

- ``(packages <packages>)`` are the local dune packages that your documentation
  code blocks depend on. I.e. if your documentation examples depend on a public
  executable or library defined from a local package, it has to be specified in
  the stanza.

- ``(preludes <files>)`` are the prelude files you want to pass to MDX.
  See `MDX's documentation <https://github.com/realworldocaml/mdx>`__ for more
  details on preludes.

.. _plugin:

plugin (since 2.8)
------------------

Plugins are a way to load ocaml libraries at runtime. The ``plugin`` stanza
allows to declare the name of the plugin, in which :ref:`sites` it should be
present, and which libraries it will load.

.. code:: lisp

   (plugin
    (name <name>)
    (libraries <libaries>)
    (site (<package> <site name>))
    (<optional-fields>))

``<optional-fields>`` are:

- ``(package <package>)`` if there is more than one package defined in the
  current scope, this specifies during the installation of which package the
  plugin will be installed. A plugin can be installed by one package in the site
  of another package.

- ``(optional)`` will not declare the plugin if the libraries are not available

The loading of the plugin is done using the facilities generated by
:ref:`generate_sites_module`

.. _generate_sites_module:

generate_sites_module (since 2.8)
---------------------------------

Dune proposes some facilities for dealing with :ref:`sites` in a program. The
``generate_sites_module`` stanza will generate code for looking up the correct locations
of the sites directories and for loading plugins. It works after installation
with or without the relocation mode, inside dune rules, when using dune exec.
For promotion it works only if the generated modules are only in the executable (or
library statically linked) promoted; generated modules in plugins will not work.

.. code:: lisp

   (generate_sites_module
    (module <name>)
    <facilities>)

The code of the module is generated in the directory with the given name. The
code is populated according to the requested facilities.


The available ``<facilities>`` are:

- ``sourceroot`` : adds in the generated module a value ``val sourceroot: string option``
  which contains the value of ``%{workspace_root}`` if the code have been built
  locally. It could be used to keep configuration file of the tool locally when
  executed with ``dune exec`` or after promotion. The value is ``None`` once it has been installed.

- ``relocatable`` : adds in the generated module a value ``val relocatable: bool``
  which indicates if the binary has been installed in the relocatable mode

- ``(sites <package>)`` : adds in the sub-module `Sites` of the generated module a value
  ``val <site>: string list`` for each ``<site>`` of ``<package>``. The
  identifier <site> is uncapitalized.

- ``(plugins (<package> <site>) ...)``: adds in the sub-module ``Plugins`` of the
  generated module a sub-module ``<site>`` with the following signature ``S``. The
  identifier ``<site>`` is capitalized.

.. code:: ocaml

   module type S = sig
     val paths: string list
     (** return the locations of the directory containing the plugins *)

     val list: unit -> string list
     (** return the list of available plugins *)

     val load_all: unit -> unit
     (** load all the plugins and their dependencies *)

     val load: string -> unit
     (** load the specified plugin and its dependencies *)
   end

The generated module as a dependency on the library ``dune-site``,
and if the facilities ``(plugins ...)`` is used, it as a dependency on the library
``dune-site.plugins``. Those dependencies are not automatically added
to the library or executable which use the module (cf. :ref:`plugins`).

.. _dune-workspace:

dune-workspace
==============

By default, a workspace has only one build context named ``default`` which
correspond to the environment in which ``dune`` is run. You can define more
contexts by writing a ``dune-workspace`` file.

You can point ``dune`` to an explicit ``dune-workspace`` file with the
``--workspace`` option. For instance it is good practice to write a
``dune-workspace.dev`` in your project with all the version of OCaml your
projects support. This way developers can tests that the code builds with all
version of OCaml by simply running:

.. code:: bash

    $ dune build --workspace dune-workspace.dev @all @runtest

The ``dune-workspace`` file uses the S-expression syntax. This is what
a typical ``dune-workspace`` file looks like:

.. code:: scheme

    (lang dune 3.0)
    (context (opam (switch 4.07.1)))
    (context (opam (switch 4.08.1)))
    (context (opam (switch 4.11.1)))

The rest of this section describe the stanzas available.

Note that an empty ``dune-workspace`` file is interpreted the same as one
containing exactly:

.. code:: scheme

    (lang dune 3.0)
    (context default)

This allows you to use an empty ``dune-workspace`` file to mark the root of your
project.

profile
-------

The build profile can be selected in the ``dune-workspace`` file by write a
``(profile ...)`` stanza. For instance:

.. code:: scheme

    (profile release)

Note that the command line option ``--profile`` has precedence over this stanza.

env
---

The ``env`` stanza can be used to set the base environment for all contexts in
this workspace. This environment has the lowest precedence of all other ``env``
stanzas. The syntax for this stanza is the same dune's :ref:`dune-env` stanza.

context
-------

The ``(context ...)`` stanza declares a build context. The argument
can be either ``default`` or ``(default)`` for the default build
context or can be the description of an opam switch, as follows:

.. code:: scheme

    (context (opam (switch <opam-switch-name>)
                   <optional-fields>))

``<optional-fields>`` are:

-  ``(name <name>)`` is the name of the subdirectory of ``_build``
   where the artifacts for this build context will be stored.

-  ``(root <opam-root>)`` is the opam root. By default it will take
   the opam root defined by the environment in which ``dune`` is
   run which is usually ``~/.opam``.

- ``(merlin)`` instructs dune to use this build context for
  merlin.

- ``(profile <profile>)`` to set a different profile for a build
  context. This has precedence over the command line option
  ``--profile``.

- ``(env <env>)`` to set the environment for a particular context. This is of
  higher precedence than the root ``env`` stanza in the workspace file. This
  field the same options as the :ref:`dune-env` stanza.

- ``(toolchain <findlib_toolchain>)`` set findlib toolchain for the context.

- ``(host <host_context>)`` choose a different context to build binaries that
  are meant to be executed on the host machine, such as preprocessors.

- ``(paths (<var1> <val1>) .. (<varN> <valN>))`` allows setting the value of any
  ``PATH``-like variables in this context. If ``PATH`` itself is modified in
  this way, its value will be used to resolve binaries in the workspace,
  including finding the compiler and related tools. These variables will also be
  passed as part of the environment to any program launched by ``dune``. For
  each variable, the value is specified using the :ref:`ordered-set-language`.
  Relative paths are interpreted with respect to the workspace root, see
  :ref:`finding-root`.

- ``(fdo <target_exe>)`` build this context with feedback-direct
  optimizations. Requires `OCamlFDO
  <https://github.com/gretay-js/ocamlfdo>`__. ``<target_exe>`` is a
  path interpreted relative to the workspace root, see
  :ref:`finding-root`. ``<target_exe>`` specifies which executable to
  optimize. Users should define a different context for each target
  executable built with FDO. The name of the context is derived
  automatically from the default name and ``<target-exe>``, unless
  explicitly specified using ``(name ...)`` field.  For example, if
  ``<target_exe>`` is *src/foo.exe* in a default context, then the
  name of the context is *default-fdo-foo* and the name of the file
  that contains execution counters is *src/fdo.exe.fdo-profile*.  This
  feature is **experimental** and no backwards compatibility is
  implied.

- By default Dune builds and installs dynamically linked foreign
  archives (usually named ``dll*.so``). It is possible to disable
  this by setting
  ``(disable_dynamically_linked_foreign_archives true)`` in the
  workspace file, in which case bytecode executables will be built
  with all foreign archives statically linked into the runtime system.


Both ``(default ...)`` and ``(opam ...)`` accept a ``targets`` field in order to
setup cross compilation. See :ref:`cross-compilation` for more
information.

Merlin reads compilation artifacts and it can only read the compilation
artifacts of a single context. Usually, you should use the artifacts from the
``default`` context, and if you have the ``(context default)`` stanza in your
``dune-workspace`` file, that is the one dune will use.

For rare cases where this is not what you want, you can force dune to use a
different build contexts for merlin by adding the field ``(merlin)`` to this
context.
