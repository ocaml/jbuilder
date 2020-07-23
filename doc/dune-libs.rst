**************
Dune libraries
**************

.. _configurator:

Configurator
============

Configurator is a small library designed to query features available on the
system, in order to generate configuration for dune builds. Such generated
configuration is usually in the form of command line flags, generated headers,
stubs, but there are no limitations on this.

Configurator allows you to query for the following features:

* Variables defined in ``ocamlc -config``,

* pkg-config_ flags for packages,

* Test features by compiling C code,

* Extract compile time information such as ``#define`` variables.

Configurator is designed to be cross compilation friendly and avoids _running_
any compiled code to extract any of the information above.

Configurator started as an `independent library
<https://github.com/janestreet/configurator>`__, but now lives in dune. You do
not need to install anything to use configurator.

Usage
-----

We'll describe configurator with a simple example. Everything else can be easily
learned by studying `configurator's API
<https://github.com/ocaml/dune/blob/master/otherlibs/configurator/src/v1.mli>`__.

To use configurator, we write an executable that will query the system using
configurator's API and output a set of targets reflecting the results. For
example:

.. code-block:: ocaml

  module C = Configurator.V1

  let clock_gettime_code = {|
  #include <time.h>

  int main()
  {
    struct timespec ts;
    clock_gettime(CLOCK_REALTIME, &ts);
    return 0;
  }
  |}

  let () =
    C.main ~name:"foo" (fun c ->
      let has_clock_gettime = C.c_test c clock_gettime_code ~link_flags:["-lrt"] in

      C.C_define.gen_header_file c ~fname:"config.h"
        [ "HAS_CLOCK_GETTIME", Switch has_clock_gettime ]);

Usually, the module above would be named ``discover.ml``. The next step is to
invoke it as an executable and tell dune about the targets that it produces:

.. code-block:: lisp

  (executable
   (name discover)
   (libraries dune.configurator))

  (rule
   (targets config.h)
   (action (run ./discover.exe)))

Another common pattern is to produce a flags file with configurator and then use
this flag file using ``:include``:

.. code-block:: lisp

  (library
   (name mylib)
   (foreign_stubs (language c) (names foo))
   (c_library_flags (:include (flags.sexp))))

For this, generate the list of flags for your library — for example
using ``Configurator.V1.Pkg_config`` — and then write them to a file,
in the above example ``flags.sexp``, with
``Configurator.V1.write_flags "flags.sexp" flags``.

Upgrading from the old Configurator
-----------------------------------

The old configurator is the independent `configurator
<https://github.com/janestreet/configurator>`__ opam package. It is deprecated
and users are encouraged to migrate to dune's own configurator. The advantage of
the transition include:

* No extra dependencies,

* No need to manually pass ``-ocamlc`` flag,

* New configurator is cross compilation compatible.

The following steps must be taken to transition from the old configurator:

* Mentions of the ``configurator`` opam package should be removed.

* The library name ``configurator`` should be changed ``dune.configurator``.

* The ``-ocamlc`` flag in rules that run configurator scripts should be removed.
  This information is now passed automatically by dune.

* The new configurator API is versioned explicitly. The version that is
  compatible with old configurator is under the ``V1`` module. Hence, to
  transition one's code it's enough to add this module alias:

.. code-block:: ocaml

   module Configurator = Configurator.V1

.. _pkg-config: https://www.freedesktop.org/wiki/Software/pkg-config/

.. _build-info:

build-info
==========

Dune can embed build information such as versions in executables
via the special ``dune-build-info`` library. This library exposes
some information about how the executable was built such as the
version of the project containing the executable or the list of
statically linked libraries with their versions. Printing the version
at which the current executable was built is as simple as:

.. code:: ocaml

          Printf.printf "version: %s\n"
            (match Build_info.V1.version () with
             | None -> "n/a"
             | Some v -> Build_info.V1.Version.to_string v)

For libraries and executables from development repositories that don't
have version information written directly in the ``dune-project``
file, the version is obtained by querying the version control
system. For instance, the following git command is used in git
repositories:

.. code:: bash

          git describe --always --dirty

which produces a human readable version string of the form
``<version>-<commits-since-version>-<hash>[-dirty]``.

Note that in the case where the version string is obtained from the
version control system, the version string will only be written in
the binary once it is installed or promoted to the source tree. In
particular, if you evaluate this expression as part of the build of
your package, it will return ``None``. This is to ensure that
committing does not hurt your development experience. Indeed, if dune
stored the version directly inside the freshly built binaries, then
every time you commit your code the version would change and dune would
need to rebuild all the binaries and everything that depend on them,
such as tests. Instead Dune leaves a placeholder inside the binary and
fills it during installation or promotion.

.. _custom-build-info:

custom-build-info
-----------------

Since dune 2.7 it is possible to extend Dune's default build information using
the ``(custom-build-info)`` field in the libraries and executables stanzas:

.. code-block:: lisp

  (custom_build_info
   (max_size <int>)
   (action <action>))

``<action>`` is the action used to produce the desired information during
link. This action should print this information on it's standard output. See the
:ref:`user-actions` section for more details. (Paths are resolved relatively to
the directory of the executable being linked.)

``<int>`` is the maximum size of the action's output. Bigger outputs will be
truncated. This is necessary because the placeholder mechanism described in
:ref:`build-info` is used to write the information in the binary only when it
is installed or promoted to the source tree.

It is possible to retrieve the information using ``Build_info.V2.custom :
unit -> string option`` for executables and ``Build_info.V2.custom_lib : string
-> string option`` for libraries, where the argument is the name of the library.

For example, given the following configuration:

.. code-block:: shell

  $ cat dune
  (executable
   (public_name foo)
   (custom_build_info
    (max_size 46)
    (action (echo "Custom build info generated during link (exe).")))

  $ cat lib/dune
  (library
   (public_name mylib)
   (custom_build_info
    (max_size 48)
    (action (echo "Custom build info generated during link (mylib).")))

The custom built informations can be access from whether `Foo` or `Mylib` as follows:

.. code-block:: ocaml

  let info_mylib = Build_info.V2.custom_lib "mylib" in
  match info_mylib with
  | Some info -> Printf.printf "From mylib: %s\n" info
  | None      -> Printf.printf "From mylib: n/a\n"

  let info_foo = Build_info.V2.custom in
  match info_foo with
  | Some info -> Printf.printf "From foo: %s\n" info
  | None      -> Printf.printf "From foo: n/a\n"


Note that even if this system is restricted to a single string,
structured data can also be transmitted using an appropriate
marshalling system.

(Experimental) Dune action plugin
=================================

*This library is experimental and no backwards compatibility is implied. Use at
your own risk.*

``Dune-action-plugin`` provides a monadic interface to express program
dependencies directly inside the source code. Programs using this feature
should be declared using ``dynamic-run`` construction instead of usual ``run``.
