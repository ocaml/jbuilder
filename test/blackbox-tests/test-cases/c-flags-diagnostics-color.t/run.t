Check that -fdiagnostics-color=always is added to :standard flag set
and the :standard set only if and only if
- Dune is running in a tty;
- the compiler is GCC or Clang;
- use_standard_c_and_cxx_flags is true.

The compiler is detected using the command name, that may not be most
portable way.

  $ O_CC=$(ocamlc -config-var c_compiler)

We also have to trick Dune into believing it's executed in a tty,
otherwise it won't output colors!

  $ function cc_supported() {
  >   if [[ "${O_CC}" == *gcc || "${O_CC}" = "clang" ]]; then
  >     echo "1";
  >   else echo "0"; fi }

  $ function env_isatty() {
  >   local b=$1; shift;
  >   if [[ "$(cc_supported)" = 1 ]]; then
  >     echo "int isatty(int fd) { return $b; }" |
  >       $(ocamlc -config-var native_c_compiler) -shared -ldl -o isatty-$b.so -xc -;
  >     env										\
  >       TERM=not_dumb									\
  >       "DYLD_INSERT_LIBRARIES=$PWD/isatty-$b.so"					\
  >       DYLD_FORCE_FLAT_NAMESPACE=y							\
  >       "LD_PRELOAD=$PWD/isatty-$b.so" 						\
  >       "$@";
  >   else
  >     env "$@";
  >   fi
  > }

tty /\ not (:standard) => not (-fdiagnostics-color)
==================================

The flag shouldn't be present in the command, as we only want it in
the :standard set.

  $ cat >dune-project <<EOF
  > (lang dune 2.8)
  > (use_standard_c_and_cxx_flags false)
  > EOF
  $ cat >dune <<EOF
  > (library
  >  (name test)
  >  (foreign_stubs (language c) (names stub)))
  > EOF

  $ env_isatty 1 dune rules -m stub.o | tr -s '\t\n\\' ' ' > out_stub

  $ grep -ce "-fdiagnostics-color=always" out_stub
  0
  [1]

tty /\ :standard => -fdiagnostics-color
==================================

The flag should be present in the command, as we want it in the
:standard set.

  $ cat >dune-project <<EOF
  > (lang dune 2.8)
  > (use_standard_c_and_cxx_flags true)
  > EOF
  $ cat >dune <<EOF
  > (library
  >  (name test)
  >  (foreign_stubs (language c) (names stub)))
  > EOF

  $ env_isatty 1 dune rules -m stub.o | tr -s '\t\n\\' ' ' > out_stub

  $ out=$(grep -ce "-fdiagnostics-color=always" out_stub);
  > if [[ "$(cc_supported)" = 1 ]]; then
  >   echo $out;
  > elif [ $out = 0 ]; then
  >   echo 1;
  > else
  >   echo 0;
  > fi
  1

tty /\ :standard /\ not (-fdiagnostics-color) => not (-fdiagnostics-color)
==================================

If the flag is disabled, it should never appear in the command line.

  $ cat >dune-project <<EOF
  > (lang dune 2.8)
  > (use_standard_c_and_cxx_flags true)
  > EOF
  $ cat >dune <<EOF
  > (library
  >  (name test)
  >  (foreign_stubs (language c) (names stub)
  >   (flags :standard \ -fdiagnostics-color=always)))
  > EOF

  $ env_isatty 1 dune rules -m stub.o | tr -s '\t\n\\' ' ' > out_stub

  $ grep -ce "-fdiagnostics-color=always" out_stub
  0
  [1]

not(tty) /\ :standard => not (-fdiagnostics-color)
==================================

If not running in a tty, the flag should not be present.

  $ cat >dune-project <<EOF
  > (lang dune 2.8)
  > (use_standard_c_and_cxx_flags true)
  > EOF
  $ cat >dune <<EOF
  > (library
  >  (name test)
  >  (foreign_stubs (language c) (names stub)))
  > EOF

  $ env_isatty 0 dune rules -m stub.o > out_stub

  $ out=$(grep -ce "-fdiagnostics-color=always" out_stub);
  > if [[ "$(cc_supported)" = 1 ]]; then
  >   echo $out;
  > elif [ $out = 0 ]; then
  >   echo 1;
  > else
  >   echo 0;
  > fi
  0

tty /\ :standard overridden => not (-fdiagnostics-color)
==================================

If the flag is disabled, it should never appear in the command line.

  $ cat >dune-project <<EOF
  > (lang dune 2.8)
  > (use_standard_c_and_cxx_flags true)
  > EOF
  $ cat >dune <<EOF
  > (library
  >  (name test)
  >  (foreign_stubs (language c) (names stub) (flags)))
  > EOF

  $ env_isatty 1 dune rules -m stub.o | tr -s '\t\n\\' ' ' > out_stub

  $ grep -ce "-fdiagnostics-color=always" out_stub
  0
  [1]
