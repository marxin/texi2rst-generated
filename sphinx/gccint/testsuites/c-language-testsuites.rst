.. _c-tests:

C Language Testsuites
*********************

GCC contains the following C language testsuites, in the
gcc/testsuite directory:

gcc.dg
  This contains tests of particular features of the C compiler, using the
  more modern :samp:`dg` harness.  Correctness tests for various compiler
  features should go here if possible.

  Magic comments determine whether the file
  is preprocessed, compiled, linked or run.  In these tests, error and warning
  message texts are compared against expected texts or regular expressions
  given in comments.  These tests are run with the options :samp:`-ansi -pedantic`
  unless other options are given in the test.  Except as noted below they
  are not run with multiple optimization options.

gcc.dg/compat
  This subdirectory contains tests for binary compatibility using
  lib/compat.exp, which in turn uses the language-independent support
  (see :ref:`Support for testing binary compatibility <compat-testing>`).

gcc.dg/cpp
  This subdirectory contains tests of the preprocessor.

gcc.dg/debug
  This subdirectory contains tests for debug formats.  Tests in this
  subdirectory are run for each debug format that the compiler supports.

gcc.dg/format
  This subdirectory contains tests of the :option:`-Wformat` format
  checking.  Tests in this directory are run with and without
  :option:`-DWIDE`.

gcc.dg/noncompile
  This subdirectory contains tests of code that should not compile and
  does not need any special compilation options.  They are run with
  multiple optimization options, since sometimes invalid code crashes
  the compiler with optimization.

gcc.dg/special
  FIXME: describe this.

gcc.c-torture
  This contains particular code fragments which have historically broken easily.
  These tests are run with multiple optimization options, so tests for features
  which only break at some optimization levels belong here.  This also contains
  tests to check that certain optimizations occur.  It might be worthwhile to
  separate the correctness tests cleanly from the code quality tests, but
  it hasn't been done yet.

gcc.c-torture/compat
  FIXME: describe this.

  This directory should probably not be used for new tests.

gcc.c-torture/compile
  This testsuite contains test cases that should compile, but do not
  need to link or run.  These test cases are compiled with several
  different combinations of optimization options.  All warnings are
  disabled for these test cases, so this directory is not suitable if
  you wish to test for the presence or absence of compiler warnings.
  While special options can be set, and tests disabled on specific
  platforms, by the use of .x files, mostly these test cases
  should not contain platform dependencies.  FIXME: discuss how defines
  such as ``STACK_SIZE`` are used.

gcc.c-torture/execute
  This testsuite contains test cases that should compile, link and run;
  otherwise the same comments as for gcc.c-torture/compile apply.

gcc.c-torture/execute/ieee
  This contains tests which are specific to IEEE floating point.

gcc.c-torture/unsorted
  FIXME: describe this.

  This directory should probably not be used for new tests.

gcc.misc-tests
  This directory contains C tests that require special handling.  Some
  of these tests have individual expect files, and others share
  special-purpose expect files:

  bprob*.c
    Test :option:`-fbranch-probabilities` using
    gcc.misc-tests/bprob.exp, which
    in turn uses the generic, language-independent framework
    (see :ref:`Support for testing profile-directed
    optimizations <profopt-testing>`).

  gcov*.c
    Test :command:`gcov` output using gcov.exp, which in turn uses the
    language-independent support (see :ref:`Support for testing gcov <gcov-testing>`).

  i386-pf-*.c
    Test i386-specific support for data prefetch using i386-prefetch.exp.

gcc.test-framework

  dg-*.c
    Test the testsuite itself using gcc.test-framework/test-framework.exp.

    FIXME: merge in testsuite/README.gcc and discuss the format of
test cases and magic comments more.

