.. _regression-tests:

Regression tests for gm2 in the repository
******************************************

The regression testsuite can be run from the gcc build directory:

.. code-block:: modula2

  $ cd build-gcc
  $ make check -j 24

which runs the complete testsuite for all compilers using 24 parallel
invocations of the compiler.  Individual language testsuites can be
run by specifying the language, for example the Modula-2 testsuite can
be run using:

.. code-block:: modula2

  $ cd build-gcc
  $ make check-m2 -j 24

Finally the results of the testsuite can be emailed to the
https://gcc.gnu.org/lists.htmlgcc-testresults list using the
script:

.. code-block:: modula2

  $ gccsrcdir/contrib/test_summary

