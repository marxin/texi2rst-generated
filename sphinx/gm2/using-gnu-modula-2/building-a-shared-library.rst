.. _building-a-shared-library:

Building a shared library
*************************

This section describes building a tiny shared library implemented in
Modula-2 and built with :samp:`libtool`.  Suppose a project consists of
two definition modules and two implementation modules and a program
module :samp:`a.def`, :samp:`a.mod`, :samp:`b.def`, :samp:`b.mod` and
:samp:`c.mod`.  The first step is to compile the modules using position
independent code.  This can be achieved by the following three
commands:

.. code-block:: modula2

  libtool --tag=CC --mode=compile gm2 -g -c a.mod -o a.lo
  libtool --tag=CC --mode=compile gm2 -g -c b.mod -o b.lo
  libtool --tag=CC --mode=compile gm2 -g -c c.mod -o c.lo

The second step is to generate the shared library initialization and
finalization routines.  We can do this by asking gm2 to generate a
list of dependant modules and then use this to generate the scaffold.
We also must compile the scaffold.

.. code-block:: modula2

  gm2 -c -g -fmakelist c.mod
  gm2 -c -g -fmakeinit -fshared c.mod
  libtool --tag=CC --mode=compile g++ -g -c c_m2.cpp -o c_m2.lo

The third step is to link all these :samp:`.lo` files.

.. code-block:: modula2

  libtool --mode=link gcc -g c_m2.lo a.lo b.lo c.lo \
          -L$(prefix)/lib64 \
          -rpath `pwd` -lgm2 -lstdc++ -lm -o libabc.la

At this point the shared library :samp:`libabc.so` will have been
created inside the directory :samp:`.libs`.

