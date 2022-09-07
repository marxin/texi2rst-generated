.. _interface-for-python:

How to produce swig interface files
***********************************

This section describes how Modula-2 implementation modules can be
called from Python (and other scripting languages such as TCL and
Perl).  GNU Modula-2 can be instructed to create a swig interface when
it is compiling an implementation module.  Swig then uses the
interface file to generate all the necessary wrapping to that the
desired scripting language may access the implementation module.

Here is an example of how you might call upon the services of the
Modula-2 library module ``NumberIO`` from Python3.

The following commands can be used to generate the Python3 module:

.. code-block:: modula2

  export src=directory to the sources
  export prefix=directory to where the compiler is installed
  gm2 -I${src} -c -g -fswig ${src}/../../../gm2-libs/NumberIO.mod
  gm2 -I${src} -c -g -fmakelist ${src}/../../../gm2-libs/NumberIO.mod

  gm2 -I${src} -c -g -fmakeinit -fshared \
     ${src}/../../../gm2-libs/NumberIO.mod

  swig -c++ -python3 NumberIO.i

  libtool --mode=compile g++ -g -c -I${src} NumberIO_m2.cpp \
    -o NumberIO_m2.lo

  libtool --tag=CC --mode=compile gm2 -g -c \
    -I${src}../../../gm2-libs \
    ${src}/../../../gm2-libs/NumberIO.mod -o NumberIO.lo

  libtool --tag=CC --mode=compile g++ -g -c NumberIO_wrap.cxx \
    -I/usr/include/python3 -o NumberIO_wrap.lo

  libtool --mode=link gcc -g NumberIO_m2.lo NumberIO_wrap.lo \
     -L${prefix}/lib64 \
     -rpath `pwd` -lgm2 -lstdc++ -lm -o libNumberIO.la

  cp .libs/libNumberIO.so _NumberIO.so

The first four commands, generate the swig interface file
:samp:`NumberIO.i` and python wrap files :samp:`NumberIO_wrap.cxx` and
:samp:`NumberIO.py`.  The next three :samp:`libtool` commnads compile
the C++ and Modula-2 source code into :samp:`.lo` objects.  The last
:samp:`libtool` command links all the :samp:`.lo` files into a
:samp:`.la` file and includes all shared library dependencies.

Now it is possible to run the following Python script
(called :samp:`testnum.py`):

.. code-block:: modula2

  import NumberIO

  print ("1234 x 2 =", NumberIO.NumberIO_StrToInt("1234")*2)

like this:

.. code-block:: modula2

  $ python3 testnum.py
  1234 x 2 = 2468

See :ref:`gm2:producing-a-python-module` for another example which
uses the ``UNQUALIFIED`` keyword to reduce the module name clutter
from the viewport of Python3.

.. toctree::
  :maxdepth: 2

  limitations-of-automatic-generated-of-swig-files

