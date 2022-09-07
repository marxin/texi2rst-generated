.. _producing-a-python-module:

How to produce a Python module
******************************

This section descibes how it is possible to produce a Python module
from Modula-2 code.  There are a number of advantages to this
approach, it ensures your code reaches a wider audience, maybe it is
easier to initialize your application in Python.

The example application here is a pedagogical two dimensional gravity
next event simulation.  The Python module needs to have a clear API
which should be placed in a single definition module.  Furthermore the
API should only use fundamental pervasive data types and strings.
Below the API is contained in the file :samp:`twoDsim.def`:

.. code-block:: modula2

  DEFINITION MODULE twoDsim ;

  EXPORT UNQUALIFIED gravity, box, poly3, poly5, poly6, mass,
                     fix, circle, pivot, velocity, accel, fps,
                     replayRate, simulateFor ;
  (*
     gravity - turn on gravity at: g m^2
  *)

  PROCEDURE gravity (g: REAL) ;

  (*
     box - place a box in the world at (x0,y0),(x0+i,y0+j)
  *)

  PROCEDURE box (x0, y0, i, j: REAL) : CARDINAL ;

  (*
     poly3 - place a triangle in the world at:
             (x0,y0),(x1,y1),(x2,y2)
  *)

  PROCEDURE poly3 (x0, y0, x1, y1, x2, y2: REAL) : CARDINAL ;

  (*
     poly5 - place a pentagon in the world at:
             (x0,y0),(x1,y1),(x2,y2),(x3,y3),(x4,y4)
  *)

  PROCEDURE poly5 (x0, y0, x1, y1,
                   x2, y2, x3, y3, x4, y4: REAL) : CARDINAL ;

  (*
     poly6 - place a hexagon in the world at:
             (x0,y0),(x1,y1),(x2,y2),(x3,y3),(x4,y4),(x5,y5)
  *)

  PROCEDURE poly6 (x0, y0, x1, y1,
                   x2, y2, x3, y3,
                   x4, y4, x5, y5: REAL) : CARDINAL ;

  (*
     mass - specify the mass of an object and return the, id.
  *)

  PROCEDURE mass (id: CARDINAL; m: REAL) : CARDINAL ;

  (*
     fix - fix the object to the world.
  *)

  PROCEDURE fix (id: CARDINAL) : CARDINAL ;

  (*
     circle - adds a circle to the world.  Center
              defined by: x0, y0 radius, r.
  *)

  PROCEDURE circle (x0, y0, r: REAL) : CARDINAL ;

  (*
     velocity - give an object, id, a velocity, vx, vy.
  *)

  PROCEDURE velocity (id: CARDINAL; vx, vy: REAL) : CARDINAL ;

  (*
     accel - give an object, id, an acceleration, ax, ay.
  *)

  PROCEDURE accel (id: CARDINAL; ax, ay: REAL) : CARDINAL ;

  (*
     fps - set frames per second.
  *)

  PROCEDURE fps (f: REAL) ;

  (*
     replayRate - set frames per second during replay.
  *)

  PROCEDURE replayRate (f: REAL) ;

  (*
     simulateFor - render for, t, seconds.
  *)

  PROCEDURE simulateFor (t: REAL) ;

  END twoDsim.

The keyword ``UNQUALIFIED`` can be used to ensure that the
compiler will provide externally accessible functions
``gravity``, ``box``, ``poly3``, ``poly5``, ``poly6``,
``mass``, ``fix``, ``circle``, ``pivot``, ``velocity``,
``accel``, ``fps``, ``replayRate``, ``simulateFor``
rather than name mangled alternatives.
Hence in our Python3 application we could write:

.. code-block:: modula2

  #!/usr/bin/env python3

  from twoDsim import *

  b = box (0.0, 0.0, 1.0, 1.0)
  b = fix (b)
  c1 = circle (0.7, 0.7, 0.05)
  c1 = mass (c1, 0.01)
  c2 = circle (0.7, 0.1, 0.05)
  c2 = mass (c2, 0.01)
  c2 = fix (c2)
  gravity (-9.81)
  fps (24.0*4.0)
  replayRate (24.0)
  print ("creating frames")
  try:
      simulateFor (1.0)
      print ("all done")
  except:
      print ("exception raised")

which accesses the various functions defined and implemented by the
module ``twoDsim``.  The Modula-2 source code is compiled via:

.. code-block:: modula2

  $ gm2 -g -fiso -c -fswig twoDsim.mod
  $ gm2 -g -fiso -c -fmakelist twoDsim.mod
  $ gm2 -g -fiso -c -fmakeinit twoDsim.mod

The first command both compiles the source file creating
:samp:`twoDsim.o` and produces a swig interface file :samp:`swig.i`.  We
now use ``swig`` and ``g++`` to produce and compile the
interface wrappers:

.. code-block:: modula2

  $ libtool --mode=compile g++ -g -c twoDsim_m2.cpp -o twoDsim_m2.lo
  $ swig -c++ -python3 twoDsim.i
  $ libtool --mode=compile g++ -c -fPIC twoDsim_wrap.cxx \
     -I/usr/include/python3 -o twoDsim_wrap.lo
  $ libtool --mode=compile gm2 -g -fPIC -fiso -c deviceGnuPic.mod
  $ libtool --mode=compile gm2 -g -fPIC -fiso -c roots.mod
  $ libtool --mode=compile gm2 -g -fPIC -fiso -c -fswig \
     twoDsim.mod -o twoDsim.lo

Finally the application is linked into a shared library:

.. code-block:: modula2

  $ libtool --mode=link gcc -g twoDsim_m2.lo twoDsim_wrap.lo \
    roots.lo deviceGnuPic.lo \
     -L${prefix}/lib64 \
     -rpath `pwd` -lgm2 -lstdc++ -lm -o libtwoDsim.la
  cp .libs/libtwoDsim.so _twoDsim.so

The library name must start with ``_`` to comply with the Python3
module naming scheme.

