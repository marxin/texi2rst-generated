  .. _mclock8:

MCLOCK8 --- Time function (64-bit)
**********************************

.. index:: MCLOCK8

.. index:: time, clock ticks

.. index:: clock ticks

:samp:`{Description}:`
  Returns the number of clock ticks since the start of the process, based
  on the function ``clock(3)`` in the C standard library.

  *Warning:* this intrinsic does not increase the range of the timing
  values over that returned by ``clock(3)``. On a system with a 32-bit
  ``clock(3)``, ``MCLOCK8`` will return a 32-bit value, even though
  it is converted to a 64-bit ``INTEGER(8)`` value. That means
  overflows of the 32-bit value can still occur. Therefore, the values
  returned by this intrinsic might be or become negative or numerically
  less than previous values during a single run of the compiled program.

:samp:`{Standard}:`
  GNU extension

:samp:`{Class}:`
  Function

:samp:`{Syntax}:`
  ``RESULT = MCLOCK8()``

:samp:`{Return value}:`
  The return value is a scalar of type ``INTEGER(8)``, equal to the
  number of clock ticks since the start of the process, or ``-1`` if
  the system does not support ``clock(3)``.

:samp:`{See also}:`
  CTIME, 
  GMTIME, 
  LTIME, 
  MCLOCK, 
  TIME8

