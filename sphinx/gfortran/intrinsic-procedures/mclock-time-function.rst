  .. _mclock:

MCLOCK --- Time function
************************

.. index:: MCLOCK

.. index:: time, clock ticks

.. index:: clock ticks

:samp:`{Description}:`
  Returns the number of clock ticks since the start of the process, based
  on the function ``clock(3)`` in the C standard library.

  This intrinsic is not fully portable, such as to systems with 32-bit
  ``INTEGER`` types but supporting times wider than 32 bits. Therefore,
  the values returned by this intrinsic might be, or become, negative, or
  numerically less than previous values, during a single run of the
  compiled program.

:samp:`{Standard}:`
  GNU extension

:samp:`{Class}:`
  Function

:samp:`{Syntax}:`
  ``RESULT = MCLOCK()``

:samp:`{Return value}:`
  The return value is a scalar of type ``INTEGER(4)``, equal to the
  number of clock ticks since the start of the process, or ``-1`` if
  the system does not support ``clock(3)``.

:samp:`{See also}:`
  CTIME, 
  GMTIME, 
  LTIME, 
  MCLOCK, 
  TIME

