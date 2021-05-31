  .. _time:

TIME - Time function
********************

.. index:: TIME

.. index:: time, current

.. index:: current time

:samp:`{Description}:`
  Returns the current time encoded as an integer (in the manner of the
  function ``time(3)`` in the C standard library). This value is
  suitable for passing to CTIME, GMTIME, and LTIME.

  This intrinsic is not fully portable, such as to systems with 32-bit
  ``INTEGER`` types but supporting times wider than 32 bits. Therefore,
  the values returned by this intrinsic might be, or become, negative, or
  numerically less than previous values, during a single run of the
  compiled program.

  See TIME8, for information on a similar intrinsic that might be
  portable to more GNU Fortran implementations, though to fewer Fortran
  compilers.

:samp:`{Standard}:`
  GNU extension

:samp:`{Class}:`
  Function

:samp:`{Syntax}:`
  ``RESULT = TIME()``

:samp:`{Return value}:`
  The return value is a scalar of type ``INTEGER(4)``.

:samp:`{See also}:`
  DATE_AND_TIME, 
  CTIME, 
  GMTIME, 
  LTIME, 
  MCLOCK, 
  TIME8

