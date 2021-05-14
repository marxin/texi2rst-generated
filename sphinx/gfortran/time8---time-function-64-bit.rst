  .. _time8:

TIME8 - Time function (64-bit)
******************************

.. index:: TIME8

.. index:: time, current

.. index:: current time

:samp:`{Description}:`
  Returns the current time encoded as an integer (in the manner of the
  function ``time(3)`` in the C standard library). This value is
  suitable for passing to CTIME, GMTIME, and LTIME.

  *Warning:* this intrinsic does not increase the range of the timing
  values over that returned by ``time(3)``. On a system with a 32-bit
  ``time(3)``, ``TIME8`` will return a 32-bit value, even though
  it is converted to a 64-bit ``INTEGER(8)`` value. That means
  overflows of the 32-bit value can still occur. Therefore, the values
  returned by this intrinsic might be or become negative or numerically
  less than previous values during a single run of the compiled program.

:samp:`{Standard}:`
  GNU extension

:samp:`{Class}:`
  Function

:samp:`{Syntax}:`
  ``RESULT = TIME8()``

:samp:`{Return value}:`
  The return value is a scalar of type ``INTEGER(8)``.

:samp:`{See also}:`
  DATE_AND_TIME, 
  CTIME, 
  GMTIME, 
  LTIME, 
  MCLOCK8, 
  TIME

