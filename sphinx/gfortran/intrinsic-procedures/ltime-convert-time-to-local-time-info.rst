  .. _ltime:

LTIME --- Convert time to local time info
*****************************************

.. index:: LTIME

.. index:: time, conversion to local time info

:samp:`{Description}:`
  Given a system time value :samp:`{TIME}` (as provided by the TIME
  intrinsic), fills :samp:`{VALUES}` with values extracted from it appropriate
  to the local time zone using ``localtime(3)``.

  This intrinsic routine is provided for backwards compatibility with 
  GNU Fortran 77.  In new code, programmers should consider the use of 
  the DATE_AND_TIME intrinsic defined by the Fortran 95
  standard.

:samp:`{Standard}:`
  GNU extension

:samp:`{Class}:`
  Subroutine

:samp:`{Syntax}:`
  ``CALL LTIME(TIME, VALUES)``

:samp:`{Arguments}:`
  ================  ====================================================
  :samp:`{TIME}`    An ``INTEGER`` scalar expression
                    corresponding to a system time, with ``INTENT(IN)``.
  ================  ====================================================
  :samp:`{VALUES}`  A default ``INTEGER`` array with 9 elements,
                    with ``INTENT(OUT)``.
  ================  ====================================================

:samp:`{Return value}:`
  The elements of :samp:`{VALUES}` are assigned as follows:

  * Seconds after the minute, range 0--59 or 0--61 to allow for leap
    seconds

  * Minutes after the hour, range 0--59

  * Hours past midnight, range 0--23

  * Day of month, range 1--31

  * Number of months since January, range 0--11

  * Years since 1900

  * Number of days since Sunday, range 0--6

  * Days since January 1, range 0--365

  * Daylight savings indicator: positive if daylight savings is in
    effect, zero if not, and negative if the information is not available.

:samp:`{See also}:`
  DATE_AND_TIME, 
  CTIME, 
  GMTIME, 
  TIME, 
  TIME8

