  .. _ctime:

CTIME --- Convert a time into a string
**************************************

.. index:: CTIME

.. index:: time, conversion to string

.. index:: conversion, to string

:samp:`{Description}:`
  ``CTIME`` converts a system time value, such as returned by
  TIME8, to a string. The output will be of the form :samp:`Sat
  Aug 19 18:13:14 1995`.

  This intrinsic is provided in both subroutine and function forms; however,
  only one form can be used in any given program unit.

:samp:`{Standard}:`
  GNU extension

:samp:`{Class}:`
  Subroutine, function

:samp:`{Syntax}:`
  =============================
  ``CALL CTIME(TIME, RESULT)``.
  =============================
  ``RESULT = CTIME(TIME)``.
  =============================

:samp:`{Arguments}:`
  ================  =================================================================
  :samp:`{TIME}`    The type shall be of type ``INTEGER``.
  ================  =================================================================
  :samp:`{RESULT}`  The type shall be of type ``CHARACTER`` and
                    of default kind. It is an ``INTENT(OUT)`` argument. If the length
                    of this variable is too short for the time and date string to fit
                    completely, it will be blank on procedure return.
  ================  =================================================================

:samp:`{Return value}:`
  The converted date and time as a string.

:samp:`{Example}:`

  .. code-block:: fortran

    program test_ctime
        integer(8) :: i
        character(len=30) :: date
        i = time8()

        ! Do something, main part of the program

        call ctime(i,date)
        print *, 'Program was started on ', date
    end program test_ctime

:samp:`{See Also}:`
  DATE_AND_TIME, 
  GMTIME, 
  LTIME, 
  TIME, 
  TIME8

