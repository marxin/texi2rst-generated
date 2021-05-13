  .. _itime:

``ITIME`` - Get current local time subroutine (hour/minutes/seconds) 
*********************************************************************

.. index:: ITIME

.. index:: time, current

.. index:: current time

:samp:`{Description}:`
  ``ITIME(VALUES)`` Fills :samp:`{VALUES}` with the numerical values at the  
  current local time. The hour (in the range 1-24), minute (in the range 1-60), 
  and seconds (in the range 1-60) appear in elements 1, 2, and 3 of :samp:`{VALUES}`, 
  respectively.

  This intrinsic routine is provided for backwards compatibility with 
  GNU Fortran 77.  In new code, programmers should consider the use of 
  the DATE_AND_TIME intrinsic defined by the Fortran 95
  standard.

:samp:`{Standard}:`
  GNU extension

:samp:`{Class}:`
  Subroutine

:samp:`{Syntax}:`
  ``CALL ITIME(VALUES)``

:samp:`{Arguments}:`
  ================  ===============================================
  :samp:`{VALUES}`  The type shall be ``INTEGER, DIMENSION(3)``
                    and the kind shall be the default integer kind.
  ================  ===============================================
  ================  ===============================================

:samp:`{Return value}:`
  Does not return anything.

:samp:`{Example}:`

  .. code-block:: c++

    program test_itime
      integer, dimension(3) :: tarray
      call itime(tarray)
      print *, tarray(1)
      print *, tarray(2)
      print *, tarray(3)
    end program test_itime

:samp:`{See also}:`
  DATE_AND_TIME

