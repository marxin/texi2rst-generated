  .. _idate:

``IDATE`` - Get current local time subroutine (day/month/year) 
***************************************************************

.. index:: IDATE

.. index:: date, current

.. index:: current date

:samp:`{Description}:`
  ``IDATE(VALUES)`` Fills :samp:`{VALUES}` with the numerical values at the  
  current local time. The day (in the range 1-31), month (in the range 1-12), 
  and year appear in elements 1, 2, and 3 of :samp:`{VALUES}` , respectively. 
  The year has four significant digits.

  This intrinsic routine is provided for backwards compatibility with 
  GNU Fortran 77.  In new code, programmers should consider the use of 
  the DATE_AND_TIME intrinsic defined by the Fortran 95
  standard.

:samp:`{Standard}:`
  GNU extension

:samp:`{Class}:`
  Subroutine

:samp:`{Syntax}:`
  ``CALL IDATE(VALUES)``

:samp:`{Arguments}:`
  ================  ===============================================
  :samp:`{VALUES}`  The type shall be ``INTEGER, DIMENSION(3)`` and
                    the kind shall be the default integer kind.
  ================  ===============================================
  ================  ===============================================

:samp:`{Return value}:`
  Does not return anything.

:samp:`{Example}:`

  .. code-block:: c++

    program test_idate
      integer, dimension(3) :: tarray
      call idate(tarray)
      print *, tarray(1)
      print *, tarray(2)
      print *, tarray(3)
    end program test_idate

:samp:`{See also}:`
  DATE_AND_TIME

