  .. _fdate:

FDATE --- Get the current time as a string
******************************************

.. index:: FDATE

.. index:: time, current

.. index:: current time

.. index:: date, current

.. index:: current date

:samp:`{Description}:`
  ``FDATE(DATE)`` returns the current date (using the same format as
  CTIME) in :samp:`{DATE}`. It is equivalent to ``CALL CTIME(DATE,
  TIME())``.

  This intrinsic is provided in both subroutine and function forms; however,
  only one form can be used in any given program unit.

:samp:`{Standard}:`
  GNU extension

:samp:`{Class}:`
  Subroutine, function

:samp:`{Syntax}:`
  =====================
  ``CALL FDATE(DATE)``.
  =====================
  ``DATE = FDATE()``.
  =====================

:samp:`{Arguments}:`
  ==============  ==================================================================
  :samp:`{DATE}`  The type shall be of type ``CHARACTER`` of the
                  default kind. It is an ``INTENT(OUT)`` argument.  If the length of
                  this variable is too short for the date and time string to fit
                  completely, it will be blank on procedure return.
  ==============  ==================================================================
  ==============  ==================================================================

:samp:`{Return value}:`
  The current date and time as a string.

:samp:`{Example}:`

  .. code-block:: c++

    program test_fdate
        integer(8) :: i, j
        character(len=30) :: date
        call fdate(date)
        print *, 'Program started on ', date
        do i = 1, 100000000 ! Just a delay
            j = i * i - i
        end do
        call fdate(date)
        print *, 'Program ended on ', date
    end program test_fdate

:samp:`{See also}:`
  DATE_AND_TIME, 
  CTIME

