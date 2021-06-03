  .. _date_and_time:

DATE_AND_TIME --- Date and time subroutine
******************************************

.. index:: DATE_AND_TIME

.. index:: date, current

.. index:: current date

.. index:: time, current

.. index:: current time

:samp:`{Description}:`
  ``DATE_AND_TIME(DATE, TIME, ZONE, VALUES)`` gets the corresponding date and
  time information from the real-time system clock.  :samp:`{DATE}` is
  ``INTENT(OUT)`` and has form ccyymmdd.  :samp:`{TIME}` is ``INTENT(OUT)`` and
  has form hhmmss.sss.  :samp:`{ZONE}` is ``INTENT(OUT)`` and has form (+-)hhmm,
  representing the difference with respect to Coordinated Universal Time (UTC).
  Unavailable time and date parameters return blanks.

  :samp:`{VALUES}` is ``INTENT(OUT)`` and provides the following:

  ==============  ===================================
  ``VALUE(1)`` :  The year
  ==============  ===================================
  ``VALUE(2)`` :  The month
  ``VALUE(3)`` :  The day of the month
  ``VALUE(4)`` :  Time difference with UTC in minutes
  ``VALUE(5)`` :  The hour of the day
  ``VALUE(6)`` :  The minutes of the hour
  ``VALUE(7)`` :  The seconds of the minute
  ``VALUE(8)`` :  The milliseconds of the second
  ==============  ===================================

:samp:`{Standard}:`
  Fortran 90 and later

:samp:`{Class}:`
  Subroutine

:samp:`{Syntax}:`
  ``CALL DATE_AND_TIME([DATE, TIME, ZONE, VALUES])``

:samp:`{Arguments}:`
  ================  ==================================================
  :samp:`{DATE}`    (Optional) The type shall be ``CHARACTER(LEN=8)``
                    or larger, and of default kind.
  ================  ==================================================
  :samp:`{TIME}`    (Optional) The type shall be ``CHARACTER(LEN=10)``
                    or larger, and of default kind.
  :samp:`{ZONE}`    (Optional) The type shall be ``CHARACTER(LEN=5)``
                    or larger, and of default kind.
  :samp:`{VALUES}`  (Optional) The type shall be ``INTEGER(8)``.
  ================  ==================================================

:samp:`{Return value}:`
  None

:samp:`{Example}:`

  .. code-block:: c++

    program test_time_and_date
        character(8)  :: date
        character(10) :: time
        character(5)  :: zone
        integer,dimension(8) :: values
        ! using keyword arguments
        call date_and_time(date,time,zone,values)
        call date_and_time(DATE=date,ZONE=zone)
        call date_and_time(TIME=time)
        call date_and_time(VALUES=values)
        print '(a,2x,a,2x,a)', date, time, zone
        print '(8i5)', values
    end program test_time_and_date

:samp:`{See also}:`
  CPU_TIME, 
  SYSTEM_CLOCK

