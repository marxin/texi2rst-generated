..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the GPL license file

.. _date_and_time:

.. index:: DATE_AND_TIME

.. index:: date, current

.. index:: current date

.. index:: time, current

.. index:: current time

DATE_AND_TIME --- Date and time subroutine
******************************************

.. function:: DATE_AND_TIME(DATE, TIME, ZONE, VALUES)

  ``DATE_AND_TIME(DATE, TIME, ZONE, VALUES)`` gets the corresponding date and
  time information from the real-time system clock.  :samp:`{DATE}` is
  ``INTENT(OUT)`` and has form ccyymmdd.  :samp:`{TIME}` is ``INTENT(OUT)`` and
  has form hhmmss.sss.  :samp:`{ZONE}` is ``INTENT(OUT)`` and has form (+-)hhmm,
  representing the difference with respect to Coordinated Universal Time (UTC).
  Unavailable time and date parameters return blanks.

  :param DATE:
    (Optional) The type shall be ``CHARACTER(LEN=8)``
    or larger, and of default kind.

  :param TIME:
    (Optional) The type shall be ``CHARACTER(LEN=10)``
    or larger, and of default kind.

  :param ZONE:
    (Optional) The type shall be ``CHARACTER(LEN=5)``
    or larger, and of default kind.

  :param VALUES:
    (Optional) The type shall be ``INTEGER(8)``.

  :return:
    None

  :samp:`{Standard}:`
    Fortran 90 and later

  :samp:`{Class}:`
    Subroutine

  :samp:`{Syntax}:`

    .. code-block:: fortran

      CALL DATE_AND_TIME([DATE, TIME, ZONE, VALUES])

  :samp:`{Example}:`

    .. code-block:: fortran

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
    :ref:`CPU_TIME`, 
    :ref:`SYSTEM_CLOCK`

