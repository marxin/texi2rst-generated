  .. _cpu_time:

CPU_TIME --- CPU elapsed time in seconds
****************************************

.. index:: CPU_TIME

.. index:: time, elapsed

:samp:`{Description}:`
  Returns a ``REAL`` value representing the elapsed CPU time in
  seconds.  This is useful for testing segments of code to determine
  execution time.

  If a time source is available, time will be reported with microsecond
  resolution. If no time source is available, :samp:`{TIME}` is set to
  ``-1.0``.

  Note that :samp:`{TIME}` may contain a, system dependent, arbitrary offset
  and may not start with ``0.0``. For ``CPU_TIME``, the absolute
  value is meaningless, only differences between subsequent calls to
  this subroutine, as shown in the example below, should be used.

:samp:`{Standard}:`
  Fortran 95 and later

:samp:`{Class}:`
  Subroutine

:samp:`{Syntax}:`
  ``CALL CPU_TIME(TIME)``

:samp:`{Arguments}:`
  ==============  ================================================
  :samp:`{TIME}`  The type shall be ``REAL`` with ``INTENT(OUT)``.
  ==============  ================================================
  ==============  ================================================

:samp:`{Return value}:`
  None

:samp:`{Example}:`

  .. code-block:: fortran

    program test_cpu_time
        real :: start, finish
        call cpu_time(start)
            ! put code to test here
        call cpu_time(finish)
        print '("Time = ",f6.3," seconds.")',finish-start
    end program test_cpu_time

:samp:`{See also}:`
  SYSTEM_CLOCK, 
  DATE_AND_TIME

