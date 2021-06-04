  .. _system_clock:

SYSTEM_CLOCK --- Time function
******************************

.. index:: SYSTEM_CLOCK

.. index:: time, clock ticks

.. index:: clock ticks

:samp:`{Description}:`
  Determines the :samp:`{COUNT}` of a processor clock since an unspecified
  time in the past modulo :samp:`{COUNT_MAX}`, :samp:`{COUNT_RATE}` determines
  the number of clock ticks per second.  If the platform supports a
  monotonic clock, that clock is used and can, depending on the platform
  clock implementation, provide up to nanosecond resolution.  If a
  monotonic clock is not available, the implementation falls back to a
  realtime clock.

  :samp:`{COUNT_RATE}` is system dependent and can vary depending on the kind of
  the arguments. For :samp:`{kind=4}` arguments (and smaller integer kinds),
  :samp:`{COUNT}` represents milliseconds, while for :samp:`{kind=8}` arguments (and
  larger integer kinds), :samp:`{COUNT}` typically represents micro- or
  nanoseconds depending on resolution of the underlying platform clock.
  :samp:`{COUNT_MAX}` usually equals ``HUGE(COUNT_MAX)``. Note that the
  millisecond resolution of the :samp:`{kind=4}` version implies that the
  :samp:`{COUNT}` will wrap around in roughly 25 days. In order to avoid issues
  with the wrap around and for more precise timing, please use the
  :samp:`{kind=8}` version.

  If there is no clock, or querying the clock fails, :samp:`{COUNT}` is set
  to ``-HUGE(COUNT)``, and :samp:`{COUNT_RATE}` and :samp:`{COUNT_MAX}` are
  set to zero.

  When running on a platform using the GNU C library (glibc) version
  2.16 or older, or a derivative thereof, the high resolution monotonic
  clock is available only when linking with the :samp:`{rt}` library.  This
  can be done explicitly by adding the ``-lrt`` flag when linking the
  application, but is also done implicitly when using OpenMP.

  On the Windows platform, the version with :samp:`{kind=4}` arguments uses
  the ``GetTickCount`` function, whereas the :samp:`{kind=8}` version
  uses ``QueryPerformanceCounter`` and
  ``QueryPerformanceCounterFrequency``. For more information, and
  potential caveats, please see the platform documentation.

:samp:`{Standard}:`
  Fortran 90 and later

:samp:`{Class}:`
  Subroutine

:samp:`{Syntax}:`
  ``CALL SYSTEM_CLOCK([COUNT, COUNT_RATE, COUNT_MAX])``

:samp:`{Arguments}:`
  ====================  ==============================================
  :samp:`{COUNT}`       (Optional) shall be a scalar of type 
                        ``INTEGER`` with ``INTENT(OUT)``.
  :samp:`{COUNT_RATE}`  (Optional) shall be a scalar of type 
                        ``INTEGER`` or ``REAL``, with ``INTENT(OUT)``.
  :samp:`{COUNT_MAX}`   (Optional) shall be a scalar of type 
                        ``INTEGER`` with ``INTENT(OUT)``.
  ====================  ==============================================

:samp:`{Example}:`

  .. code-block:: fortran

    PROGRAM test_system_clock
      INTEGER :: count, count_rate, count_max
      CALL SYSTEM_CLOCK(count, count_rate, count_max)
      WRITE(*,*) count, count_rate, count_max
    END PROGRAM

:samp:`{See also}:`
  DATE_AND_TIME, 
  CPU_TIME

