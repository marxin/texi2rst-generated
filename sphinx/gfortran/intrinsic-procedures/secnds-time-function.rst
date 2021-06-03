  .. _secnds:

SECNDS --- Time function
************************

.. index:: SECNDS

.. index:: time, elapsed

.. index:: elapsed time

:samp:`{Description}:`
  ``SECNDS(X)`` gets the time in seconds from the real-time system clock.
  :samp:`{X}` is a reference time, also in seconds. If this is zero, the time in
  seconds from midnight is returned. This function is non-standard and its
  use is discouraged.

:samp:`{Standard}:`
  GNU extension

:samp:`{Class}:`
  Function

:samp:`{Syntax}:`
  ``RESULT = SECNDS (X)``

:samp:`{Arguments}:`
  ===========  =============================
  :samp:`{T}`  Shall be of type ``REAL(4)``.
  ===========  =============================
  :samp:`{X}`  Shall be of type ``REAL(4)``.
  ===========  =============================

:samp:`{Return value}:`
  None

:samp:`{Example}:`

  .. code-block:: c++

    program test_secnds
        integer :: i
        real(4) :: t1, t2
        print *, secnds (0.0)   ! seconds since midnight
        t1 = secnds (0.0)       ! reference time
        do i = 1, 10000000      ! do something
        end do
        t2 = secnds (t1)        ! elapsed time
        print *, "Something took ", t2, " seconds."
    end program test_secnds

