  .. _dtime:

DTIME --- Execution time subroutine (or function)
*************************************************

.. index:: DTIME

.. index:: time, elapsed

.. index:: elapsed time

:samp:`{Description}:`
  ``DTIME(VALUES, TIME)`` initially returns the number of seconds of runtime
  since the start of the process's execution in :samp:`{TIME}`.  :samp:`{VALUES}`
  returns the user and system components of this time in ``VALUES(1)`` and
  ``VALUES(2)`` respectively. :samp:`{TIME}` is equal to ``VALUES(1) +
  VALUES(2)``.

  Subsequent invocations of ``DTIME`` return values accumulated since the
  previous invocation.

  On some systems, the underlying timings are represented using types with
  sufficiently small limits that overflows (wrap around) are possible, such as
  32-bit types. Therefore, the values returned by this intrinsic might be, or
  become, negative, or numerically less than previous values, during a single
  run of the compiled program.

  Please note, that this implementation is thread safe if used within OpenMP
  directives, i.e., its state will be consistent while called from multiple
  threads. However, if ``DTIME`` is called from multiple threads, the result
  is still the time since the last invocation. This may not give the intended
  results. If possible, use ``CPU_TIME`` instead.

  This intrinsic is provided in both subroutine and function forms; however,
  only one form can be used in any given program unit.

  :samp:`{VALUES}` and :samp:`{TIME}` are ``INTENT(OUT)`` and provide the following:

  ===============  ================================
  ``VALUES(1)`` :  User time in seconds.
  ===============  ================================
  ``VALUES(2)`` :  System time in seconds.
  ``TIME`` :       Run time since start in seconds.
  ===============  ================================

:samp:`{Standard}:`
  GNU extension

:samp:`{Class}:`
  Subroutine, function

:samp:`{Syntax}:`
  ============================================
  ``CALL DTIME(VALUES, TIME)``.
  ============================================
  ``TIME = DTIME(VALUES)``, (not recommended).
  ============================================

:samp:`{Arguments}:`
  ================  ============================================
  :samp:`{VALUES}`  The type shall be ``REAL(4), DIMENSION(2)``.
  ================  ============================================
  :samp:`{TIME}`    The type shall be ``REAL(4)``.
  ================  ============================================

:samp:`{Return value}:`
  Elapsed time in seconds since the last invocation or since the start of program
  execution if not called before.

:samp:`{Example}:`

  .. code-block:: fortran

    program test_dtime
        integer(8) :: i, j
        real, dimension(2) :: tarray
        real :: result
        call dtime(tarray, result)
        print *, result
        print *, tarray(1)
        print *, tarray(2)   
        do i=1,100000000    ! Just a delay
            j = i * i - i
        end do
        call dtime(tarray, result)
        print *, result
        print *, tarray(1)
        print *, tarray(2)
    end program test_dtime

:samp:`{See also}:`
  CPU_TIME

