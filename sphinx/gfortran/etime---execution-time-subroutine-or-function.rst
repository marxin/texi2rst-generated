  .. _etime:

``ETIME`` - Execution time subroutine (or function)
***************************************************

.. index:: ETIME

.. index:: time, elapsed

:samp:`{Description}:`
  ``ETIME(VALUES, TIME)`` returns the number of seconds of runtime
  since the start of the process's execution in :samp:`{TIME}`.  :samp:`{VALUES}`
  returns the user and system components of this time in ``VALUES(1)`` and
  ``VALUES(2)`` respectively. :samp:`{TIME}` is equal to ``VALUES(1) + VALUES(2)``.

  On some systems, the underlying timings are represented using types with
  sufficiently small limits that overflows (wrap around) are possible, such as
  32-bit types. Therefore, the values returned by this intrinsic might be, or
  become, negative, or numerically less than previous values, during a single
  run of the compiled program.

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
  ``CALL ETIME(VALUES, TIME)``.
  ============================================
  ``TIME = ETIME(VALUES)``, (not recommended).
  ============================================

:samp:`{Arguments}:`
  ================  ============================================
  :samp:`{VALUES}`  The type shall be ``REAL(4), DIMENSION(2)``.
  ================  ============================================
  :samp:`{TIME}`    The type shall be ``REAL(4)``.
  ================  ============================================

:samp:`{Return value}:`
  Elapsed time in seconds since the start of program execution.

:samp:`{Example}:`

  .. code-block:: c++

    program test_etime
        integer(8) :: i, j
        real, dimension(2) :: tarray
        real :: result
        call ETIME(tarray, result)
        print *, result
        print *, tarray(1)
        print *, tarray(2)   
        do i=1,100000000    ! Just a delay
            j = i * i - i
        end do
        call ETIME(tarray, result)
        print *, result
        print *, tarray(1)
        print *, tarray(2)
    end program test_etime

:samp:`{See also}:`
  CPU_TIME

