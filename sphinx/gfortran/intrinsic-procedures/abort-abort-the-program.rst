.. _abort:

ABORT --- Abort the program
***************************

.. index:: ABORT

.. index:: program termination, with core dump

.. index:: terminate program, with core dump

.. index:: core, dump

:samp:`{Description}:`
  ``ABORT`` causes immediate termination of the program.  On operating
  systems that support a core dump, ``ABORT`` will produce a core dump.
  It will also print a backtrace, unless ``-fno-backtrace`` is given.

:samp:`{Standard}:`
  GNU extension

:samp:`{Class}:`
  Subroutine

:samp:`{Syntax}:`
  ``CALL ABORT``

:samp:`{Return value}:`
  Does not return.

:samp:`{Example}:`

  .. code-block:: c++

    program test_abort
      integer :: i = 1, j = 2
      if (i /= j) call abort
    end program test_abort

:samp:`{See also}:`
  EXIT, 
  KILL, 
  BACKTRACE

