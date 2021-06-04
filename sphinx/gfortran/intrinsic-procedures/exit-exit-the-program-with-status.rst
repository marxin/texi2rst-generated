  .. _exit:

EXIT --- Exit the program with status. 
***************************************

.. index:: EXIT

.. index:: program termination

.. index:: terminate program

:samp:`{Description}:`
  ``EXIT`` causes immediate termination of the program with status.  If status
  is omitted it returns the canonical *success* for the system.  All Fortran
  I/O units are closed. 

:samp:`{Standard}:`
  GNU extension

:samp:`{Class}:`
  Subroutine

:samp:`{Syntax}:`
  ``CALL EXIT([STATUS])``

:samp:`{Arguments}:`
  ================  ============================================
  :samp:`{STATUS}`  Shall be an ``INTEGER`` of the default kind.
  ================  ============================================

:samp:`{Return value}:`
  ``STATUS`` is passed to the parent process on exit.

:samp:`{Example}:`

  .. code-block:: fortran

    program test_exit
      integer :: STATUS = 0
      print *, 'This program is going to exit.'
      call EXIT(STATUS)
    end program test_exit

:samp:`{See also}:`
  ABORT, 
  KILL

