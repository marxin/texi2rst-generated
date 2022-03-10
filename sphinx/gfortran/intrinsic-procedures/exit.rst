..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the GPL license file

.. _exit:

.. index:: EXIT

.. index:: program termination

.. index:: terminate program

EXIT --- Exit the program with status. 
***************************************

.. function:: EXIT

  ``EXIT`` causes immediate termination of the program with status.  If status
  is omitted it returns the canonical *success* for the system.  All Fortran
  I/O units are closed. 

  :param STATUS:
    Shall be an ``INTEGER`` of the default kind.

  :return:
    ``STATUS`` is passed to the parent process on exit.

  :samp:`{Standard}:`
    GNU extension

  :samp:`{Class}:`
    Subroutine

  :samp:`{Syntax}:`

    .. code-block:: fortran

      CALL EXIT([STATUS])

  :samp:`{Example}:`

    .. code-block:: fortran

      program test_exit
        integer :: STATUS = 0
        print *, 'This program is going to exit.'
        call EXIT(STATUS)
      end program test_exit

  :samp:`{See also}:`
    :ref:`ABORT`, 
    :ref:`KILL`

