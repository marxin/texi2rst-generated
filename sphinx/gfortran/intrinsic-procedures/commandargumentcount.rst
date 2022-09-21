..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the copyright.rst file.

.. _command_argument_count:

.. index:: COMMAND_ARGUMENT_COUNT

.. index:: command-line arguments

.. index:: command-line arguments, number of

.. index:: arguments, to program

COMMAND_ARGUMENT_COUNT --- Get number of command line arguments
***************************************************************

.. function:: COMMAND_ARGUMENT_COUNT

  ``COMMAND_ARGUMENT_COUNT`` returns the number of arguments passed on the
  command line when the containing program was invoked.

  :return:
    The return value is an ``INTEGER`` of default kind.

  :samp:`{Standard}:`
    Fortran 2003 and later

  :samp:`{Class}:`
    Inquiry function

  :samp:`{Syntax}:`

    .. code-block:: fortran

      RESULT = COMMAND_ARGUMENT_COUNT()

  :samp:`{Example}:`

    .. code-block:: fortran

      program test_command_argument_count
          integer :: count
          count = command_argument_count()
          print *, count
      end program test_command_argument_count

  :samp:`{See also}:`
    :ref:`GET_COMMAND`, 
    :ref:`GET_COMMAND_ARGUMENT`

