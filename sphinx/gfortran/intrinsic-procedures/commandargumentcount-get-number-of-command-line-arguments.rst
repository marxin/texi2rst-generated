  .. _command_argument_count:

COMMAND_ARGUMENT_COUNT - Get number of command line arguments
*************************************************************

.. index:: COMMAND_ARGUMENT_COUNT

.. index:: command-line arguments

.. index:: command-line arguments, number of

.. index:: arguments, to program

:samp:`{Description}:`
  ``COMMAND_ARGUMENT_COUNT`` returns the number of arguments passed on the
  command line when the containing program was invoked.

:samp:`{Standard}:`
  Fortran 2003 and later

:samp:`{Class}:`
  Inquiry function

:samp:`{Syntax}:`
  ``RESULT = COMMAND_ARGUMENT_COUNT()``

:samp:`{Arguments}:`
  ====
  None
  ====
  ====

:samp:`{Return value}:`
  The return value is an ``INTEGER`` of default kind.

:samp:`{Example}:`

  .. code-block:: c++

    program test_command_argument_count
        integer :: count
        count = command_argument_count()
        print *, count
    end program test_command_argument_count

:samp:`{See also}:`
  GET_COMMAND, 
  GET_COMMAND_ARGUMENT

