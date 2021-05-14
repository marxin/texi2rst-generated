  .. _iargc:

IARGC - Get the number of command line arguments
************************************************

.. index:: IARGC

.. index:: command-line arguments

.. index:: command-line arguments, number of

.. index:: arguments, to program

:samp:`{Description}:`
  ``IARGC`` returns the number of arguments passed on the
  command line when the containing program was invoked.

  This intrinsic routine is provided for backwards compatibility with 
  GNU Fortran 77.  In new code, programmers should consider the use of 
  the COMMAND_ARGUMENT_COUNT intrinsic defined by the Fortran 2003 
  standard.

:samp:`{Standard}:`
  GNU extension

:samp:`{Class}:`
  Function

:samp:`{Syntax}:`
  ``RESULT = IARGC()``

:samp:`{Arguments}:`
  None

:samp:`{Return value}:`
  The number of command line arguments, type ``INTEGER(4)``.

:samp:`{Example}:`
  See GETARG

:samp:`{See also}:`
  GNU Fortran 77 compatibility subroutine: 
  GETARG 
  Fortran 2003 functions and subroutines: 
  GET_COMMAND, 
  GET_COMMAND_ARGUMENT, 
  COMMAND_ARGUMENT_COUNT

