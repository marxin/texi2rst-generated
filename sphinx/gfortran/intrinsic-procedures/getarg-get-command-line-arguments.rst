  .. _getarg:

GETARG --- Get command line arguments
*************************************

.. index:: GETARG

.. index:: command-line arguments

.. index:: arguments, to program

:samp:`{Description}:`
  Retrieve the :samp:`{POS}` -th argument that was passed on the
  command line when the containing program was invoked.

  This intrinsic routine is provided for backwards compatibility with 
  GNU Fortran 77.  In new code, programmers should consider the use of 
  the GET_COMMAND_ARGUMENT intrinsic defined by the Fortran 2003 
  standard.

:samp:`{Standard}:`
  GNU extension

:samp:`{Class}:`
  Subroutine

:samp:`{Syntax}:`
  ``CALL GETARG(POS, VALUE)``

:samp:`{Arguments}:`
  ===============  ===============================================
  :samp:`{POS}`    Shall be of type ``INTEGER`` and not wider than
                   the default integer kind; :samp:`{POS}` \geq 0
  :samp:`{VALUE}`  Shall be of type ``CHARACTER`` and of default
                   kind.
  ===============  ===============================================

:samp:`{Return value}:`
  After ``GETARG`` returns, the :samp:`{VALUE}` argument holds the
  :samp:`{POS}` th command line argument. If :samp:`{VALUE}` cannot hold the
  argument, it is truncated to fit the length of :samp:`{VALUE}`. If there are
  less than :samp:`{POS}` arguments specified at the command line, :samp:`{VALUE}`
  will be filled with blanks. If :samp:`{POS}` = 0, :samp:`{VALUE}` is set
  to the name of the program (on systems that support this feature).

:samp:`{Example}:`

  .. code-block:: fortran

    PROGRAM test_getarg
      INTEGER :: i
      CHARACTER(len=32) :: arg

      DO i = 1, iargc()
        CALL getarg(i, arg)
        WRITE (*,*) arg
      END DO
    END PROGRAM

:samp:`{See also}:`
  GNU Fortran 77 compatibility function: 
  IARGC 
  Fortran 2003 functions and subroutines: 
  GET_COMMAND, 
  GET_COMMAND_ARGUMENT, 
  COMMAND_ARGUMENT_COUNT

