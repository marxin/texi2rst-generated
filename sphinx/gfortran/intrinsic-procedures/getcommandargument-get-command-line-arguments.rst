  .. _get_command_argument:

GET_COMMAND_ARGUMENT --- Get command line arguments
***************************************************

.. index:: GET_COMMAND_ARGUMENT

.. index:: command-line arguments

.. index:: arguments, to program

:samp:`{Description}:`
  Retrieve the :samp:`{NUMBER}` -th argument that was passed on the
  command line when the containing program was invoked.

:samp:`{Standard}:`
  Fortran 2003 and later

:samp:`{Class}:`
  Subroutine

:samp:`{Syntax}:`
  ``CALL GET_COMMAND_ARGUMENT(NUMBER [, VALUE, LENGTH, STATUS])``

:samp:`{Arguments}:`
  ================  ==================================================
  :samp:`{NUMBER}`  Shall be a scalar of type ``INTEGER`` and of
                    default kind, :samp:`{NUMBER}` \geq 0
  ================  ==================================================
  :samp:`{VALUE}`   (Optional) Shall be a scalar of type ``CHARACTER``
                    and of default kind.
  :samp:`{LENGTH}`  (Optional) Shall be a scalar of type ``INTEGER``
                    and of default kind.
  :samp:`{STATUS}`  (Optional) Shall be a scalar of type ``INTEGER``
                    and of default kind.
  ================  ==================================================

:samp:`{Return value}:`
  After ``GET_COMMAND_ARGUMENT`` returns, the :samp:`{VALUE}` argument holds the 
  :samp:`{NUMBER}` -th command line argument. If :samp:`{VALUE}` cannot hold the argument, it is 
  truncated to fit the length of :samp:`{VALUE}`. If there are less than :samp:`{NUMBER}`
  arguments specified at the command line, :samp:`{VALUE}` will be filled with blanks. 
  If :samp:`{NUMBER}` = 0, :samp:`{VALUE}` is set to the name of the program (on
  systems that support this feature). The :samp:`{LENGTH}` argument contains the
  length of the :samp:`{NUMBER}` -th command line argument. If the argument retrieval
  fails, :samp:`{STATUS}` is a positive number; if :samp:`{VALUE}` contains a truncated
  command line argument, :samp:`{STATUS}` is -1; and otherwise the :samp:`{STATUS}` is
  zero.

:samp:`{Example}:`

  .. code-block:: c++

    PROGRAM test_get_command_argument
      INTEGER :: i
      CHARACTER(len=32) :: arg

      i = 0
      DO
        CALL get_command_argument(i, arg)
        IF (LEN_TRIM(arg) == 0) EXIT

        WRITE (*,*) TRIM(arg)
        i = i+1
      END DO
    END PROGRAM

:samp:`{See also}:`
  GET_COMMAND, 
  COMMAND_ARGUMENT_COUNT

