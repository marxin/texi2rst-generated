.. _gerror:

GERROR --- Get last system error message
****************************************

.. index:: GERROR

.. index:: system, error handling

.. function:: GERROR

  Returns the system error message corresponding to the last system error.
  This resembles the functionality of ``strerror(3)`` in C.

  :param RESULT:
    Shall be of type ``CHARACTER`` and of default kind.

  :samp:`{Standard}:`
    GNU extension

  :samp:`{Class}:`
    Subroutine

  :samp:`{Syntax}:`

  .. code-block:: fortran

    CALL GERROR(RESULT)

  :samp:`{Example}:`

    .. code-block:: fortran

      PROGRAM test_gerror
        CHARACTER(len=100) :: msg
        CALL gerror(msg)
        WRITE(*,*) msg
      END PROGRAM

  :samp:`{See also}:`
    IERRNO, 
    PERROR

