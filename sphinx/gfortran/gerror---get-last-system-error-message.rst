  .. _gerror:

GERROR - Get last system error message
**************************************

.. index:: GERROR

.. index:: system, error handling

:samp:`{Description}:`
  Returns the system error message corresponding to the last system error.
  This resembles the functionality of ``strerror(3)`` in C.

:samp:`{Standard}:`
  GNU extension

:samp:`{Class}:`
  Subroutine

:samp:`{Syntax}:`
  ``CALL GERROR(RESULT)``

:samp:`{Arguments}:`
  ================  ==========================================
  :samp:`{RESULT}`  Shall of type ``CHARACTER`` and of default
  ================  ==========================================
  ================  ==========================================

:samp:`{Example}:`

  .. code-block:: c++

    PROGRAM test_gerror
      CHARACTER(len=100) :: msg
      CALL gerror(msg)
      WRITE(*,*) msg
    END PROGRAM

:samp:`{See also}:`
  IERRNO, 
  PERROR

