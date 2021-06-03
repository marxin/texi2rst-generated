  .. _fget:

FGET --- Read a single character in stream mode from stdin 
***********************************************************

.. index:: FGET

.. index:: read character, stream mode

.. index:: stream mode, read character

.. index:: file operation, read character

:samp:`{Description}:`
  Read a single character in stream mode from stdin by bypassing normal 
  formatted output. Stream I/O should not be mixed with normal record-oriented 
  (formatted or unformatted) I/O on the same unit; the results are unpredictable.

  This intrinsic is provided in both subroutine and function forms; however,
  only one form can be used in any given program unit.

  Note that the ``FGET`` intrinsic is provided for backwards compatibility with 
  :command:`g77`.  GNU Fortran provides the Fortran 2003 Stream facility.
  Programmers should consider the use of new stream IO feature in new code 
  for future portability. See also Fortran 2003 status.

:samp:`{Standard}:`
  GNU extension

:samp:`{Class}:`
  Subroutine, function

:samp:`{Syntax}:`
  ===========================
  ``CALL FGET(C [, STATUS])``
  ===========================
  ``STATUS = FGET(C)``
  ===========================

:samp:`{Arguments}:`
  ================  =======================================================================
  :samp:`{C}`       The type shall be ``CHARACTER`` and of default
                    kind.
  ================  =======================================================================
  :samp:`{STATUS}`  (Optional) status flag of type ``INTEGER``.
                    Returns 0 on success, -1 on end-of-file, and a system specific positive
                    error code otherwise.
  ================  =======================================================================

:samp:`{Example}:`

  .. code-block:: c++

    PROGRAM test_fget
      INTEGER, PARAMETER :: strlen = 100
      INTEGER :: status, i = 1
      CHARACTER(len=strlen) :: str = ""

      WRITE (*,*) 'Enter text:'
      DO
        CALL fget(str(i:i), status)
        if (status /= 0 .OR. i > strlen) exit
        i = i + 1
      END DO
      WRITE (*,*) TRIM(str)
    END PROGRAM

:samp:`{See also}:`
  FGETC, 
  FPUT, 
  FPUTC

