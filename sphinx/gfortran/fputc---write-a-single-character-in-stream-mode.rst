  .. _fputc:

``FPUTC`` - Write a single character in stream mode
***************************************************

.. index:: FPUTC

.. index:: write character, stream mode

.. index:: stream mode, write character

.. index:: file operation, write character

:samp:`{Description}:`
  Write a single character in stream mode by bypassing normal formatted 
  output. Stream I/O should not be mixed with normal record-oriented 
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
  ==================================
  ``CALL FPUTC(UNIT, C [, STATUS])``
  ==================================
  ``STATUS = FPUTC(UNIT, C)``
  ==================================

:samp:`{Arguments}:`
  ================  ======================================================================
  :samp:`{UNIT}`    The type shall be ``INTEGER``.
  ================  ======================================================================
  :samp:`{C}`       The type shall be ``CHARACTER`` and of default
                    kind.
  :samp:`{STATUS}`  (Optional) status flag of type ``INTEGER``.
                    Returns 0 on success, -1 on end-of-file and a system specific positive
                    error code otherwise.
  ================  ======================================================================

:samp:`{Example}:`

  .. code-block:: fortran

    PROGRAM test_fputc
      CHARACTER(len=10) :: str = "gfortran"
      INTEGER :: fd = 42, i

      OPEN(UNIT = fd, FILE = "out", ACTION = "WRITE", STATUS="NEW")
      DO i = 1, len_trim(str)
        CALL fputc(fd, str(i:i))
      END DO
      CLOSE(fd)
    END PROGRAM

:samp:`{See also}:`
  FPUT, 
  FGET, 
  FGETC

