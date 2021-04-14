  .. _fgetc:

``FGETC`` - Read a single character in stream mode
**************************************************

.. index:: FGETC

.. index:: read character, stream mode

.. index:: stream mode, read character

.. index:: file operation, read character

:samp:`{Description}:`
  Read a single character in stream mode by bypassing normal formatted output. 
  Stream I/O should not be mixed with normal record-oriented (formatted or 
  unformatted) I/O on the same unit; the results are unpredictable.

  This intrinsic is provided in both subroutine and function forms; however,
  only one form can be used in any given program unit.

  Note that the ``FGET`` intrinsic is provided for backwards compatibility
  with :command:`g77`.  GNU Fortran provides the Fortran 2003 Stream facility.
  Programmers should consider the use of new stream IO feature in new code 
  for future portability. See also Fortran 2003 status.

:samp:`{Standard}:`
  GNU extension

:samp:`{Class}:`
  Subroutine, function

:samp:`{Syntax}:`
  ==================================
  ``CALL FGETC(UNIT, C [, STATUS])``
  ==================================
  ``STATUS = FGETC(UNIT, C)``
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

  .. code-block:: c++

    PROGRAM test_fgetc
      INTEGER :: fd = 42, status
      CHARACTER :: c

      OPEN(UNIT=fd, FILE="/etc/passwd", ACTION="READ", STATUS = "OLD")
      DO
        CALL fgetc(fd, c, status)
        IF (status /= 0) EXIT
        call fput(c)
      END DO
      CLOSE(UNIT=fd)
    END PROGRAM

:samp:`{See also}:`
  FGET, 
  FPUT, 
  FPUTC

