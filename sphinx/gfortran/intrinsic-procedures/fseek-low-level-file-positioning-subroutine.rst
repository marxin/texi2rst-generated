  .. _fseek:

FSEEK --- Low level file positioning subroutine
***********************************************

.. index:: FSEEK

.. index:: file operation, seek

.. index:: file operation, position

:samp:`{Description}:`
  Moves :samp:`{UNIT}` to the specified :samp:`{OFFSET}`. If :samp:`{WHENCE}` 
  is set to 0, the :samp:`{OFFSET}` is taken as an absolute value ``SEEK_SET``,
  if set to 1, :samp:`{OFFSET}` is taken to be relative to the current position 
  ``SEEK_CUR``, and if set to 2 relative to the end of the file ``SEEK_END``.
  On error, :samp:`{STATUS}` is set to a nonzero value. If :samp:`{STATUS}` the seek 
  fails silently.

  This intrinsic routine is not fully backwards compatible with :command:`g77`. 
  In :command:`g77`, the ``FSEEK`` takes a statement label instead of a 
  :samp:`{STATUS}` variable. If FSEEK is used in old code, change

  .. code-block:: fortran

      CALL FSEEK(UNIT, OFFSET, WHENCE, *label)

  to

  .. code-block:: fortran

      INTEGER :: status
      CALL FSEEK(UNIT, OFFSET, WHENCE, status)
      IF (status /= 0) GOTO label

  Please note that GNU Fortran provides the Fortran 2003 Stream facility.
  Programmers should consider the use of new stream IO feature in new code 
  for future portability. See also Fortran 2003 status.

:samp:`{Standard}:`
  GNU extension

:samp:`{Class}:`
  Subroutine

:samp:`{Syntax}:`
  ``CALL FSEEK(UNIT, OFFSET, WHENCE[, STATUS])``

:samp:`{Arguments}:`
  ================  ======================================
  :samp:`{UNIT}`    Shall be a scalar of type ``INTEGER``.
  ================  ======================================
  :samp:`{OFFSET}`  Shall be a scalar of type ``INTEGER``.
  :samp:`{WHENCE}`  Shall be a scalar of type ``INTEGER``.
                    Its value shall be either 0, 1 or 2.
  :samp:`{STATUS}`  (Optional) shall be a scalar of type 
                    ``INTEGER(4)``.
  ================  ======================================

:samp:`{Example}:`

  .. code-block:: fortran

    PROGRAM test_fseek
      INTEGER, PARAMETER :: SEEK_SET = 0, SEEK_CUR = 1, SEEK_END = 2
      INTEGER :: fd, offset, ierr

      ierr   = 0
      offset = 5
      fd     = 10

      OPEN(UNIT=fd, FILE="fseek.test")
      CALL FSEEK(fd, offset, SEEK_SET, ierr)  ! move to OFFSET
      print *, FTELL(fd), ierr

      CALL FSEEK(fd, 0, SEEK_END, ierr)       ! move to end
      print *, FTELL(fd), ierr

      CALL FSEEK(fd, 0, SEEK_SET, ierr)       ! move to beginning
      print *, FTELL(fd), ierr

      CLOSE(UNIT=fd)
    END PROGRAM

:samp:`{See also}:`
  FTELL

