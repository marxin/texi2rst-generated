  .. _lstat:

LSTAT --- Get file status
*************************

.. index:: LSTAT

.. index:: file system, file status

:samp:`{Description}:`
  ``LSTAT`` is identical to STAT, except that if path is a
  symbolic link, then the link itself is statted, not the file that it
  refers to.

  The elements in ``VALUES`` are the same as described by STAT.

  This intrinsic is provided in both subroutine and function forms;
  however, only one form can be used in any given program unit.

:samp:`{Standard}:`
  GNU extension

:samp:`{Class}:`
  Subroutine, function

:samp:`{Syntax}:`
  =======================================
  ``CALL LSTAT(NAME, VALUES [, STATUS])``
  ``STATUS = LSTAT(NAME, VALUES)``
  =======================================

:samp:`{Arguments}:`
  ================  ================================================================
  :samp:`{NAME}`    The type shall be ``CHARACTER`` of the default
                    kind, a valid path within the file system.
  :samp:`{VALUES}`  The type shall be ``INTEGER(4), DIMENSION(13)``.
  :samp:`{STATUS}`  (Optional) status flag of type ``INTEGER(4)``.
                    Returns 0 on success and a system specific error code otherwise.
  ================  ================================================================

:samp:`{Example}:`
  See STAT for an example.

:samp:`{See also}:`
  To stat an open file: 
  FSTAT 
  To stat a file: 
  STAT

