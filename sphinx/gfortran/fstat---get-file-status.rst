  .. _fstat:

``FSTAT`` - Get file status
***************************

.. index:: FSTAT

.. index:: file system, file status

:samp:`{Description}:`
  ``FSTAT`` is identical to STAT, except that information about an 
  already opened file is obtained.

  The elements in ``VALUES`` are the same as described by STAT.

  This intrinsic is provided in both subroutine and function forms; however,
  only one form can be used in any given program unit.

:samp:`{Standard}:`
  GNU extension

:samp:`{Class}:`
  Subroutine, function

:samp:`{Syntax}:`
  =======================================
  ``CALL FSTAT(UNIT, VALUES [, STATUS])``
  =======================================
  ``STATUS = FSTAT(UNIT, VALUES)``
  =======================================

:samp:`{Arguments}:`
  ================  =========================================================
  :samp:`{UNIT}`    An open I/O unit number of type ``INTEGER``.
  ================  =========================================================
  :samp:`{VALUES}`  The type shall be ``INTEGER(4), DIMENSION(13)``.
  :samp:`{STATUS}`  (Optional) status flag of type ``INTEGER(4)``. Returns 0 
                    on success and a system specific error code otherwise.
  ================  =========================================================

:samp:`{Example}:`
  See STAT for an example.

:samp:`{See also}:`
  To stat a link: 
  LSTAT 
  To stat a file: 
  STAT

