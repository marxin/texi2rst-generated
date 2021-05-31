  .. _unlink:

UNLINK - Remove a file from the file system
*******************************************

.. index:: UNLINK

.. index:: file system, remove file

:samp:`{Description}:`
  Unlinks the file :samp:`{PATH}`. A null character ( ``CHAR(0)`` ) can be
  used to mark the end of the name in :samp:`{PATH}` ; otherwise, trailing
  blanks in the file name are ignored.  If the :samp:`{STATUS}` argument is
  supplied, it contains 0 on success or a nonzero error code upon return;
  see ``unlink(2)``.

  This intrinsic is provided in both subroutine and function forms;
  however, only one form can be used in any given program unit.

:samp:`{Standard}:`
  GNU extension

:samp:`{Class}:`
  Subroutine, function

:samp:`{Syntax}:`
  ================================
  ``CALL UNLINK(PATH [, STATUS])``
  ================================
  ``STATUS = UNLINK(PATH)``
  ================================

:samp:`{Arguments}:`
  ================  ================================================
  :samp:`{PATH}`    Shall be of default ``CHARACTER`` type.
  ================  ================================================
  :samp:`{STATUS}`  (Optional) Shall be of default ``INTEGER`` type.
  ================  ================================================

:samp:`{See also}:`
  LINK, 
  SYMLNK

