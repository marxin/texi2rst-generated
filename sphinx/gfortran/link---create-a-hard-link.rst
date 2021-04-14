  .. _link:

``LINK`` - Create a hard link
*****************************

.. index:: LINK

.. index:: file system, create link

.. index:: file system, hard link

:samp:`{Description}:`
  Makes a (hard) link from file :samp:`{PATH1}` to :samp:`{PATH2}`. A null
  character (``CHAR(0)``) can be used to mark the end of the names in
  :samp:`{PATH1}` and :samp:`{PATH2}` ; otherwise, trailing blanks in the file
  names are ignored.  If the :samp:`{STATUS}` argument is supplied, it
  contains 0 on success or a nonzero error code upon return; see
  ``link(2)``.

  This intrinsic is provided in both subroutine and function forms;
  however, only one form can be used in any given program unit.

:samp:`{Standard}:`
  GNU extension

:samp:`{Class}:`
  Subroutine, function

:samp:`{Syntax}:`
  ======================================
  ``CALL LINK(PATH1, PATH2 [, STATUS])``
  ======================================
  ``STATUS = LINK(PATH1, PATH2)``
  ======================================

:samp:`{Arguments}:`
  ================  ================================================
  :samp:`{PATH1}`   Shall be of default ``CHARACTER`` type.
  ================  ================================================
  :samp:`{PATH2}`   Shall be of default ``CHARACTER`` type.
  :samp:`{STATUS}`  (Optional) Shall be of default ``INTEGER`` type.
  ================  ================================================

:samp:`{See also}:`
  SYMLNK, 
  UNLINK

