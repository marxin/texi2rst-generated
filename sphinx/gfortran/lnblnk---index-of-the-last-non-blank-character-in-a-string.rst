  .. _lnblnk:

``LNBLNK`` - Index of the last non-blank character in a string
**************************************************************

.. index:: LNBLNK

.. index:: string, find non-blank character

:samp:`{Description}:`
  Returns the length of a character string, ignoring any trailing blanks.
  This is identical to the standard ``LEN_TRIM`` intrinsic, and is only
  included for backwards compatibility.

:samp:`{Standard}:`
  GNU extension

:samp:`{Class}:`
  Elemental function

:samp:`{Syntax}:`
  ``RESULT = LNBLNK(STRING)``

:samp:`{Arguments}:`
  ================  ========================================
  :samp:`{STRING}`  Shall be a scalar of type ``CHARACTER``,
                    with ``INTENT(IN)``
  ================  ========================================
  ================  ========================================

:samp:`{Return value}:`
  The return value is of ``INTEGER(kind=4)`` type.

:samp:`{See also}:`
  INDEX intrinsic, 
  LEN_TRIM

