  .. _len_trim:

LEN_TRIM - Length of a character entity without trailing blank characters
*************************************************************************

.. index:: LEN_TRIM

.. index:: string, length, without trailing whitespace

:samp:`{Description}:`
  Returns the length of a character string, ignoring any trailing blanks.

:samp:`{Standard}:`
  Fortran 90 and later, with :samp:`{KIND}` argument Fortran 2003 and later

:samp:`{Class}:`
  Elemental function

:samp:`{Syntax}:`
  ``RESULT = LEN_TRIM(STRING [, KIND])``

:samp:`{Arguments}:`
  ================  =======================================================
  :samp:`{STRING}`  Shall be a scalar of type ``CHARACTER``,
                    with ``INTENT(IN)``
  ================  =======================================================
  :samp:`{KIND}`    (Optional) An ``INTEGER`` initialization
                    expression indicating the kind parameter of the result.
  ================  =======================================================

:samp:`{Return value}:`
  The return value is of type ``INTEGER`` and of kind :samp:`{KIND}`. If
  :samp:`{KIND}` is absent, the return value is of default integer kind.

:samp:`{See also}:`
  LEN, 
  ADJUSTL, 
  ADJUSTR

