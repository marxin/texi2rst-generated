  .. _minval:

MINVAL --- Minimum value of an array
************************************

.. index:: MINVAL

.. index:: array, minimum value

.. index:: minimum value

:samp:`{Description}:`
  Determines the minimum value of the elements in an array value, or, if
  the :samp:`{DIM}` argument is supplied, determines the minimum value along
  each row of the array in the :samp:`{DIM}` direction.  If :samp:`{MASK}` is
  present, only the elements for which :samp:`{MASK}` is ``.TRUE.`` are
  considered.  If the array has zero size, or all of the elements of
  :samp:`{MASK}` are ``.FALSE.``, then the result is ``HUGE(ARRAY)`` if
  :samp:`{ARRAY}` is numeric, or a string of ``CHAR(255)`` characters if
  :samp:`{ARRAY}` is of character type.

:samp:`{Standard}:`
  Fortran 90 and later

:samp:`{Class}:`
  Transformational function

:samp:`{Syntax}:`
  ========================================
  ``RESULT = MINVAL(ARRAY, DIM [, MASK])``
  ``RESULT = MINVAL(ARRAY [, MASK])``
  ========================================

:samp:`{Arguments}:`
  ===============  ======================================================================
  :samp:`{ARRAY}`  Shall be an array of type ``INTEGER`` or
                   ``REAL``.
  :samp:`{DIM}`    (Optional) Shall be a scalar of type
                   ``INTEGER``, with a value between one and the rank of :samp:`{ARRAY}`,
                   inclusive.  It may not be an optional dummy argument.
  :samp:`{MASK}`   Shall be of type ``LOGICAL``,
                   and conformable with :samp:`{ARRAY}`.
  ===============  ======================================================================

:samp:`{Return value}:`
  If :samp:`{DIM}` is absent, or if :samp:`{ARRAY}` has a rank of one, the result
  is a scalar.  If :samp:`{DIM}` is present, the result is an array with a
  rank one less than the rank of :samp:`{ARRAY}`, and a size corresponding to
  the size of :samp:`{ARRAY}` with the :samp:`{DIM}` dimension removed.  In all
  cases, the result is of the same type and kind as :samp:`{ARRAY}`.

:samp:`{See also}:`
  MIN, 
  MINLOC

