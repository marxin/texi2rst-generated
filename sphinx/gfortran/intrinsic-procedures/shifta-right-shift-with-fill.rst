  .. _shifta:

SHIFTA --- Right shift with fill
********************************

.. index:: SHIFTA

.. index:: bits, shift right

.. index:: shift, right with fill

:samp:`{Description}:`
  ``SHIFTA`` returns a value corresponding to :samp:`{I}` with all of the
  bits shifted right by :samp:`{SHIFT}` places.  :samp:`{SHIFT}` that be
  nonnegative and less than or equal to ``BIT_SIZE(I)``, otherwise
  the result value is undefined.  Bits shifted out from the right end
  are lost. The fill is arithmetic: the bits shifted in from the left
  end are equal to the leftmost bit, which in two's complement
  representation is the sign bit.

:samp:`{Standard}:`
  Fortran 2008 and later

:samp:`{Class}:`
  Elemental function

:samp:`{Syntax}:`
  ``RESULT = SHIFTA(I, SHIFT)``

:samp:`{Arguments}:`
  ===============  ==============================
  :samp:`{I}`      The type shall be ``INTEGER``.
  ===============  ==============================
  :samp:`{SHIFT}`  The type shall be ``INTEGER``.
  ===============  ==============================

:samp:`{Return value}:`
  The return value is of type ``INTEGER`` and of the same kind as
  :samp:`{I}`.

:samp:`{See also}:`
  SHIFTL, 
  SHIFTR

