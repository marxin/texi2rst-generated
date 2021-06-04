  .. _rshift:

RSHIFT --- Right shift bits
***************************

.. index:: RSHIFT

.. index:: bits, shift right

:samp:`{Description}:`
  ``RSHIFT`` returns a value corresponding to :samp:`{I}` with all of the
  bits shifted right by :samp:`{SHIFT}` places.  :samp:`{SHIFT}` shall be
  nonnegative and less than or equal to ``BIT_SIZE(I)``, otherwise
  the result value is undefined.  Bits shifted out from the right end
  are lost. The fill is arithmetic: the bits shifted in from the left
  end are equal to the leftmost bit, which in two's complement
  representation is the sign bit.

  This function has been superseded by the ``SHIFTA`` intrinsic, which
  is standard in Fortran 2008 and later.

:samp:`{Standard}:`
  GNU extension

:samp:`{Class}:`
  Elemental function

:samp:`{Syntax}:`
  ``RESULT = RSHIFT(I, SHIFT)``

:samp:`{Arguments}:`
  ===============  ==============================
  :samp:`{I}`      The type shall be ``INTEGER``.
  :samp:`{SHIFT}`  The type shall be ``INTEGER``.
  ===============  ==============================

:samp:`{Return value}:`
  The return value is of type ``INTEGER`` and of the same kind as
  :samp:`{I}`.

:samp:`{See also}:`
  ISHFT, 
  ISHFTC, 
  LSHIFT, 
  SHIFTA, 
  SHIFTR, 
  SHIFTL

