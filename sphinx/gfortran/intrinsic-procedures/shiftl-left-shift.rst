  .. _shiftl:

SHIFTL --- Left shift
*********************

.. index:: SHIFTL

.. index:: bits, shift left

.. index:: shift, left

:samp:`{Description}:`
  ``SHIFTL`` returns a value corresponding to :samp:`{I}` with all of the
  bits shifted left by :samp:`{SHIFT}` places.  :samp:`{SHIFT}` shall be
  nonnegative and less than or equal to ``BIT_SIZE(I)``, otherwise
  the result value is undefined.  Bits shifted out from the left end are
  lost, and bits shifted in from the right end are set to 0.

:samp:`{Standard}:`
  Fortran 2008 and later

:samp:`{Class}:`
  Elemental function

:samp:`{Syntax}:`
  ``RESULT = SHIFTL(I, SHIFT)``

:samp:`{Arguments}:`
  ===============  ==============================
  :samp:`{I}`      The type shall be ``INTEGER``.
  :samp:`{SHIFT}`  The type shall be ``INTEGER``.
  ===============  ==============================

:samp:`{Return value}:`
  The return value is of type ``INTEGER`` and of the same kind as
  :samp:`{I}`.

:samp:`{See also}:`
  SHIFTA, 
  SHIFTR

