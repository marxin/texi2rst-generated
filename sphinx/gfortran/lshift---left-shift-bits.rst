  .. _lshift:

``LSHIFT`` - Left shift bits
****************************

.. index:: LSHIFT

.. index:: bits, shift left

:samp:`{Description}:`
  ``LSHIFT`` returns a value corresponding to :samp:`{I}` with all of the
  bits shifted left by :samp:`{SHIFT}` places.  :samp:`{SHIFT}` shall be
  nonnegative and less than or equal to ``BIT_SIZE(I)``, otherwise
  the result value is undefined.  Bits shifted out from the left end are
  lost; zeros are shifted in from the opposite end.

  This function has been superseded by the ``ISHFT`` intrinsic, which
  is standard in Fortran 95 and later, and the ``SHIFTL`` intrinsic,
  which is standard in Fortran 2008 and later.

:samp:`{Standard}:`
  GNU extension

:samp:`{Class}:`
  Elemental function

:samp:`{Syntax}:`
  ``RESULT = LSHIFT(I, SHIFT)``

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
  ISHFT, 
  ISHFTC, 
  RSHIFT, 
  SHIFTA, 
  SHIFTL, 
  SHIFTR

