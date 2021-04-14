  .. _shiftr:

``SHIFTR`` - Right shift
************************

.. index:: SHIFTR

.. index:: bits, shift right

.. index:: shift, right

:samp:`{Description}:`
  ``SHIFTR`` returns a value corresponding to :samp:`{I}` with all of the
  bits shifted right by :samp:`{SHIFT}` places.  :samp:`{SHIFT}` shall be
  nonnegative and less than or equal to ``BIT_SIZE(I)``, otherwise
  the result value is undefined.  Bits shifted out from the right end
  are lost, and bits shifted in from the left end are set to 0.

:samp:`{Standard}:`
  Fortran 2008 and later

:samp:`{Class}:`
  Elemental function

:samp:`{Syntax}:`
  ``RESULT = SHIFTR(I, SHIFT)``

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
  SHIFTA, 
  SHIFTL

