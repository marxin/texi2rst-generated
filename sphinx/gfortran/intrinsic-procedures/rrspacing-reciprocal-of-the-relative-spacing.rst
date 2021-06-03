  .. _rrspacing:

RRSPACING --- Reciprocal of the relative spacing
************************************************

.. index:: RRSPACING

.. index:: real number, relative spacing

.. index:: floating point, relative spacing

:samp:`{Description}:`
  ``RRSPACING(X)`` returns the  reciprocal of the relative spacing of
  model numbers near :samp:`{X}`.

:samp:`{Standard}:`
  Fortran 90 and later

:samp:`{Class}:`
  Elemental function

:samp:`{Syntax}:`
  ``RESULT = RRSPACING(X)``

:samp:`{Arguments}:`
  ===========  ==========================
  :samp:`{X}`  Shall be of type ``REAL``.
  ===========  ==========================
  ===========  ==========================

:samp:`{Return value}:`
  The return value is of the same type and kind as :samp:`{X}`.
  The value returned is equal to
  ``ABS(FRACTION(X)) * FLOAT(RADIX(X))**DIGITS(X)``.

:samp:`{See also}:`
  SPACING

