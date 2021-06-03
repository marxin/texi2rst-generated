  .. _set_exponent:

SET_EXPONENT --- Set the exponent of the model
**********************************************

.. index:: SET_EXPONENT

.. index:: real number, set exponent

.. index:: floating point, set exponent

:samp:`{Description}:`
  ``SET_EXPONENT(X, I)`` returns the real number whose fractional part
  is that that of :samp:`{X}` and whose exponent part is :samp:`{I}`.

:samp:`{Standard}:`
  Fortran 90 and later

:samp:`{Class}:`
  Elemental function

:samp:`{Syntax}:`
  ``RESULT = SET_EXPONENT(X, I)``

:samp:`{Arguments}:`
  ===========  =============================
  :samp:`{X}`  Shall be of type ``REAL``.
  ===========  =============================
  :samp:`{I}`  Shall be of type ``INTEGER``.
  ===========  =============================

:samp:`{Return value}:`
  The return value is of the same type and kind as :samp:`{X}`.
  The real number whose fractional part
  is that that of :samp:`{X}` and whose exponent part if :samp:`{I}` is returned;
  it is ``FRACTION(X) * RADIX(X)**I``.

:samp:`{Example}:`

  .. code-block:: c++

    PROGRAM test_setexp
      REAL :: x = 178.1387e-4
      INTEGER :: i = 17
      PRINT *, SET_EXPONENT(x, i), FRACTION(x) * RADIX(x)**i
    END PROGRAM

