  .. _fraction:

FRACTION --- Fractional part of the model representation
********************************************************

.. index:: FRACTION

.. index:: real number, fraction

.. index:: floating point, fraction

:samp:`{Description}:`
  ``FRACTION(X)`` returns the fractional part of the model
  representation of ``X``.

:samp:`{Standard}:`
  Fortran 90 and later

:samp:`{Class}:`
  Elemental function

:samp:`{Syntax}:`
  ``Y = FRACTION(X)``

:samp:`{Arguments}:`
  ===========  =============================================
  :samp:`{X}`  The type of the argument shall be a ``REAL``.
  ===========  =============================================

:samp:`{Return value}:`
  The return value is of the same type and kind as the argument.
  The fractional part of the model representation of ``X`` is returned;
  it is ``X * RADIX(X)**(-EXPONENT(X))``.

:samp:`{Example}:`

  .. code-block:: fortran

    program test_fraction
      real :: x
      x = 178.1387e-4
      print *, fraction(x), x * radix(x)**(-exponent(x))
    end program test_fraction

