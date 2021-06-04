  .. _acosh:

ACOSH --- Inverse hyperbolic cosine function
********************************************

.. index:: ACOSH

.. index:: DACOSH

.. index:: area hyperbolic cosine

.. index:: inverse hyperbolic cosine

.. index:: hyperbolic function, cosine, inverse

.. index:: cosine, hyperbolic, inverse

:samp:`{Description}:`
  ``ACOSH(X)`` computes the inverse hyperbolic cosine of :samp:`{X}`.

:samp:`{Standard}:`
  Fortran 2008 and later

:samp:`{Class}:`
  Elemental function

:samp:`{Syntax}:`
  ``RESULT = ACOSH(X)``

:samp:`{Arguments}:`
  ===========  ==========================================
  :samp:`{X}`  The type shall be ``REAL`` or ``COMPLEX``.
  ===========  ==========================================

:samp:`{Return value}:`
  The return value has the same type and kind as :samp:`{X}`. If :samp:`{X}` is
  complex, the imaginary part of the result is in radians and lies between
  0 \leq \Im \acosh(x) \leq \pi.

:samp:`{Example}:`

  .. code-block:: fortran

    PROGRAM test_acosh
      REAL(8), DIMENSION(3) :: x = (/ 1.0, 2.0, 3.0 /)
      WRITE (*,*) ACOSH(x)
    END PROGRAM

:samp:`{Specific names}:`
  =============  =============  ===========  =============
  Name           Argument       Return type  Standard
  ``DACOSH(X)``  ``REAL(8) X``  ``REAL(8)``  GNU extension
  =============  =============  ===========  =============

:samp:`{See also}:`
  Inverse function: 
  COSH

