  .. _cosh:

COSH --- Hyperbolic cosine function
***********************************

.. index:: COSH

.. index:: DCOSH

.. index:: hyperbolic cosine

.. index:: hyperbolic function, cosine

.. index:: cosine, hyperbolic

:samp:`{Description}:`
  ``COSH(X)`` computes the hyperbolic cosine of :samp:`{X}`.

:samp:`{Standard}:`
  Fortran 77 and later, for a complex argument Fortran 2008 or later

:samp:`{Class}:`
  Elemental function

:samp:`{Syntax}:`
  ``X = COSH(X)``

:samp:`{Arguments}:`
  ===========  ==========================================
  :samp:`{X}`  The type shall be ``REAL`` or ``COMPLEX``.
  ===========  ==========================================
  ===========  ==========================================

:samp:`{Return value}:`
  The return value has same type and kind as :samp:`{X}`. If :samp:`{X}` is
  complex, the imaginary part of the result is in radians. If :samp:`{X}`
  is ``REAL``, the return value has a lower bound of one,
  \cosh (x) \geq 1.

:samp:`{Example}:`

  .. code-block:: fortran

    program test_cosh
      real(8) :: x = 1.0_8
      x = cosh(x)
    end program test_cosh

:samp:`{Specific names}:`
  ============  =============  ===========  ====================
  Name          Argument       Return type  Standard
  ============  =============  ===========  ====================
  ``COSH(X)``   ``REAL(4) X``  ``REAL(4)``  Fortran 77 and later
  ``DCOSH(X)``  ``REAL(8) X``  ``REAL(8)``  Fortran 77 and later
  ============  =============  ===========  ====================

:samp:`{See also}:`
  Inverse function: 
  ACOSH

