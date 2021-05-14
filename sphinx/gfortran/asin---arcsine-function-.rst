  .. _asin:

ASIN - Arcsine function 
************************

.. index:: ASIN

.. index:: DASIN

.. index:: trigonometric function, sine, inverse

.. index:: sine, inverse

:samp:`{Description}:`
  ``ASIN(X)`` computes the arcsine of its :samp:`{X}` (inverse of ``SIN(X)`` ).

:samp:`{Standard}:`
  Fortran 77 and later, for a complex argument Fortran 2008 or later

:samp:`{Class}:`
  Elemental function

:samp:`{Syntax}:`
  ``RESULT = ASIN(X)``

:samp:`{Arguments}:`
  ===========  =========================================================
  :samp:`{X}`  The type shall be either ``REAL`` and a magnitude that is
               less than or equal to one - or be ``COMPLEX``.
  ===========  =========================================================
  ===========  =========================================================

:samp:`{Return value}:`
  The return value is of the same type and kind as :samp:`{X}`.
  The real part of the result is in radians and lies in the range
  -\pi/2 \leq \Re \asin(x) \leq \pi/2.

:samp:`{Example}:`

  .. code-block:: c++

    program test_asin
      real(8) :: x = 0.866_8
      x = asin(x)
    end program test_asin

:samp:`{Specific names}:`
  ============  =============  ===========  ====================
  Name          Argument       Return type  Standard
  ============  =============  ===========  ====================
  ``ASIN(X)``   ``REAL(4) X``  ``REAL(4)``  Fortran 77 and later
  ``DASIN(X)``  ``REAL(8) X``  ``REAL(8)``  Fortran 77 and later
  ============  =============  ===========  ====================

:samp:`{See also}:`
  Inverse function: 
  SIN 
  Degrees function: 
  ASIND

