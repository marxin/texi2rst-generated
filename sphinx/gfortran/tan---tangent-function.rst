  .. _tan:

TAN - Tangent function
**********************

.. index:: TAN

.. index:: DTAN

.. index:: trigonometric function, tangent

.. index:: tangent

:samp:`{Description}:`
  ``TAN(X)`` computes the tangent of :samp:`{X}`.

:samp:`{Standard}:`
  Fortran 77 and later, for a complex argument Fortran 2008 or later

:samp:`{Class}:`
  Elemental function

:samp:`{Syntax}:`
  ``RESULT = TAN(X)``

:samp:`{Arguments}:`
  ===========  ==========================================
  :samp:`{X}`  The type shall be ``REAL`` or ``COMPLEX``.
  ===========  ==========================================
  ===========  ==========================================

:samp:`{Return value}:`
  The return value has same type and kind as :samp:`{X}`, and its value is in radians.

:samp:`{Example}:`

  .. code-block:: c++

    program test_tan
      real(8) :: x = 0.165_8
      x = tan(x)
    end program test_tan

:samp:`{Specific names}:`
  ===========  =============  ===========  ====================
  Name         Argument       Return type  Standard
  ===========  =============  ===========  ====================
  ``TAN(X)``   ``REAL(4) X``  ``REAL(4)``  Fortran 77 and later
  ``DTAN(X)``  ``REAL(8) X``  ``REAL(8)``  Fortran 77 and later
  ===========  =============  ===========  ====================

:samp:`{See also}:`
  Inverse function: 
  ATAN 
  Degrees function: 
  TAND

