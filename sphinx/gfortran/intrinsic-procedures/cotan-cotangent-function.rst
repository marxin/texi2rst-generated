  .. _cotan:

COTAN --- Cotangent function
****************************

.. index:: COTAN

.. index:: DCOTAN

.. index:: trigonometric function, cotangent

.. index:: cotangent

:samp:`{Description}:`
  ``COTAN(X)`` computes the cotangent of :samp:`{X}`. Equivalent to ``COS(x)``
  divided by ``SIN(x)``, or ``1 / TAN(x)``.

  This function is for compatibility only and should be avoided in favor of
  standard constructs wherever possible.

:samp:`{Standard}:`
  GNU extension, enabled with :option:`-fdec-math`.

:samp:`{Class}:`
  Elemental function

:samp:`{Syntax}:`
  ``RESULT = COTAN(X)``

:samp:`{Arguments}:`
  ===========  ==========================================
  :samp:`{X}`  The type shall be ``REAL`` or ``COMPLEX``.
  ===========  ==========================================

:samp:`{Return value}:`
  The return value has same type and kind as :samp:`{X}`, and its value is in radians.

:samp:`{Example}:`

  .. code-block:: fortran

    program test_cotan
      real(8) :: x = 0.165_8
      x = cotan(x)
    end program test_cotan

:samp:`{Specific names}:`
  =============  =============  ===========  =============
  Name           Argument       Return type  Standard
  ``COTAN(X)``   ``REAL(4) X``  ``REAL(4)``  GNU extension
  ``DCOTAN(X)``  ``REAL(8) X``  ``REAL(8)``  GNU extension
  =============  =============  ===========  =============

:samp:`{See also}:`
  Converse function: 
  TAN 
  Degrees function: 
  COTAND

