  .. _cotand:

``COTAND`` - Cotangent function, degrees
****************************************

.. index:: COTAND

.. index:: DCOTAND

.. index:: trigonometric function, cotangent, degrees

.. index:: cotangent, degrees

:samp:`{Description}:`
  ``COTAND(X)`` computes the cotangent of :samp:`{X}` in degrees.  Equivalent to
  ``COSD(x)`` divided by ``SIND(x)``, or ``1 / TAND(x)``.

:samp:`{Standard}:`
  GNU extension, enabled with :option:`-fdec-math`.

  This function is for compatibility only and should be avoided in favor of
  standard constructs wherever possible.

:samp:`{Class}:`
  Elemental function

:samp:`{Syntax}:`
  ``RESULT = COTAND(X)``

:samp:`{Arguments}:`
  ===========  ==========================================
  :samp:`{X}`  The type shall be ``REAL`` or ``COMPLEX``.
  ===========  ==========================================
  ===========  ==========================================

:samp:`{Return value}:`
  The return value has same type and kind as :samp:`{X}` , and its value is in degrees.

:samp:`{Example}:`

  .. code-block:: c++

    program test_cotand
      real(8) :: x = 0.165_8
      x = cotand(x)
    end program test_cotand

:samp:`{Specific names}:`
  ==============  =============  ===========  =============
  Name            Argument       Return type  Standard
  ==============  =============  ===========  =============
  ``COTAND(X)``   ``REAL(4) X``  ``REAL(4)``  GNU extension
  ``DCOTAND(X)``  ``REAL(8) X``  ``REAL(8)``  GNU extension
  ==============  =============  ===========  =============

:samp:`{See also}:`
  Converse function: 
  TAND 
  Radians function: 
  COTAN

