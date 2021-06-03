  .. _asind:

ASIND --- Arcsine function, degrees
***********************************

.. index:: ASIND

.. index:: DASIND

.. index:: trigonometric function, sine, inverse, degrees

.. index:: sine, inverse, degrees

:samp:`{Description}:`
  ``ASIND(X)`` computes the arcsine of its :samp:`{X}` in degrees (inverse of
  ``SIND(X)`` ).

  This function is for compatibility only and should be avoided in favor of
  standard constructs wherever possible.

:samp:`{Standard}:`
  GNU extension, enabled with :option:`-fdec-math`.

:samp:`{Class}:`
  Elemental function

:samp:`{Syntax}:`
  ``RESULT = ASIND(X)``

:samp:`{Arguments}:`
  ===========  =========================================================
  :samp:`{X}`  The type shall be either ``REAL`` and a magnitude that is
               less than or equal to one - or be ``COMPLEX``.
  ===========  =========================================================
  ===========  =========================================================

:samp:`{Return value}:`
  The return value is of the same type and kind as :samp:`{X}`.
  The real part of the result is in degrees and lies in the range
  -90 \leq \Re \asin(x) \leq 90.

:samp:`{Example}:`

  .. code-block:: c++

    program test_asind
      real(8) :: x = 0.866_8
      x = asind(x)
    end program test_asind

:samp:`{Specific names}:`
  =============  =============  ===========  =============
  Name           Argument       Return type  Standard
  =============  =============  ===========  =============
  ``ASIND(X)``   ``REAL(4) X``  ``REAL(4)``  GNU extension
  ``DASIND(X)``  ``REAL(8) X``  ``REAL(8)``  GNU extension
  =============  =============  ===========  =============

:samp:`{See also}:`
  Inverse function: 
  SIND 
  Radians function: 
  ASIN

