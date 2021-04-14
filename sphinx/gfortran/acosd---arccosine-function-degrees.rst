  .. _acosd:

``ACOSD`` - Arccosine function, degrees
***************************************

.. index:: ACOSD

.. index:: DACOSD

.. index:: trigonometric function, cosine, inverse, degrees

.. index:: cosine, inverse, degrees

:samp:`{Description}:`
  ``ACOSD(X)`` computes the arccosine of :samp:`{X}` in degrees (inverse of
  ``COSD(X)``).

  This function is for compatibility only and should be avoided in favor of
  standard constructs wherever possible.

:samp:`{Standard}:`
  GNU extension, enabled with :option:`-fdec-math`

:samp:`{Class}:`
  Elemental function

:samp:`{Syntax}:`
  ``RESULT = ACOSD(X)``

:samp:`{Arguments}:`
  ===========  =============================================================
  :samp:`{X}`  The type shall either be ``REAL`` with a magnitude that is
               less than or equal to one - or the type shall be ``COMPLEX``.
  ===========  =============================================================
  ===========  =============================================================

:samp:`{Return value}:`
  The return value is of the same type and kind as :samp:`{X}`.
  The real part of the result is in degrees and lies in the range
  0 \leq \Re \acos(x) \leq 180.

:samp:`{Example}:`

  .. code-block:: c++

    program test_acosd
      real(8) :: x = 0.866_8
      x = acosd(x)
    end program test_acosd

:samp:`{Specific names}:`
  =============  =============  ===========  =============
  Name           Argument       Return type  Standard
  =============  =============  ===========  =============
  ``ACOSD(X)``   ``REAL(4) X``  ``REAL(4)``  GNU extension
  ``DACOSD(X)``  ``REAL(8) X``  ``REAL(8)``  GNU extension
  =============  =============  ===========  =============

:samp:`{See also}:`
  Inverse function: 
  COSD 
  Radians function: 
  ACOS 

