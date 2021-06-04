  .. _cosd:

COSD --- Cosine function, degrees
*********************************

.. index:: COSD

.. index:: DCOSD

.. index:: CCOSD

.. index:: ZCOSD

.. index:: CDCOSD

.. index:: trigonometric function, cosine, degrees

.. index:: cosine, degrees

:samp:`{Description}:`
  ``COSD(X)`` computes the cosine of :samp:`{X}` in degrees.

  This function is for compatibility only and should be avoided in favor of
  standard constructs wherever possible.

:samp:`{Standard}:`
  GNU extension, enabled with :option:`-fdec-math`.

:samp:`{Class}:`
  Elemental function

:samp:`{Syntax}:`
  ``RESULT = COSD(X)``

:samp:`{Arguments}:`
  ===========  =============================
  :samp:`{X}`  The type shall be ``REAL`` or
               ``COMPLEX``.
  ===========  =============================

:samp:`{Return value}:`
  The return value is of the same type and kind as :samp:`{X}`. The real part
  of the result is in degrees.  If :samp:`{X}` is of the type ``REAL``,
  the return value lies in the range -1 \leq \cosd (x) \leq 1.

:samp:`{Example}:`

  .. code-block:: fortran

    program test_cosd
      real :: x = 0.0
      x = cosd(x)
    end program test_cosd

:samp:`{Specific names}:`
  =============  ================  ==============  =============
  Name           Argument          Return type     Standard
  ``COSD(X)``    ``REAL(4) X``     ``REAL(4)``     GNU extension
  ``DCOSD(X)``   ``REAL(8) X``     ``REAL(8)``     GNU extension
  ``CCOSD(X)``   ``COMPLEX(4) X``  ``COMPLEX(4)``  GNU extension
  ``ZCOSD(X)``   ``COMPLEX(8) X``  ``COMPLEX(8)``  GNU extension
  ``CDCOSD(X)``  ``COMPLEX(8) X``  ``COMPLEX(8)``  GNU extension
  =============  ================  ==============  =============

:samp:`{See also}:`
  Inverse function: 
  ACOSD 
  Radians function: 
  COS

