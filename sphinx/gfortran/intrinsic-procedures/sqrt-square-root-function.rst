  .. _sqrt:

SQRT --- Square-root function
*****************************

.. index:: SQRT

.. index:: DSQRT

.. index:: CSQRT

.. index:: ZSQRT

.. index:: CDSQRT

.. index:: root

.. index:: square-root

:samp:`{Description}:`
  ``SQRT(X)`` computes the square root of :samp:`{X}`.

:samp:`{Standard}:`
  Fortran 77 and later

:samp:`{Class}:`
  Elemental function

:samp:`{Syntax}:`
  ``RESULT = SQRT(X)``

:samp:`{Arguments}:`
  ===========  =============================
  :samp:`{X}`  The type shall be ``REAL`` or
               ``COMPLEX``.
  ===========  =============================

:samp:`{Return value}:`
  The return value is of type ``REAL`` or ``COMPLEX``.
  The kind type parameter is the same as :samp:`{X}`.

:samp:`{Example}:`

  .. code-block:: fortran

    program test_sqrt
      real(8) :: x = 2.0_8
      complex :: z = (1.0, 2.0)
      x = sqrt(x)
      z = sqrt(z)
    end program test_sqrt

:samp:`{Specific names}:`
  =============  ================  ==============  ====================
  Name           Argument          Return type     Standard
  ``SQRT(X)``    ``REAL(4) X``     ``REAL(4)``     Fortran 77 and later
  ``DSQRT(X)``   ``REAL(8) X``     ``REAL(8)``     Fortran 77 and later
  ``CSQRT(X)``   ``COMPLEX(4) X``  ``COMPLEX(4)``  Fortran 77 and later
  ``ZSQRT(X)``   ``COMPLEX(8) X``  ``COMPLEX(8)``  GNU extension
  ``CDSQRT(X)``  ``COMPLEX(8) X``  ``COMPLEX(8)``  GNU extension
  =============  ================  ==============  ====================
