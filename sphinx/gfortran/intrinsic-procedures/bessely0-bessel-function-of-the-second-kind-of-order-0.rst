  .. _bessel_y0:

BESSEL_Y0 - Bessel function of the second kind of order 0
*********************************************************

.. index:: BESSEL_Y0

.. index:: BESY0

.. index:: DBESY0

.. index:: Bessel function, second kind

:samp:`{Description}:`
  ``BESSEL_Y0(X)`` computes the Bessel function of the second kind of
  order 0 of :samp:`{X}`. This function is available under the name
  ``BESY0`` as a GNU extension.

:samp:`{Standard}:`
  Fortran 2008 and later

:samp:`{Class}:`
  Elemental function

:samp:`{Syntax}:`
  ``RESULT = BESSEL_Y0(X)``

:samp:`{Arguments}:`
  ===========  ===========================
  :samp:`{X}`  The type shall be ``REAL``.
  ===========  ===========================
  ===========  ===========================

:samp:`{Return value}:`
  The return value is of type ``REAL``. It has the same kind as :samp:`{X}`.

:samp:`{Example}:`

  .. code-block:: c++

    program test_besy0
      real(8) :: x = 0.0_8
      x = bessel_y0(x)
    end program test_besy0

:samp:`{Specific names}:`
  =============  =============  ===========  =============
  Name           Argument       Return type  Standard
  =============  =============  ===========  =============
  ``DBESY0(X)``  ``REAL(8) X``  ``REAL(8)``  GNU extension
  =============  =============  ===========  =============
