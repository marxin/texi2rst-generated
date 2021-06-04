  .. _bessel_y1:

BESSEL_Y1 --- Bessel function of the second kind of order 1
***********************************************************

.. index:: BESSEL_Y1

.. index:: BESY1

.. index:: DBESY1

.. index:: Bessel function, second kind

:samp:`{Description}:`
  ``BESSEL_Y1(X)`` computes the Bessel function of the second kind of
  order 1 of :samp:`{X}`. This function is available under the name
  ``BESY1`` as a GNU extension.

:samp:`{Standard}:`
  Fortran 2008 and later

:samp:`{Class}:`
  Elemental function

:samp:`{Syntax}:`
  ``RESULT = BESSEL_Y1(X)``

:samp:`{Arguments}:`
  ===========  ===========================
  :samp:`{X}`  The type shall be ``REAL``.
  ===========  ===========================
  ===========  ===========================

:samp:`{Return value}:`
  The return value is of type ``REAL``. It has the same kind as :samp:`{X}`.

:samp:`{Example}:`

  .. code-block:: fortran

    program test_besy1
      real(8) :: x = 1.0_8
      x = bessel_y1(x)
    end program test_besy1

:samp:`{Specific names}:`
  =============  =============  ===========  =============
  Name           Argument       Return type  Standard
  =============  =============  ===========  =============
  ``DBESY1(X)``  ``REAL(8) X``  ``REAL(8)``  GNU extension
  =============  =============  ===========  =============
