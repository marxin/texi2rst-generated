  .. _bessel_j0:

``BESSEL_J0`` - Bessel function of the first kind of order 0
************************************************************

.. index:: BESSEL_J0

.. index:: BESJ0

.. index:: DBESJ0

.. index:: Bessel function, first kind

:samp:`{Description}:`
  ``BESSEL_J0(X)`` computes the Bessel function of the first kind of
  order 0 of :samp:`{X}`. This function is available under the name
  ``BESJ0`` as a GNU extension.

:samp:`{Standard}:`
  Fortran 2008 and later

:samp:`{Class}:`
  Elemental function

:samp:`{Syntax}:`
  ``RESULT = BESSEL_J0(X)``

:samp:`{Arguments}:`
  ===========  ===========================
  :samp:`{X}`  The type shall be ``REAL``.
  ===========  ===========================
  ===========  ===========================

:samp:`{Return value}:`
  The return value is of type ``REAL`` and lies in the
  range - 0.4027... \leq Bessel (0,x) \leq 1. It has the same
  kind as :samp:`{X}`.

:samp:`{Example}:`

  .. code-block:: c++

    program test_besj0
      real(8) :: x = 0.0_8
      x = bessel_j0(x)
    end program test_besj0

:samp:`{Specific names}:`
  =============  =============  ===========  =============
  Name           Argument       Return type  Standard
  =============  =============  ===========  =============
  ``DBESJ0(X)``  ``REAL(8) X``  ``REAL(8)``  GNU extension
  =============  =============  ===========  =============
