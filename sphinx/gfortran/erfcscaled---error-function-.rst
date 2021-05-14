  .. _erfc_scaled:

ERFC_SCALED - Error function 
*****************************

.. index:: ERFC_SCALED

.. index:: error function, complementary, exponentially-scaled

:samp:`{Description}:`
  ``ERFC_SCALED(X)`` computes the exponentially-scaled complementary
  error function of :samp:`{X}`.

:samp:`{Standard}:`
  Fortran 2008 and later

:samp:`{Class}:`
  Elemental function

:samp:`{Syntax}:`
  ``RESULT = ERFC_SCALED(X)``

:samp:`{Arguments}:`
  ===========  ===========================
  :samp:`{X}`  The type shall be ``REAL``.
  ===========  ===========================
  ===========  ===========================

:samp:`{Return value}:`
  The return value is of type ``REAL`` and of the same kind as :samp:`{X}`.

:samp:`{Example}:`

  .. code-block:: c++

    program test_erfc_scaled
      real(8) :: x = 0.17_8
      x = erfc_scaled(x)
    end program test_erfc_scaled

