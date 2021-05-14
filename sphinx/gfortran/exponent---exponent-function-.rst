  .. _exponent:

EXPONENT - Exponent function 
*****************************

.. index:: EXPONENT

.. index:: real number, exponent

.. index:: floating point, exponent

:samp:`{Description}:`
  ``EXPONENT(X)`` returns the value of the exponent part of :samp:`{X}`. If :samp:`{X}`
  is zero the value returned is zero. 

:samp:`{Standard}:`
  Fortran 90 and later

:samp:`{Class}:`
  Elemental function

:samp:`{Syntax}:`
  ``RESULT = EXPONENT(X)``

:samp:`{Arguments}:`
  ===========  ===========================
  :samp:`{X}`  The type shall be ``REAL``.
  ===========  ===========================
  ===========  ===========================

:samp:`{Return value}:`
  The return value is of type default ``INTEGER``.

:samp:`{Example}:`

  .. code-block:: c++

    program test_exponent
      real :: x = 1.0
      integer :: i
      i = exponent(x)
      print *, i
      print *, exponent(0.0)
    end program test_exponent

