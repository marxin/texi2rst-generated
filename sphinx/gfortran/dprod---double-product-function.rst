  .. _dprod:

DPROD - Double product function
*******************************

.. index:: DPROD

.. index:: product, double-precision

:samp:`{Description}:`
  ``DPROD(X,Y)`` returns the product ``X*Y``.

:samp:`{Standard}:`
  Fortran 77 and later

:samp:`{Class}:`
  Elemental function

:samp:`{Syntax}:`
  ``RESULT = DPROD(X, Y)``

:samp:`{Arguments}:`
  ===========  ===========================
  :samp:`{X}`  The type shall be ``REAL``.
  ===========  ===========================
  :samp:`{Y}`  The type shall be ``REAL``.
  ===========  ===========================

:samp:`{Return value}:`
  The return value is of type ``REAL(8)``.

:samp:`{Example}:`

  .. code-block:: c++

    program test_dprod
        real :: x = 5.2
        real :: y = 2.3
        real(8) :: d
        d = dprod(x,y)
        print *, d
    end program test_dprod

:samp:`{Specific names}:`
  ==============  ================  ===========  ====================
  Name            Argument          Return type  Standard
  ==============  ================  ===========  ====================
  ``DPROD(X,Y)``  ``REAL(4) X, Y``  ``REAL(8)``  Fortran 77 and later
  ==============  ================  ===========  ====================
