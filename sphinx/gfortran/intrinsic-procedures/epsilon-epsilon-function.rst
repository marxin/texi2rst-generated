  .. _epsilon:

EPSILON --- Epsilon function
****************************

.. index:: EPSILON

.. index:: model representation, epsilon

:samp:`{Description}:`
  ``EPSILON(X)`` returns the smallest number :samp:`{E}` of the same kind
  as :samp:`{X}` such that 1 + E > 1.

:samp:`{Standard}:`
  Fortran 90 and later

:samp:`{Class}:`
  Inquiry function

:samp:`{Syntax}:`
  ``RESULT = EPSILON(X)``

:samp:`{Arguments}:`
  ===========  ===========================
  :samp:`{X}`  The type shall be ``REAL``.
  ===========  ===========================

:samp:`{Return value}:`
  The return value is of same type as the argument.

:samp:`{Example}:`

  .. code-block:: fortran

    program test_epsilon
        real :: x = 3.143
        real(8) :: y = 2.33
        print *, EPSILON(x)
        print *, EPSILON(y)
    end program test_epsilon

