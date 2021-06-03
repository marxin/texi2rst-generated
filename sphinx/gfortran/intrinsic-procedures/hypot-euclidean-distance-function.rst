  .. _hypot:

HYPOT --- Euclidean distance function
*************************************

.. index:: HYPOT

.. index:: Euclidean distance

:samp:`{Description}:`
  ``HYPOT(X,Y)`` is the Euclidean distance function. It is equal to
  \sqrt{X^2 + Y^2}, without undue underflow or overflow.

:samp:`{Standard}:`
  Fortran 2008 and later

:samp:`{Class}:`
  Elemental function

:samp:`{Syntax}:`
  ``RESULT = HYPOT(X, Y)``

:samp:`{Arguments}:`
  ===========  =====================================================
  :samp:`{X}`  The type shall be ``REAL``.
  ===========  =====================================================
  :samp:`{Y}`  The type and kind type parameter shall be the same as
               :samp:`{X}`.
  ===========  =====================================================

:samp:`{Return value}:`
  The return value has the same type and kind type parameter as :samp:`{X}`.

:samp:`{Example}:`

  .. code-block:: c++

    program test_hypot
      real(4) :: x = 1.e0_4, y = 0.5e0_4
      x = hypot(x,y)
    end program test_hypot

