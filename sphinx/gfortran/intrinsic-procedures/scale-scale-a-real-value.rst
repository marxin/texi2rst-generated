  .. _scale:

SCALE --- Scale a real value
****************************

.. index:: SCALE

.. index:: real number, scale

.. index:: floating point, scale

:samp:`{Description}:`
  ``SCALE(X,I)`` returns ``X * RADIX(X)**I``.

:samp:`{Standard}:`
  Fortran 90 and later

:samp:`{Class}:`
  Elemental function

:samp:`{Syntax}:`
  ``RESULT = SCALE(X, I)``

:samp:`{Arguments}:`
  ===========  ================================================
  :samp:`{X}`  The type of the argument shall be a ``REAL``.
  ===========  ================================================
  :samp:`{I}`  The type of the argument shall be a ``INTEGER``.
  ===========  ================================================

:samp:`{Return value}:`
  The return value is of the same type and kind as :samp:`{X}`.
  Its value is ``X * RADIX(X)**I``.

:samp:`{Example}:`

  .. code-block:: fortran

    program test_scale
      real :: x = 178.1387e-4
      integer :: i = 5
      print *, scale(x,i), x*radix(x)**i
    end program test_scale

