  .. _dble:

DBLE --- Double conversion function
***********************************

.. index:: DBLE

.. index:: conversion, to real

:samp:`{Description}:`
  ``DBLE(A)`` Converts :samp:`{A}` to double precision real type.

:samp:`{Standard}:`
  Fortran 77 and later

:samp:`{Class}:`
  Elemental function

:samp:`{Syntax}:`
  ``RESULT = DBLE(A)``

:samp:`{Arguments}:`
  ===========  ========================================
  :samp:`{A}`  The type shall be ``INTEGER``, ``REAL``,
               or ``COMPLEX``.
  ===========  ========================================
  ===========  ========================================

:samp:`{Return value}:`
  The return value is of type double precision real.

:samp:`{Example}:`

  .. code-block:: fortran

    program test_dble
        real    :: x = 2.18
        integer :: i = 5
        complex :: z = (2.3,1.14)
        print *, dble(x), dble(i), dble(z)
    end program test_dble

:samp:`{See also}:`
  REAL

