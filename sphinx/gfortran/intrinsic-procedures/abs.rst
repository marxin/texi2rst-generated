..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the GPL license file

.. index:: ABS, CABS, DABS, IABS, ZABS, CDABS, BABS, IIABS, JIABS, KIABS, absolute value

.. _abs:

ABS --- Absolute value
**********************

.. function:: ABS(A)

  ``ABS(A)`` computes the absolute value of ``A``.

  :param A:
    The type of the argument shall be an ``INTEGER``,
    ``REAL``, or ``COMPLEX``.

  :return:
    The return value is of the same type and
    kind as the argument except the return value is ``REAL`` for a
    ``COMPLEX`` argument.

  :samp:`{Standard}:`
    Fortran 77 and later, has overloads that are GNU extensions

  :samp:`{Class}:`
    Elemental function

  :samp:`{Syntax}:`

    .. code-block:: fortran

      RESULT = ABS(A)

  :samp:`{Example}:`

    .. code-block:: fortran

      program test_abs
        integer :: i = -1
        real :: x = -1.e0
        complex :: z = (-1.e0,0.e0)
        i = abs(i)
        x = abs(x)
        x = abs(z)
      end program test_abs

  :samp:`{Specific names}:`
    ============  ================  ==============  ====================
    Name          Argument          Return type     Standard
    ============  ================  ==============  ====================
    ``ABS(A)``    ``REAL(4) A``     ``REAL(4)``     Fortran 77 and later
    ``CABS(A)``   ``COMPLEX(4) A``  ``REAL(4)``     Fortran 77 and later
    ``DABS(A)``   ``REAL(8) A``     ``REAL(8)``     Fortran 77 and later
    ``IABS(A)``   ``INTEGER(4) A``  ``INTEGER(4)``  Fortran 77 and later
    ``BABS(A)``   ``INTEGER(1) A``  ``INTEGER(1)``  GNU extension
    ``IIABS(A)``  ``INTEGER(2) A``  ``INTEGER(2)``  GNU extension
    ``JIABS(A)``  ``INTEGER(4) A``  ``INTEGER(4)``  GNU extension
    ``KIABS(A)``  ``INTEGER(8) A``  ``INTEGER(8)``  GNU extension
    ``ZABS(A)``   ``COMPLEX(8) A``  ``REAL(8)``     GNU extension
    ``CDABS(A)``  ``COMPLEX(8) A``  ``REAL(8)``     GNU extension
    ============  ================  ==============  ====================
