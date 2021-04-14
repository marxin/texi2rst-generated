  .. _complex:

``COMPLEX`` - Complex conversion function
*****************************************

.. index:: COMPLEX

.. index:: complex numbers, conversion to

.. index:: conversion, to complex

:samp:`{Description}:`
  ``COMPLEX(X, Y)`` returns a complex number where :samp:`{X}` is converted
  to the real component and :samp:`{Y}` is converted to the imaginary
  component.

:samp:`{Standard}:`
  GNU extension

:samp:`{Class}:`
  Elemental function

:samp:`{Syntax}:`
  ``RESULT = COMPLEX(X, Y)``

:samp:`{Arguments}:`
  ===========  ========================================
  :samp:`{X}`  The type may be ``INTEGER`` or ``REAL``.
  ===========  ========================================
  :samp:`{Y}`  The type may be ``INTEGER`` or ``REAL``.
  ===========  ========================================

:samp:`{Return value}:`
  If :samp:`{X}` and :samp:`{Y}` are both of ``INTEGER`` type, then the return
  value is of default ``COMPLEX`` type.

  If :samp:`{X}` and :samp:`{Y}` are of ``REAL`` type, or one is of ``REAL``
  type and one is of ``INTEGER`` type, then the return value is of
  ``COMPLEX`` type with a kind equal to that of the ``REAL``
  argument with the highest precision.

:samp:`{Example}:`

  .. code-block:: c++

    program test_complex
        integer :: i = 42
        real :: x = 3.14
        print *, complex(i, x)
    end program test_complex

:samp:`{See also}:`
  CMPLX

