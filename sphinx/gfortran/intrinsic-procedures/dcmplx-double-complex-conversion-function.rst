  .. _dcmplx:

DCMPLX --- Double complex conversion function
*********************************************

.. index:: DCMPLX

.. index:: complex numbers, conversion to

.. index:: conversion, to complex

:samp:`{Description}:`
  ``DCMPLX(X [,Y])`` returns a double complex number where :samp:`{X}` is
  converted to the real component.  If :samp:`{Y}` is present it is converted to the
  imaginary component.  If :samp:`{Y}` is not present then the imaginary component is
  set to 0.0.  If :samp:`{X}` is complex then :samp:`{Y}` must not be present.

:samp:`{Standard}:`
  GNU extension

:samp:`{Class}:`
  Elemental function

:samp:`{Syntax}:`
  ``RESULT = DCMPLX(X [, Y])``

:samp:`{Arguments}:`
  ===========  ====================================================
  :samp:`{X}`  The type may be ``INTEGER``, ``REAL``,
               or ``COMPLEX``.
  ===========  ====================================================
  :samp:`{Y}`  (Optional if :samp:`{X}` is not ``COMPLEX``.) May be
               ``INTEGER`` or ``REAL``.
  ===========  ====================================================

:samp:`{Return value}:`
  The return value is of type ``COMPLEX(8)``

:samp:`{Example}:`

  .. code-block:: c++

    program test_dcmplx
        integer :: i = 42
        real :: x = 3.14
        complex :: z
        z = cmplx(i, x)
        print *, dcmplx(i)
        print *, dcmplx(x)
        print *, dcmplx(z)
        print *, dcmplx(x,i)
    end program test_dcmplx

