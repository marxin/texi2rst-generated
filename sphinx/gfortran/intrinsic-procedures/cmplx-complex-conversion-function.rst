  .. _cmplx:

CMPLX --- Complex conversion function
*************************************

.. index:: CMPLX

.. index:: complex numbers, conversion to

.. index:: conversion, to complex

:samp:`{Description}:`
  ``CMPLX(X [, Y [, KIND]])`` returns a complex number where :samp:`{X}` is converted to
  the real component.  If :samp:`{Y}` is present it is converted to the imaginary
  component.  If :samp:`{Y}` is not present then the imaginary component is set to
  0.0.  If :samp:`{X}` is complex then :samp:`{Y}` must not be present.

:samp:`{Standard}:`
  Fortran 77 and later

:samp:`{Class}:`
  Elemental function

:samp:`{Syntax}:`
  ``RESULT = CMPLX(X [, Y [, KIND]])``

:samp:`{Arguments}:`
  ==============  =======================================================
  :samp:`{X}`     The type may be ``INTEGER``, ``REAL``,
                  or ``COMPLEX``.
  ==============  =======================================================
  :samp:`{Y}`     (Optional; only allowed if :samp:`{X}` is not
                  ``COMPLEX``.)  May be ``INTEGER`` or ``REAL``.
  :samp:`{KIND}`  (Optional) An ``INTEGER`` initialization
                  expression indicating the kind parameter of the result.
  ==============  =======================================================

:samp:`{Return value}:`
  The return value is of ``COMPLEX`` type, with a kind equal to
  :samp:`{KIND}` if it is specified.  If :samp:`{KIND}` is not specified, the
  result is of the default ``COMPLEX`` kind, regardless of the kinds of
  :samp:`{X}` and :samp:`{Y}`. 

:samp:`{Example}:`

  .. code-block:: c++

    program test_cmplx
        integer :: i = 42
        real :: x = 3.14
        complex :: z
        z = cmplx(i, x)
        print *, z, cmplx(x)
    end program test_cmplx

:samp:`{See also}:`
  COMPLEX

