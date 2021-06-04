  .. _precision:

PRECISION --- Decimal precision of a real kind
**********************************************

.. index:: PRECISION

.. index:: model representation, precision

:samp:`{Description}:`
  ``PRECISION(X)`` returns the decimal precision in the model of the
  type of ``X``.

:samp:`{Standard}:`
  Fortran 90 and later

:samp:`{Class}:`
  Inquiry function

:samp:`{Syntax}:`
  ``RESULT = PRECISION(X)``

:samp:`{Arguments}:`
  ===========  ================================================
  :samp:`{X}`  Shall be of type ``REAL`` or ``COMPLEX``. It may
               be scalar or valued.
  ===========  ================================================
  ===========  ================================================

:samp:`{Return value}:`
  The return value is of type ``INTEGER`` and of the default integer
  kind.

:samp:`{Example}:`

  .. code-block:: fortran

    program prec_and_range
      real(kind=4) :: x(2)
      complex(kind=8) :: y

      print *, precision(x), range(x)
      print *, precision(y), range(y)
    end program prec_and_range

:samp:`{See also}:`
  SELECTED_REAL_KIND, 
  RANGE

