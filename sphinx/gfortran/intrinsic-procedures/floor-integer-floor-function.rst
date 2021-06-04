  .. _floor:

FLOOR --- Integer floor function
********************************

.. index:: FLOOR

.. index:: floor

.. index:: rounding, floor

:samp:`{Description}:`
  ``FLOOR(A)`` returns the greatest integer less than or equal to :samp:`{X}`.

:samp:`{Standard}:`
  Fortran 95 and later

:samp:`{Class}:`
  Elemental function

:samp:`{Syntax}:`
  ``RESULT = FLOOR(A [, KIND])``

:samp:`{Arguments}:`
  ==============  =======================================================
  :samp:`{A}`     The type shall be ``REAL``.
  :samp:`{KIND}`  (Optional) An ``INTEGER`` initialization
                  expression indicating the kind parameter of the result.
  ==============  =======================================================

:samp:`{Return value}:`
  The return value is of type ``INTEGER(KIND)`` if :samp:`{KIND}` is present
  and of default-kind ``INTEGER`` otherwise.

:samp:`{Example}:`

  .. code-block:: fortran

    program test_floor
        real :: x = 63.29
        real :: y = -63.59
        print *, floor(x) ! returns 63
        print *, floor(y) ! returns -64
    end program test_floor

:samp:`{See also}:`
  CEILING, 
  NINT

