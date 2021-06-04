  .. _ceiling:

CEILING --- Integer ceiling function
************************************

.. index:: CEILING

.. index:: ceiling

.. index:: rounding, ceiling

:samp:`{Description}:`
  ``CEILING(A)`` returns the least integer greater than or equal to :samp:`{A}`.

:samp:`{Standard}:`
  Fortran 95 and later

:samp:`{Class}:`
  Elemental function

:samp:`{Syntax}:`
  ``RESULT = CEILING(A [, KIND])``

:samp:`{Arguments}:`
  ==============  =======================================================
  :samp:`{A}`     The type shall be ``REAL``.
  ==============  =======================================================
  :samp:`{KIND}`  (Optional) An ``INTEGER`` initialization
                  expression indicating the kind parameter of the result.
  ==============  =======================================================

:samp:`{Return value}:`
  The return value is of type ``INTEGER(KIND)`` if :samp:`{KIND}` is present
  and a default-kind ``INTEGER`` otherwise.

:samp:`{Example}:`

  .. code-block:: fortran

    program test_ceiling
        real :: x = 63.29
        real :: y = -63.59
        print *, ceiling(x) ! returns 64
        print *, ceiling(y) ! returns -63
    end program test_ceiling

:samp:`{See also}:`
  FLOOR, 
  NINT

