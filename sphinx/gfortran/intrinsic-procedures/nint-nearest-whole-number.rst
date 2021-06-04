  .. _nint:

NINT --- Nearest whole number
*****************************

.. index:: NINT

.. index:: IDNINT

.. index:: rounding, nearest whole number

:samp:`{Description}:`
  ``NINT(A)`` rounds its argument to the nearest whole number.

:samp:`{Standard}:`
  Fortran 77 and later, with :samp:`{KIND}` argument Fortran 90 and later

:samp:`{Class}:`
  Elemental function

:samp:`{Syntax}:`
  ``RESULT = NINT(A [, KIND])``

:samp:`{Arguments}:`
  ==============  =======================================================
  :samp:`{A}`     The type of the argument shall be ``REAL``.
  ==============  =======================================================
  :samp:`{KIND}`  (Optional) An ``INTEGER`` initialization
                  expression indicating the kind parameter of the result.
  ==============  =======================================================

:samp:`{Return value}:`
  Returns :samp:`{A}` with the fractional portion of its magnitude eliminated by
  rounding to the nearest whole number and with its sign preserved,
  converted to an ``INTEGER`` of the default kind.

:samp:`{Example}:`

  .. code-block:: fortran

    program test_nint
      real(4) x4
      real(8) x8
      x4 = 1.234E0_4
      x8 = 4.321_8
      print *, nint(x4), idnint(x8)
    end program test_nint

:samp:`{Specific names}:`
  =============  =============  ===========  ====================
  Name           Argument       Return Type  Standard
  =============  =============  ===========  ====================
  ``NINT(A)``    ``REAL(4) A``  ``INTEGER``  Fortran 77 and later
  ``IDNINT(A)``  ``REAL(8) A``  ``INTEGER``  Fortran 77 and later
  =============  =============  ===========  ====================

:samp:`{See also}:`
  CEILING, 
  FLOOR

