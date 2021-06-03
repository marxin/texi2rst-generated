  .. _aint:

AINT --- Truncate to a whole number
***********************************

.. index:: AINT

.. index:: DINT

.. index:: floor

.. index:: rounding, floor

:samp:`{Description}:`
  ``AINT(A [, KIND])`` truncates its argument to a whole number.

:samp:`{Standard}:`
  Fortran 77 and later

:samp:`{Class}:`
  Elemental function

:samp:`{Syntax}:`
  ``RESULT = AINT(A [, KIND])`` 

:samp:`{Arguments}:`
  ==============  =======================================================
  :samp:`{A}`     The type of the argument shall be ``REAL``.
  ==============  =======================================================
  :samp:`{KIND}`  (Optional) An ``INTEGER`` initialization
                  expression indicating the kind parameter of the result.
  ==============  =======================================================

:samp:`{Return value}:`
  The return value is of type ``REAL`` with the kind type parameter of the
  argument if the optional :samp:`{KIND}` is absent; otherwise, the kind
  type parameter will be given by :samp:`{KIND}`.  If the magnitude of 
  :samp:`{X}` is less than one, ``AINT(X)`` returns zero.  If the
  magnitude is equal to or greater than one then it returns the largest
  whole number that does not exceed its magnitude.  The sign is the same
  as the sign of :samp:`{X}`. 

:samp:`{Example}:`

  .. code-block:: c++

    program test_aint
      real(4) x4
      real(8) x8
      x4 = 1.234E0_4
      x8 = 4.321_8
      print *, aint(x4), dint(x8)
      x8 = aint(x4,8)
    end program test_aint

:samp:`{Specific names}:`
  ===========  =============  ===========  ====================
  Name         Argument       Return type  Standard
  ===========  =============  ===========  ====================
  ``AINT(A)``  ``REAL(4) A``  ``REAL(4)``  Fortran 77 and later
  ``DINT(A)``  ``REAL(8) A``  ``REAL(8)``  Fortran 77 and later
  ===========  =============  ===========  ====================
