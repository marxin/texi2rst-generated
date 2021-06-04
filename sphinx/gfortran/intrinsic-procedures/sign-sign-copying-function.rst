  .. _sign:

SIGN --- Sign copying function
******************************

.. index:: SIGN

.. index:: ISIGN

.. index:: DSIGN

.. index:: sign copying

:samp:`{Description}:`
  ``SIGN(A,B)`` returns the value of :samp:`{A}` with the sign of :samp:`{B}`.

:samp:`{Standard}:`
  Fortran 77 and later

:samp:`{Class}:`
  Elemental function

:samp:`{Syntax}:`
  ``RESULT = SIGN(A, B)``

:samp:`{Arguments}:`
  ===========  ==================================================
  :samp:`{A}`  Shall be of type ``INTEGER`` or ``REAL``
  ===========  ==================================================
  :samp:`{B}`  Shall be of the same type and kind as :samp:`{A}`.
  ===========  ==================================================

:samp:`{Return value}:`
  The kind of the return value is that of :samp:`{A}` and :samp:`{B}`.
  If B\ge 0 then the result is ``ABS(A)``, else
  it is ``-ABS(A)``.

:samp:`{Example}:`

  .. code-block:: fortran

    program test_sign
      print *, sign(-12,1)
      print *, sign(-12,0)
      print *, sign(-12,-1)

      print *, sign(-12.,1.)
      print *, sign(-12.,0.)
      print *, sign(-12.,-1.)
    end program test_sign

:samp:`{Specific names}:`
  ==============  ===================  ==============  ====================
  Name            Arguments            Return type     Standard
  ==============  ===================  ==============  ====================
  ``SIGN(A,B)``   ``REAL(4) A, B``     ``REAL(4)``     Fortran 77 and later
  ``ISIGN(A,B)``  ``INTEGER(4) A, B``  ``INTEGER(4)``  Fortran 77 and later
  ``DSIGN(A,B)``  ``REAL(8) A, B``     ``REAL(8)``     Fortran 77 and later
  ==============  ===================  ==============  ====================
