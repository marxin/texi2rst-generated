  .. _char:

CHAR --- Character conversion function
**************************************

.. index:: CHAR

.. index:: conversion, to character

:samp:`{Description}:`
  ``CHAR(I [, KIND])`` returns the character represented by the integer :samp:`{I}`.

:samp:`{Standard}:`
  Fortran 77 and later

:samp:`{Class}:`
  Elemental function

:samp:`{Syntax}:`
  ``RESULT = CHAR(I [, KIND])``

:samp:`{Arguments}:`
  ==============  =======================================================
  :samp:`{I}`     The type shall be ``INTEGER``.
  ==============  =======================================================
  :samp:`{KIND}`  (Optional) An ``INTEGER`` initialization
                  expression indicating the kind parameter of the result.
  ==============  =======================================================

:samp:`{Return value}:`
  The return value is of type ``CHARACTER(1)``

:samp:`{Example}:`

  .. code-block:: fortran

    program test_char
        integer :: i = 74
        character(1) :: c
        c = char(i)
        print *, i, c ! returns 'J'
    end program test_char

:samp:`{Specific names}:`
  ===========  =============  ====================  ====================
  Name         Argument       Return type           Standard
  ===========  =============  ====================  ====================
  ``CHAR(I)``  ``INTEGER I``  ``CHARACTER(LEN=1)``  Fortran 77 and later
  ===========  =============  ====================  ====================

:samp:`{Note}:`
  See ICHAR for a discussion of converting between numerical values
  and formatted string representations.

:samp:`{See also}:`
  ACHAR, 
  IACHAR, 
  ICHAR

