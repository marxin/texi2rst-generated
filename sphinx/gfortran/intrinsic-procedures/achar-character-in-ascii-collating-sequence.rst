  .. _achar:

ACHAR --- Character in ASCII collating sequence 
************************************************

.. index:: ACHAR

.. index:: ASCII collating sequence

.. index:: collating sequence, ASCII

:samp:`{Description}:`
  ``ACHAR(I)`` returns the character located at position ``I``
  in the ASCII collating sequence.

:samp:`{Standard}:`
  Fortran 77 and later, with :samp:`{KIND}` argument Fortran 2003 and later

:samp:`{Class}:`
  Elemental function

:samp:`{Syntax}:`
  ``RESULT = ACHAR(I [, KIND])``

:samp:`{Arguments}:`
  ==============  =======================================================
  :samp:`{I}`     The type shall be ``INTEGER``.
  :samp:`{KIND}`  (Optional) An ``INTEGER`` initialization
                  expression indicating the kind parameter of the result.
  ==============  =======================================================

:samp:`{Return value}:`
  The return value is of type ``CHARACTER`` with a length of one.
  If the :samp:`{KIND}` argument is present, the return value is of the
  specified kind and of the default kind otherwise.

:samp:`{Example}:`

  .. code-block:: fortran

    program test_achar
      character c
      c = achar(32)
    end program test_achar

:samp:`{Note}:`
  See ICHAR for a discussion of converting between numerical values
  and formatted string representations.

:samp:`{See also}:`
  CHAR, 
  IACHAR, 
  ICHAR

