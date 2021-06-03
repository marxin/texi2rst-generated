  .. _iachar:

IACHAR --- Code in ASCII collating sequence 
********************************************

.. index:: IACHAR

.. index:: ASCII collating sequence

.. index:: collating sequence, ASCII

.. index:: conversion, to integer

:samp:`{Description}:`
  ``IACHAR(C)`` returns the code for the ASCII character
  in the first character position of ``C``.

:samp:`{Standard}:`
  Fortran 95 and later, with :samp:`{KIND}` argument Fortran 2003 and later

:samp:`{Class}:`
  Elemental function

:samp:`{Syntax}:`
  ``RESULT = IACHAR(C [, KIND])``

:samp:`{Arguments}:`
  ==============  =======================================================
  :samp:`{C}`     Shall be a scalar ``CHARACTER``, with ``INTENT(IN)``
  ==============  =======================================================
  :samp:`{KIND}`  (Optional) An ``INTEGER`` initialization
                  expression indicating the kind parameter of the result.
  ==============  =======================================================

:samp:`{Return value}:`
  The return value is of type ``INTEGER`` and of kind :samp:`{KIND}`. If
  :samp:`{KIND}` is absent, the return value is of default integer kind.

:samp:`{Example}:`

  .. code-block:: c++

    program test_iachar
      integer i
      i = iachar(' ')
    end program test_iachar

:samp:`{Note}:`
  See ICHAR for a discussion of converting between numerical values
  and formatted string representations.

:samp:`{See also}:`
  ACHAR, 
  CHAR, 
  ICHAR

