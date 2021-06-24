..
  Copyright 1988-2021 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the GPL license file

.. _llt:

LLT --- Lexical less than
*************************

.. index:: LLT

.. index:: lexical comparison of strings

.. index:: string, comparison

.. function:: LLT

  Determines whether one string is lexically less than another string,
  where the two strings are interpreted as containing ASCII character
  codes.  If the String A and String B are not the same length, the
  shorter is compared as if spaces were appended to it to form a value
  that has the same length as the longer.

  :param STRING_A:
    Shall be of default ``CHARACTER`` type.

  :param STRING_B:
    Shall be of default ``CHARACTER`` type.

  :return:
    Returns ``.TRUE.`` if ``STRING_A < STRING_B``, and ``.FALSE.``
    otherwise, based on the ASCII ordering.

  :samp:`{Standard}:`
    Fortran 77 and later

  :samp:`{Class}:`
    Elemental function

  :samp:`{Syntax}:`

  .. code-block:: fortran

    RESULT = LLT(STRING_A, STRING_B)

  :samp:`{Specific names}:`
    ===========================  =============  ===========  ====================
    Name                         Argument       Return type  Standard
    ===========================  =============  ===========  ====================
    ``LLT(STRING_A, STRING_B)``  ``CHARACTER``  ``LOGICAL``  Fortran 77 and later
    ===========================  =============  ===========  ====================

  :samp:`{See also}:`
    LGE, 
    LGT, 
    LLE
