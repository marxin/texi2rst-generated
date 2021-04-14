  .. _lgt:

``LGT`` - Lexical greater than
******************************

.. index:: LGT

.. index:: lexical comparison of strings

.. index:: string, comparison

:samp:`{Description}:`
  Determines whether one string is lexically greater than another string,
  where the two strings are interpreted as containing ASCII character
  codes.  If the String A and String B are not the same length, the
  shorter is compared as if spaces were appended to it to form a value
  that has the same length as the longer.

  In general, the lexical comparison intrinsics ``LGE``, ``LGT``,
  ``LLE``, and ``LLT`` differ from the corresponding intrinsic
  operators ``.GE.``, ``.GT.``, ``.LE.``, and ``.LT.``, in
  that the latter use the processor's character ordering (which is not
  ASCII on some targets), whereas the former always use the ASCII
  ordering.

:samp:`{Standard}:`
  Fortran 77 and later

:samp:`{Class}:`
  Elemental function

:samp:`{Syntax}:`
  ``RESULT = LGT(STRING_A, STRING_B)``

:samp:`{Arguments}:`
  ==================  =======================================
  :samp:`{STRING_A}`  Shall be of default ``CHARACTER`` type.
  ==================  =======================================
  :samp:`{STRING_B}`  Shall be of default ``CHARACTER`` type.
  ==================  =======================================

:samp:`{Return value}:`
  Returns ``.TRUE.`` if ``STRING_A > STRING_B``, and ``.FALSE.``
  otherwise, based on the ASCII ordering.

:samp:`{Specific names}:`
  ===========================  =============  ===========  ====================
  Name                         Argument       Return type  Standard
  ===========================  =============  ===========  ====================
  ``LGT(STRING_A, STRING_B)``  ``CHARACTER``  ``LOGICAL``  Fortran 77 and later
  ===========================  =============  ===========  ====================

:samp:`{See also}:`
  LGE, 
  LLE, 
  LLT

