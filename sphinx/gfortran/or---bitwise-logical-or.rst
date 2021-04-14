  .. _or:

``OR`` - Bitwise logical OR
***************************

.. index:: OR

.. index:: bitwise logical or

.. index:: logical or, bitwise

:samp:`{Description}:`
  Bitwise logical ``OR``.

  This intrinsic routine is provided for backwards compatibility with 
  GNU Fortran 77.  For integer arguments, programmers should consider
  the use of the IOR intrinsic defined by the Fortran standard.

:samp:`{Standard}:`
  GNU extension

:samp:`{Class}:`
  Function

:samp:`{Syntax}:`
  ``RESULT = OR(I, J)``

:samp:`{Arguments}:`
  ===========  ===========================================================================
  :samp:`{I}`  The type shall be either a scalar ``INTEGER``
               type or a scalar ``LOGICAL`` type or a boz-literal-constant.
  ===========  ===========================================================================
  :samp:`{J}`  The type shall be the same as the type of :samp:`{I}` or
               a boz-literal-constant. :samp:`{I}` and :samp:`{J}` shall not both be
               boz-literal-constants.  If either :samp:`{I}` and :samp:`{J}` is a
               boz-literal-constant, then the other argument must be a scalar ``INTEGER``.
  ===========  ===========================================================================

:samp:`{Return value}:`
  The return type is either a scalar ``INTEGER`` or a scalar
  ``LOGICAL``.  If the kind type parameters differ, then the
  smaller kind type is implicitly converted to larger kind, and the 
  return has the larger kind.  A boz-literal-constant is 
  converted to an ``INTEGER`` with the kind type parameter of
  the other argument as-if a call to INT occurred.

:samp:`{Example}:`

  .. code-block:: c++

    PROGRAM test_or
      LOGICAL :: T = .TRUE., F = .FALSE.
      INTEGER :: a, b
      DATA a / Z'F' /, b / Z'3' /

      WRITE (*,*) OR(T, T), OR(T, F), OR(F, T), OR(F, F)
      WRITE (*,*) OR(a, b)
    END PROGRAM

:samp:`{See also}:`
  Fortran 95 elemental function: 
  IOR

