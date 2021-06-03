  .. _iand:

IAND --- Bitwise logical and
****************************

.. index:: IAND

.. index:: BIAND

.. index:: IIAND

.. index:: JIAND

.. index:: KIAND

.. index:: bitwise logical and

.. index:: logical and, bitwise

:samp:`{Description}:`
  Bitwise logical ``AND``.

:samp:`{Standard}:`
  Fortran 90 and later, with boz-literal-constant Fortran 2008 and later, has overloads that are GNU extensions

:samp:`{Class}:`
  Elemental function

:samp:`{Syntax}:`
  ``RESULT = IAND(I, J)``

:samp:`{Arguments}:`
  ===========  ====================================================================
  :samp:`{I}`  The type shall be ``INTEGER`` or a boz-literal-constant.
  ===========  ====================================================================
  :samp:`{J}`  The type shall be ``INTEGER`` with the same
               kind type parameter as :samp:`{I}` or a boz-literal-constant.
               :samp:`{I}` and :samp:`{J}` shall not both be boz-literal-constants.
  ===========  ====================================================================

:samp:`{Return value}:`
  The return type is ``INTEGER`` with the kind type parameter of the
  arguments.
  A boz-literal-constant is converted to an ``INTEGER`` with the kind
  type parameter of the other argument as-if a call to INT occurred.

:samp:`{Example}:`

  .. code-block:: c++

    PROGRAM test_iand
      INTEGER :: a, b
      DATA a / Z'F' /, b / Z'3' /
      WRITE (*,*) IAND(a, b)
    END PROGRAM

:samp:`{Specific names}:`
  ============  ================  ==============  ====================
  Name          Argument          Return type     Standard
  ============  ================  ==============  ====================
  ``IAND(A)``   ``INTEGER A``     ``INTEGER``     Fortran 90 and later
  ``BIAND(A)``  ``INTEGER(1) A``  ``INTEGER(1)``  GNU extension
  ``IIAND(A)``  ``INTEGER(2) A``  ``INTEGER(2)``  GNU extension
  ``JIAND(A)``  ``INTEGER(4) A``  ``INTEGER(4)``  GNU extension
  ``KIAND(A)``  ``INTEGER(8) A``  ``INTEGER(8)``  GNU extension
  ============  ================  ==============  ====================

:samp:`{See also}:`
  IOR, 
  IEOR, 
  IBITS, 
  IBSET, 
  IBCLR, 
  NOT

