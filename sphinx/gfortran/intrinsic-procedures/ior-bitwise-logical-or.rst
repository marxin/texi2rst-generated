  .. _ior:

IOR --- Bitwise logical or
**************************

.. index:: IOR

.. index:: BIOR

.. index:: IIOR

.. index:: JIOR

.. index:: KIOR

.. index:: bitwise logical or

.. index:: logical or, bitwise

:samp:`{Description}:`
  ``IOR`` returns the bitwise Boolean inclusive-OR of :samp:`{I}` and
  :samp:`{J}`.

:samp:`{Standard}:`
  Fortran 90 and later, with boz-literal-constant Fortran 2008 and later, has overloads that are GNU extensions

:samp:`{Class}:`
  Elemental function

:samp:`{Syntax}:`
  ``RESULT = IOR(I, J)``

:samp:`{Arguments}:`
  ===========  ====================================================================
  :samp:`{I}`  The type shall be ``INTEGER`` or a boz-literal-constant.
  :samp:`{J}`  The type shall be ``INTEGER`` with the same
               kind type parameter as :samp:`{I}` or a boz-literal-constant.
               :samp:`{I}` and :samp:`{J}` shall not both be boz-literal-constants.
  ===========  ====================================================================

:samp:`{Return value}:`
  The return type is ``INTEGER`` with the kind type parameter of the
  arguments.
  A boz-literal-constant is converted to an ``INTEGER`` with the kind
  type parameter of the other argument as-if a call to INT occurred.

:samp:`{Specific names}:`
  ===========  ================  ==============  ====================
  Name         Argument          Return type     Standard
  ``IOR(A)``   ``INTEGER A``     ``INTEGER``     Fortran 90 and later
  ``BIOR(A)``  ``INTEGER(1) A``  ``INTEGER(1)``  GNU extension
  ``IIOR(A)``  ``INTEGER(2) A``  ``INTEGER(2)``  GNU extension
  ``JIOR(A)``  ``INTEGER(4) A``  ``INTEGER(4)``  GNU extension
  ``KIOR(A)``  ``INTEGER(8) A``  ``INTEGER(8)``  GNU extension
  ===========  ================  ==============  ====================

:samp:`{See also}:`
  IEOR, 
  IAND, 
  IBITS, 
  IBSET, 
  IBCLR, 
  NOT

