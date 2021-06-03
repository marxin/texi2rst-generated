  .. _ibset:

IBSET --- Set bit
*****************

.. index:: IBSET

.. index:: BBSET

.. index:: IIBSET

.. index:: JIBSET

.. index:: KIBSET

.. index:: bits, set

:samp:`{Description}:`
  ``IBSET`` returns the value of :samp:`{I}` with the bit at position
  :samp:`{POS}` set to one.

:samp:`{Standard}:`
  Fortran 90 and later, has overloads that are GNU extensions

:samp:`{Class}:`
  Elemental function

:samp:`{Syntax}:`
  ``RESULT = IBSET(I, POS)``

:samp:`{Arguments}:`
  =============  ==============================
  :samp:`{I}`    The type shall be ``INTEGER``.
  =============  ==============================
  :samp:`{POS}`  The type shall be ``INTEGER``.
  =============  ==============================

:samp:`{Return value}:`
  The return value is of type ``INTEGER`` and of the same kind as
  :samp:`{I}`.

:samp:`{Specific names}:`
  =============  ================  ==============  ====================
  Name           Argument          Return type     Standard
  =============  ================  ==============  ====================
  ``IBSET(A)``   ``INTEGER A``     ``INTEGER``     Fortran 90 and later
  ``BBSET(A)``   ``INTEGER(1) A``  ``INTEGER(1)``  GNU extension
  ``IIBSET(A)``  ``INTEGER(2) A``  ``INTEGER(2)``  GNU extension
  ``JIBSET(A)``  ``INTEGER(4) A``  ``INTEGER(4)``  GNU extension
  ``KIBSET(A)``  ``INTEGER(8) A``  ``INTEGER(8)``  GNU extension
  =============  ================  ==============  ====================

:samp:`{See also}:`
  IBCLR, 
  IBITS, 
  IAND, 
  IOR, 
  IEOR, 
  MVBITS

