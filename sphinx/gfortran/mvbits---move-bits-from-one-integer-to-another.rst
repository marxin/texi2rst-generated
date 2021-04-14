  .. _mvbits:

``MVBITS`` - Move bits from one integer to another
**************************************************

.. index:: MVBITS

.. index:: BMVBITS

.. index:: IMVBITS

.. index:: JMVBITS

.. index:: KMVBITS

.. index:: bits, move

:samp:`{Description}:`
  Moves :samp:`{LEN}` bits from positions :samp:`{FROMPOS}` through
  ``FROMPOS+LEN-1`` of :samp:`{FROM}` to positions :samp:`{TOPOS}` through
  ``TOPOS+LEN-1`` of :samp:`{TO}`. The portion of argument :samp:`{TO}` not
  affected by the movement of bits is unchanged. The values of
  ``FROMPOS+LEN-1`` and ``TOPOS+LEN-1`` must be less than
  ``BIT_SIZE(FROM)``.

:samp:`{Standard}:`
  Fortran 90 and later, has overloads that are GNU extensions

:samp:`{Class}:`
  Elemental subroutine

:samp:`{Syntax}:`
  ``CALL MVBITS(FROM, FROMPOS, LEN, TO, TOPOS)``

:samp:`{Arguments}:`
  =================  =====================================
  :samp:`{FROM}`     The type shall be ``INTEGER``.
  =================  =====================================
  :samp:`{FROMPOS}`  The type shall be ``INTEGER``.
  :samp:`{LEN}`      The type shall be ``INTEGER``.
  :samp:`{TO}`       The type shall be ``INTEGER``, of the
                     same kind as :samp:`{FROM}`.
  :samp:`{TOPOS}`    The type shall be ``INTEGER``.
  =================  =====================================

:samp:`{Specific names}:`
  ==============  ================  ==============  ====================
  Name            Argument          Return type     Standard
  ==============  ================  ==============  ====================
  ``MVBITS(A)``   ``INTEGER A``     ``INTEGER``     Fortran 90 and later
  ``BMVBITS(A)``  ``INTEGER(1) A``  ``INTEGER(1)``  GNU extension
  ``IMVBITS(A)``  ``INTEGER(2) A``  ``INTEGER(2)``  GNU extension
  ``JMVBITS(A)``  ``INTEGER(4) A``  ``INTEGER(4)``  GNU extension
  ``KMVBITS(A)``  ``INTEGER(8) A``  ``INTEGER(8)``  GNU extension
  ==============  ================  ==============  ====================

:samp:`{See also}:`
  IBCLR, 
  IBSET, 
  IBITS, 
  IAND, 
  IOR, 
  IEOR

