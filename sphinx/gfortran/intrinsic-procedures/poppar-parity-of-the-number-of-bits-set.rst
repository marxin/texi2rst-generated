  .. _poppar:

POPPAR --- Parity of the number of bits set
*******************************************

.. index:: POPPAR

.. index:: binary representation

.. index:: parity

:samp:`{Description}:`
  ``POPPAR(I)`` returns parity of the integer ``I``, i.e. the parity
  of the number of bits set ('1' bits) in the binary representation of
  ``I``. It is equal to 0 if ``I`` has an even number of bits set,
  and 1 for an odd number of '1' bits.

:samp:`{Standard}:`
  Fortran 2008 and later

:samp:`{Class}:`
  Elemental function

:samp:`{Syntax}:`
  ``RESULT = POPPAR(I)``

:samp:`{Arguments}:`
  ===========  =============================
  :samp:`{I}`  Shall be of type ``INTEGER``.
  ===========  =============================

:samp:`{Return value}:`
  The return value is of type ``INTEGER`` and of the default integer
  kind.

:samp:`{Example}:`

  .. code-block:: fortran

    program test_population
      print *, popcnt(127),       poppar(127)
      print *, popcnt(huge(0_4)), poppar(huge(0_4))
      print *, popcnt(huge(0_8)), poppar(huge(0_8))
    end program test_population

:samp:`{See also}:`
  POPCNT, 
  LEADZ, 
  TRAILZ

