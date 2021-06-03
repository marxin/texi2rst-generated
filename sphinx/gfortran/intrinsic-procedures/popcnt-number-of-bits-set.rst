  .. _popcnt:

POPCNT --- Number of bits set
*****************************

.. index:: POPCNT

.. index:: binary representation

.. index:: bits set

:samp:`{Description}:`
  ``POPCNT(I)`` returns the number of bits set ('1' bits) in the binary
  representation of ``I``.

:samp:`{Standard}:`
  Fortran 2008 and later

:samp:`{Class}:`
  Elemental function

:samp:`{Syntax}:`
  ``RESULT = POPCNT(I)``

:samp:`{Arguments}:`
  ===========  =============================
  :samp:`{I}`  Shall be of type ``INTEGER``.
  ===========  =============================
  ===========  =============================

:samp:`{Return value}:`
  The return value is of type ``INTEGER`` and of the default integer
  kind.

:samp:`{Example}:`

  .. code-block:: c++

    program test_population
      print *, popcnt(127),       poppar(127)
      print *, popcnt(huge(0_4)), poppar(huge(0_4))
      print *, popcnt(huge(0_8)), poppar(huge(0_8))
    end program test_population

:samp:`{See also}:`
  POPPAR, 
  LEADZ, 
  TRAILZ

