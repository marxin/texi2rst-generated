  .. _leadz:

LEADZ --- Number of leading zero bits of an integer
***************************************************

.. index:: LEADZ

.. index:: zero bits

:samp:`{Description}:`
  ``LEADZ`` returns the number of leading zero bits of an integer.

:samp:`{Standard}:`
  Fortran 2008 and later

:samp:`{Class}:`
  Elemental function

:samp:`{Syntax}:`
  ``RESULT = LEADZ(I)``

:samp:`{Arguments}:`
  ===========  =============================
  :samp:`{I}`  Shall be of type ``INTEGER``.
  ===========  =============================
  ===========  =============================

:samp:`{Return value}:`
  The type of the return value is the default ``INTEGER``.
  If all the bits of ``I`` are zero, the result value is ``BIT_SIZE(I)``.

:samp:`{Example}:`

  .. code-block:: c++

    PROGRAM test_leadz
      WRITE (*,*) BIT_SIZE(1)  ! prints 32
      WRITE (*,*) LEADZ(1)     ! prints 31
    END PROGRAM

:samp:`{See also}:`
  BIT_SIZE, 
  TRAILZ, 
  POPCNT, 
  POPPAR

