  .. _trailz:

TRAILZ - Number of trailing zero bits of an integer
***************************************************

.. index:: TRAILZ

.. index:: zero bits

:samp:`{Description}:`
  ``TRAILZ`` returns the number of trailing zero bits of an integer.

:samp:`{Standard}:`
  Fortran 2008 and later

:samp:`{Class}:`
  Elemental function

:samp:`{Syntax}:`
  ``RESULT = TRAILZ(I)``

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

    PROGRAM test_trailz
      WRITE (*,*) TRAILZ(8)  ! prints 3
    END PROGRAM

:samp:`{See also}:`
  BIT_SIZE, 
  LEADZ, 
  POPPAR, 
  POPCNT

