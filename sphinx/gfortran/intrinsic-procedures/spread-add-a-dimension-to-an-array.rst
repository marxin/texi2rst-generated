  .. _spread:

SPREAD --- Add a dimension to an array
**************************************

.. index:: SPREAD

.. index:: array, increase dimension

.. index:: array, duplicate elements

.. index:: array, duplicate dimensions

:samp:`{Description}:`
  Replicates a :samp:`{SOURCE}` array :samp:`{NCOPIES}` times along a specified 
  dimension :samp:`{DIM}`.

:samp:`{Standard}:`
  Fortran 90 and later

:samp:`{Class}:`
  Transformational function

:samp:`{Syntax}:`
  ``RESULT = SPREAD(SOURCE, DIM, NCOPIES)``

:samp:`{Arguments}:`
  =================  ==============================================================================
  :samp:`{SOURCE}`   Shall be a scalar or an array of any type and 
                     a rank less than seven.
  :samp:`{DIM}`      Shall be a scalar of type ``INTEGER`` with a 
                     value in the range from 1 to n+1, where n equals the rank of :samp:`{SOURCE}`.
  :samp:`{NCOPIES}`  Shall be a scalar of type ``INTEGER``.
  =================  ==============================================================================

:samp:`{Return value}:`
  The result is an array of the same type as :samp:`{SOURCE}` and has rank n+1
  where n equals the rank of :samp:`{SOURCE}`.

:samp:`{Example}:`

  .. code-block:: fortran

    PROGRAM test_spread
      INTEGER :: a = 1, b(2) = (/ 1, 2 /)
      WRITE(*,*) SPREAD(A, 1, 2)            ! "1 1"
      WRITE(*,*) SPREAD(B, 1, 2)            ! "1 1 2 2"
    END PROGRAM

:samp:`{See also}:`
  UNPACK

