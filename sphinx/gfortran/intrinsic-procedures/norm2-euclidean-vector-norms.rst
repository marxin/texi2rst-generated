  .. _norm2:

NORM2 --- Euclidean vector norms
********************************

.. index:: NORM2

.. index:: Euclidean vector norm

.. index:: L2 vector norm

.. index:: norm, Euclidean

:samp:`{Description}:`
  Calculates the Euclidean vector norm (L_2 norm)
  of :samp:`{ARRAY}` along dimension :samp:`{DIM}`.

:samp:`{Standard}:`
  Fortran 2008 and later

:samp:`{Class}:`
  Transformational function

:samp:`{Syntax}:`
  ================================
  ``RESULT = NORM2(ARRAY[, DIM])``
  ================================

:samp:`{Arguments}:`
  ===============  ===========================================================
  :samp:`{ARRAY}`  Shall be an array of type ``REAL``
  :samp:`{DIM}`    (Optional) shall be a scalar of type 
                   ``INTEGER`` with a value in the range from 1 to n, where n 
                   equals the rank of :samp:`{ARRAY}`.
  ===============  ===========================================================

:samp:`{Return value}:`
  The result is of the same type as :samp:`{ARRAY}`.

  If :samp:`{DIM}` is absent, a scalar with the square root of the sum of all
  elements in :samp:`{ARRAY}` squared  is returned. Otherwise, an array of
  rank n-1, where n equals the rank of :samp:`{ARRAY}`, and a
  shape similar to that of :samp:`{ARRAY}` with dimension :samp:`{DIM}` dropped
  is returned.

:samp:`{Example}:`

  .. code-block:: fortran

    PROGRAM test_sum
      REAL :: x(5) = [ real :: 1, 2, 3, 4, 5 ]
      print *, NORM2(x)  ! = sqrt(55.) ~ 7.416
    END PROGRAM

