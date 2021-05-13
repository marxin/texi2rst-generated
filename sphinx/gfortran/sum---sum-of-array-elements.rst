  .. _sum:

``SUM`` - Sum of array elements
*******************************

.. index:: SUM

.. index:: array, sum

.. index:: array, add elements

.. index:: array, conditionally add elements

.. index:: sum array elements

:samp:`{Description}:`
  Adds the elements of :samp:`{ARRAY}` along dimension :samp:`{DIM}` if
  the corresponding element in :samp:`{MASK}` is ``TRUE``.

:samp:`{Standard}:`
  Fortran 90 and later

:samp:`{Class}:`
  Transformational function

:samp:`{Syntax}:`
  ====================================
  ``RESULT = SUM(ARRAY[, MASK])``
  ====================================
  ``RESULT = SUM(ARRAY, DIM[, MASK])``
  ====================================

:samp:`{Arguments}:`
  ===============  ========================================================================
  :samp:`{ARRAY}`  Shall be an array of type ``INTEGER``, 
                   ``REAL`` or ``COMPLEX``.
  ===============  ========================================================================
  :samp:`{DIM}`    (Optional) shall be a scalar of type 
                   ``INTEGER`` with a value in the range from 1 to n, where n 
                   equals the rank of :samp:`{ARRAY}`.
  :samp:`{MASK}`   (Optional) shall be of type ``LOGICAL`` 
                   and either be a scalar or an array of the same shape as :samp:`{ARRAY}`.
  ===============  ========================================================================

:samp:`{Return value}:`
  The result is of the same type as :samp:`{ARRAY}`.

  If :samp:`{DIM}` is absent, a scalar with the sum of all elements in :samp:`{ARRAY}`
  is returned. Otherwise, an array of rank n-1, where n equals the rank of 
  :samp:`{ARRAY}`, and a shape similar to that of :samp:`{ARRAY}` with dimension :samp:`{DIM}` 
  dropped is returned.

:samp:`{Example}:`

  .. code-block:: c++

    PROGRAM test_sum
      INTEGER :: x(5) = (/ 1, 2, 3, 4 ,5 /)
      print *, SUM(x)                        ! all elements, sum = 15
      print *, SUM(x, MASK=MOD(x, 2)==1)     ! odd elements, sum = 9
    END PROGRAM

:samp:`{See also}:`
  PRODUCT

