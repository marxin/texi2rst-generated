  .. _iall:

IALL --- Bitwise AND of array elements
**************************************

.. index:: IALL

.. index:: array, AND

.. index:: bits, AND of array elements

:samp:`{Description}:`
  Reduces with bitwise AND the elements of :samp:`{ARRAY}` along dimension :samp:`{DIM}`
  if the corresponding element in :samp:`{MASK}` is ``TRUE``.

:samp:`{Standard}:`
  Fortran 2008 and later

:samp:`{Class}:`
  Transformational function

:samp:`{Syntax}:`
  =====================================
  ``RESULT = IALL(ARRAY[, MASK])``
  =====================================
  ``RESULT = IALL(ARRAY, DIM[, MASK])``
  =====================================

:samp:`{Arguments}:`
  ===============  ========================================================================
  :samp:`{ARRAY}`  Shall be an array of type ``INTEGER``
  ===============  ========================================================================
  :samp:`{DIM}`    (Optional) shall be a scalar of type 
                   ``INTEGER`` with a value in the range from 1 to n, where n 
                   equals the rank of :samp:`{ARRAY}`.
  :samp:`{MASK}`   (Optional) shall be of type ``LOGICAL`` 
                   and either be a scalar or an array of the same shape as :samp:`{ARRAY}`.
  ===============  ========================================================================

:samp:`{Return value}:`
  The result is of the same type as :samp:`{ARRAY}`.

  If :samp:`{DIM}` is absent, a scalar with the bitwise ALL of all elements in
  :samp:`{ARRAY}` is returned. Otherwise, an array of rank n-1, where n equals
  the rank of :samp:`{ARRAY}`, and a shape similar to that of :samp:`{ARRAY}` with
  dimension :samp:`{DIM}` dropped is returned.

:samp:`{Example}:`

  .. code-block:: fortran

    PROGRAM test_iall
      INTEGER(1) :: a(2)

      a(1) = b'00100100'
      a(2) = b'01101010'

      ! prints 00100000
      PRINT '(b8.8)', IALL(a)
    END PROGRAM

:samp:`{See also}:`
  IANY, 
  IPARITY, 
  IAND

