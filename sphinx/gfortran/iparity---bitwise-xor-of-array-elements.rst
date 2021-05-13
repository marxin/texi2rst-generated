  .. _iparity:

``IPARITY`` - Bitwise XOR of array elements
*******************************************

.. index:: IPARITY

.. index:: array, parity

.. index:: array, XOR

.. index:: bits, XOR of array elements

:samp:`{Description}:`
  Reduces with bitwise XOR (exclusive or) the elements of :samp:`{ARRAY}` along
  dimension :samp:`{DIM}` if the corresponding element in :samp:`{MASK}` is ``TRUE``.

:samp:`{Standard}:`
  Fortran 2008 and later

:samp:`{Class}:`
  Transformational function

:samp:`{Syntax}:`
  ========================================
  ``RESULT = IPARITY(ARRAY[, MASK])``
  ========================================
  ``RESULT = IPARITY(ARRAY, DIM[, MASK])``
  ========================================

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

  If :samp:`{DIM}` is absent, a scalar with the bitwise XOR of all elements in
  :samp:`{ARRAY}` is returned. Otherwise, an array of rank n-1, where n equals
  the rank of :samp:`{ARRAY}`, and a shape similar to that of :samp:`{ARRAY}` with
  dimension :samp:`{DIM}` dropped is returned.

:samp:`{Example}:`

  .. code-block:: c++

    PROGRAM test_iparity
      INTEGER(1) :: a(2)

      a(1) = int(b'00100100', 1)
      a(2) = int(b'01101010', 1)

      ! prints 01001110
      PRINT '(b8.8)', IPARITY(a)
    END PROGRAM

:samp:`{See also}:`
  IANY, 
  IALL, 
  IEOR, 
  PARITY

