.. _product:

PRODUCT --- Product of array elements
*************************************

.. index:: PRODUCT

.. index:: array, product

.. index:: array, multiply elements

.. index:: array, conditionally multiply elements

.. index:: multiply array elements

.. function:: PRODUCT

  Multiplies the elements of :samp:`{ARRAY}` along dimension :samp:`{DIM}` if
  the corresponding element in :samp:`{MASK}` is ``TRUE``.

  :param ARRAY:
    Shall be an array of type ``INTEGER``, 
    ``REAL`` or ``COMPLEX``.

  :param DIM:
    (Optional) shall be a scalar of type 
    ``INTEGER`` with a value in the range from 1 to n, where n 
    equals the rank of :samp:`{ARRAY}`.

  :param MASK:
    (Optional) shall be of type ``LOGICAL`` 
    and either be a scalar or an array of the same shape as :samp:`{ARRAY}`.

  :return:
    The result is of the same type as :samp:`{ARRAY}`.

  :samp:`{Standard}:`
    Fortran 90 and later

  :samp:`{Class}:`
    Transformational function

  :samp:`{Syntax}:`

  .. code-block:: fortran

    RESULT = PRODUCT(ARRAY[, MASK])
    RESULT = PRODUCT(ARRAY, DIM[, MASK])

  :samp:`{Example}:`

    .. code-block:: fortran

      PROGRAM test_product
        INTEGER :: x(5) = (/ 1, 2, 3, 4 ,5 /)
        print *, PRODUCT(x)                    ! all elements, product = 120
        print *, PRODUCT(x, MASK=MOD(x, 2)==1) ! odd elements, product = 15
      END PROGRAM

  :samp:`{See also}:`
    SUM

