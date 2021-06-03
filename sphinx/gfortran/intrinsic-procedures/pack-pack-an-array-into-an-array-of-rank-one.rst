  .. _pack:

PACK --- Pack an array into an array of rank one
************************************************

.. index:: PACK

.. index:: array, packing

.. index:: array, reduce dimension

.. index:: array, gather elements

:samp:`{Description}:`
  Stores the elements of :samp:`{ARRAY}` in an array of rank one.

  The beginning of the resulting array is made up of elements whose :samp:`{MASK}` 
  equals ``TRUE``. Afterwards, positions are filled with elements taken from
  :samp:`{VECTOR}`.

:samp:`{Standard}:`
  Fortran 90 and later

:samp:`{Class}:`
  Transformational function

:samp:`{Syntax}:`
  ``RESULT = PACK(ARRAY, MASK[,VECTOR])``

:samp:`{Arguments}:`
  ================  ===============================================================================
  :samp:`{ARRAY}`   Shall be an array of any type.
  ================  ===============================================================================
  :samp:`{MASK}`    Shall be an array of type ``LOGICAL`` and 
                    of the same size as :samp:`{ARRAY}`. Alternatively, it may be a ``LOGICAL`` 
                    scalar.
  :samp:`{VECTOR}`  (Optional) shall be an array of the same type 
                    as :samp:`{ARRAY}` and of rank one. If present, the number of elements in 
                    :samp:`{VECTOR}` shall be equal to or greater than the number of true elements 
                    in :samp:`{MASK}`. If :samp:`{MASK}` is scalar, the number of elements in 
                    :samp:`{VECTOR}` shall be equal to or greater than the number of elements in
                    :samp:`{ARRAY}`.
  ================  ===============================================================================

:samp:`{Return value}:`
  The result is an array of rank one and the same type as that of :samp:`{ARRAY}`.
  If :samp:`{VECTOR}` is present, the result size is that of :samp:`{VECTOR}`, the
  number of ``TRUE`` values in :samp:`{MASK}` otherwise.

:samp:`{Example}:`
  Gathering nonzero elements from an array:

  .. code-block:: c++

    PROGRAM test_pack_1
      INTEGER :: m(6)
      m = (/ 1, 0, 0, 0, 5, 0 /)
      WRITE(*, FMT="(6(I0, ' '))") pack(m, m /= 0)  ! "1 5"
    END PROGRAM

  Gathering nonzero elements from an array and appending elements from :samp:`{VECTOR}` :

  .. code-block:: c++

    PROGRAM test_pack_2
      INTEGER :: m(4)
      m = (/ 1, 0, 0, 2 /)
      ! The following results in "1 2 3 4"
      WRITE(*, FMT="(4(I0, ' '))") pack(m, m /= 0, (/ 0, 0, 3, 4 /))
    END PROGRAM

:samp:`{See also}:`
  UNPACK

