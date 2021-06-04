  .. _unpack:

UNPACK --- Unpack an array of rank one into an array
****************************************************

.. index:: UNPACK

.. index:: array, unpacking

.. index:: array, increase dimension

.. index:: array, scatter elements

:samp:`{Description}:`
  Store the elements of :samp:`{VECTOR}` in an array of higher rank.

:samp:`{Standard}:`
  Fortran 90 and later

:samp:`{Class}:`
  Transformational function

:samp:`{Syntax}:`
  ``RESULT = UNPACK(VECTOR, MASK, FIELD)``

:samp:`{Arguments}:`
  ================  ===========================================================================
  :samp:`{VECTOR}`  Shall be an array of any type and rank one. It 
                    shall have at least as many elements as :samp:`{MASK}` has ``TRUE`` values.
  :samp:`{MASK}`    Shall be an array of type ``LOGICAL``.
  :samp:`{FIELD}`   Shall be of the same type as :samp:`{VECTOR}` and have
                    the same shape as :samp:`{MASK}`.
  ================  ===========================================================================

:samp:`{Return value}:`
  The resulting array corresponds to :samp:`{FIELD}` with ``TRUE`` elements
  of :samp:`{MASK}` replaced by values from :samp:`{VECTOR}` in array element order.

:samp:`{Example}:`

  .. code-block:: fortran

    PROGRAM test_unpack
      integer :: vector(2)  = (/1,1/)
      logical :: mask(4)  = (/ .TRUE., .FALSE., .FALSE., .TRUE. /)
      integer :: field(2,2) = 0, unity(2,2)

      ! result: unity matrix
      unity = unpack(vector, reshape(mask, (/2,2/)), field)
    END PROGRAM

:samp:`{See also}:`
  PACK, 
  SPREAD

