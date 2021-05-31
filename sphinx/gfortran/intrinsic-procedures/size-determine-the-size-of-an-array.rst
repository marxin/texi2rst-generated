  .. _size:

SIZE - Determine the size of an array
*************************************

.. index:: SIZE

.. index:: array, size

.. index:: array, number of elements

.. index:: array, count elements

:samp:`{Description}:`
  Determine the extent of :samp:`{ARRAY}` along a specified dimension :samp:`{DIM}`,
  or the total number of elements in :samp:`{ARRAY}` if :samp:`{DIM}` is absent.

:samp:`{Standard}:`
  Fortran 90 and later, with :samp:`{KIND}` argument Fortran 2003 and later

:samp:`{Class}:`
  Inquiry function

:samp:`{Syntax}:`
  ``RESULT = SIZE(ARRAY[, DIM [, KIND]])``

:samp:`{Arguments}:`
  ===============  =========================================================================
  :samp:`{ARRAY}`  Shall be an array of any type. If :samp:`{ARRAY}` is
                   a pointer it must be associated and allocatable arrays must be allocated.
  ===============  =========================================================================
  :samp:`{DIM}`    (Optional) shall be a scalar of type ``INTEGER`` 
                   and its value shall be in the range from 1 to n, where n equals the rank 
                   of :samp:`{ARRAY}`.
  :samp:`{KIND}`   (Optional) An ``INTEGER`` initialization
                   expression indicating the kind parameter of the result.
  ===============  =========================================================================

:samp:`{Return value}:`
  The return value is of type ``INTEGER`` and of kind :samp:`{KIND}`. If
  :samp:`{KIND}` is absent, the return value is of default integer kind.

:samp:`{Example}:`

  .. code-block:: c++

    PROGRAM test_size
      WRITE(*,*) SIZE((/ 1, 2 /))    ! 2
    END PROGRAM

:samp:`{See also}:`
  SHAPE, 
  RESHAPE

