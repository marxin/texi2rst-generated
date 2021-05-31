  .. _reshape:

RESHAPE - Function to reshape an array
**************************************

.. index:: RESHAPE

.. index:: array, change dimensions

.. index:: array, transmogrify

:samp:`{Description}:`
  Reshapes :samp:`{SOURCE}` to correspond to :samp:`{SHAPE}`. If necessary,
  the new array may be padded with elements from :samp:`{PAD}` or permuted
  as defined by :samp:`{ORDER}`.

:samp:`{Standard}:`
  Fortran 90 and later

:samp:`{Class}:`
  Transformational function

:samp:`{Syntax}:`
  ``RESULT = RESHAPE(SOURCE, SHAPE[, PAD, ORDER])``

:samp:`{Arguments}:`
  ================  =========================================================================
  :samp:`{SOURCE}`  Shall be an array of any type.
  ================  =========================================================================
  :samp:`{SHAPE}`   Shall be of type ``INTEGER`` and an 
                    array of rank one. Its values must be positive or zero.
  :samp:`{PAD}`     (Optional) shall be an array of the same 
                    type as :samp:`{SOURCE}`.
  :samp:`{ORDER}`   (Optional) shall be of type ``INTEGER``
                    and an array of the same shape as :samp:`{SHAPE}`. Its values shall
                    be a permutation of the numbers from 1 to n, where n is the size of 
                    :samp:`{SHAPE}`. If :samp:`{ORDER}` is absent, the natural ordering shall
                    be assumed.
  ================  =========================================================================

:samp:`{Return value}:`
  The result is an array of shape :samp:`{SHAPE}` with the same type as 
  :samp:`{SOURCE}`. 

:samp:`{Example}:`

  .. code-block:: c++

    PROGRAM test_reshape
      INTEGER, DIMENSION(4) :: x
      WRITE(*,*) SHAPE(x)                       ! prints "4"
      WRITE(*,*) SHAPE(RESHAPE(x, (/2, 2/)))    ! prints "2 2"
    END PROGRAM

:samp:`{See also}:`
  SHAPE

