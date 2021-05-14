  .. _shape:

SHAPE - Determine the shape of an array
***************************************

.. index:: SHAPE

.. index:: array, shape

:samp:`{Description}:`
  Determines the shape of an array.

:samp:`{Standard}:`
  Fortran 90 and later, with :samp:`{KIND}` argument Fortran 2003 and later

:samp:`{Class}:`
  Inquiry function

:samp:`{Syntax}:`
  ``RESULT = SHAPE(SOURCE [, KIND])``

:samp:`{Arguments}:`
  ================  =======================================================================
  :samp:`{SOURCE}`  Shall be an array or scalar of any type. 
                    If :samp:`{SOURCE}` is a pointer it must be associated and allocatable 
                    arrays must be allocated.
  ================  =======================================================================
  :samp:`{KIND}`    (Optional) An ``INTEGER`` initialization
                    expression indicating the kind parameter of the result.
  ================  =======================================================================

:samp:`{Return value}:`
  An ``INTEGER`` array of rank one with as many elements as :samp:`{SOURCE}` 
  has dimensions. The elements of the resulting array correspond to the extend
  of :samp:`{SOURCE}` along the respective dimensions. If :samp:`{SOURCE}` is a scalar,
  the result is the rank one array of size zero. If :samp:`{KIND}` is absent, the
  return value has the default integer kind otherwise the specified kind.

:samp:`{Example}:`

  .. code-block:: c++

    PROGRAM test_shape
      INTEGER, DIMENSION(-1:1, -1:2) :: A
      WRITE(*,*) SHAPE(A)             ! (/ 3, 4 /)
      WRITE(*,*) SIZE(SHAPE(42))      ! (/ /)
    END PROGRAM

:samp:`{See also}:`
  RESHAPE, 
  SIZE

