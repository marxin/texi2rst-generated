  .. _co_broadcast:

CO_BROADCAST --- Copy a value to all images the current set of images
*********************************************************************

.. index:: CO_BROADCAST

.. index:: Collectives, value broadcasting

:samp:`{Description}:`
  ``CO_BROADCAST`` copies the value of argument :samp:`{A}` on the image with
  image index ``SOURCE_IMAGE`` to all images in the current team.  :samp:`{A}`
  becomes defined as if by intrinsic assignment.  If the execution was
  successful and :samp:`{STAT}` is present, it is assigned the value zero.  If the
  execution failed, :samp:`{STAT}` gets assigned a nonzero value and, if present,
  :samp:`{ERRMSG}` gets assigned a value describing the occurred error.

:samp:`{Standard}:`
  Technical Specification (TS) 18508 or later

:samp:`{Class}:`
  Collective subroutine

:samp:`{Syntax}:`
  ``CALL CO_BROADCAST(A, SOURCE_IMAGE [, STAT, ERRMSG])``

:samp:`{Arguments}:`
  ======================  =========================================================================
  :samp:`{A}`             INTENT(INOUT) argument; shall have the same
                          dynamic type and type parameters on all images of the current team. If it
                          is an array, it shall have the same shape on all images.
  ======================  =========================================================================
  :samp:`{SOURCE_IMAGE}`  a scalar integer expression.
                          It shall have the same value on all images and refer to an
                          image of the current team.
  :samp:`{STAT}`          (optional) a scalar integer variable
  :samp:`{ERRMSG}`        (optional) a scalar character variable
  ======================  =========================================================================

:samp:`{Example}:`

  .. code-block:: c++

    program test
      integer :: val(3)
      if (this_image() == 1) then
        val = [1, 5, 3]
      end if
      call co_broadcast (val, source_image=1)
      print *, this_image, ":", val
    end program test

:samp:`{See also}:`
  CO_MAX, 
  CO_MIN, 
  CO_SUM, 
  CO_REDUCE

