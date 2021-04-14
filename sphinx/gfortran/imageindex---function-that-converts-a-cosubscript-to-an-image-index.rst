  .. _image_index:

``IMAGE_INDEX`` - Function that converts a cosubscript to an image index
************************************************************************

.. index:: IMAGE_INDEX

.. index:: coarray, IMAGE_INDEX

.. index:: images, cosubscript to image index conversion

:samp:`{Description}:`
  Returns the image index belonging to a cosubscript.

:samp:`{Standard}:`
  Fortran 2008 and later

:samp:`{Class}:`
  Inquiry function.

:samp:`{Syntax}:`
  ``RESULT = IMAGE_INDEX(COARRAY, SUB)``

:samp:`{Arguments}:`
  =================  ===============================================
  :samp:`{COARRAY}`  Coarray of any type.
  =================  ===============================================
  :samp:`{SUB}`      default integer rank-1 array of a size equal to
                     the corank of :samp:`{COARRAY}`.
  =================  ===============================================

:samp:`{Return value}:`
  Scalar default integer with the value of the image index which corresponds
  to the cosubscripts. For invalid cosubscripts the result is zero.

:samp:`{Example}:`

  .. code-block:: c++

    INTEGER :: array[2,-1:4,8,*]
    ! Writes  28 (or 0 if there are fewer than 28 images)
    WRITE (*,*) IMAGE_INDEX (array, [2,0,3,1])

:samp:`{See also}:`
  THIS_IMAGE, 
  NUM_IMAGES

