  .. _any:

ANY --- Any value in MASK along DIM is true 
********************************************

.. index:: ANY

.. index:: array, apply condition

.. index:: array, condition testing

:samp:`{Description}:`
  ``ANY(MASK [, DIM])`` determines if any of the values in the logical array
  :samp:`{MASK}` along dimension :samp:`{DIM}` are ``.TRUE.``.

:samp:`{Standard}:`
  Fortran 90 and later

:samp:`{Class}:`
  Transformational function

:samp:`{Syntax}:`
  ``RESULT = ANY(MASK [, DIM])``

:samp:`{Arguments}:`
  ==============  ==================================================================
  :samp:`{MASK}`  The type of the argument shall be ``LOGICAL`` and
                  it shall not be scalar.
  ==============  ==================================================================
  :samp:`{DIM}`   (Optional) :samp:`{DIM}` shall be a scalar integer
                  with a value that lies between one and the rank of :samp:`{MASK}`.
  ==============  ==================================================================

:samp:`{Return value}:`
  ``ANY(MASK)`` returns a scalar value of type ``LOGICAL`` where
  the kind type parameter is the same as the kind type parameter of
  :samp:`{MASK}`.  If :samp:`{DIM}` is present, then ``ANY(MASK, DIM)`` returns
  an array with the rank of :samp:`{MASK}` minus 1.  The shape is determined from
  the shape of :samp:`{MASK}` where the :samp:`{DIM}` dimension is elided. 

  (A)
    ``ANY(MASK)`` is true if any element of :samp:`{MASK}` is true;
    otherwise, it is false.  It also is false if :samp:`{MASK}` has zero size.

  (B)
    If the rank of :samp:`{MASK}` is one, then ``ANY(MASK,DIM)`` is equivalent
    to ``ANY(MASK)``.  If the rank is greater than one, then ``ANY(MASK,DIM)``
    is determined by applying ``ANY`` to the array sections.

:samp:`{Example}:`

  .. code-block:: c++

    program test_any
      logical l
      l = any((/.true., .true., .true./))
      print *, l
      call section
      contains
        subroutine section
          integer a(2,3), b(2,3)
          a = 1
          b = 1
          b(2,2) = 2
          print *, any(a .eq. b, 1)
          print *, any(a .eq. b, 2)
        end subroutine section
    end program test_any

