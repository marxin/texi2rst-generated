  .. _all:

ALL --- All values in MASK along DIM are true 
**********************************************

.. index:: ALL

.. index:: array, apply condition

.. index:: array, condition testing

:samp:`{Description}:`
  ``ALL(MASK [, DIM])`` determines if all the values are true in :samp:`{MASK}`
  in the array along dimension :samp:`{DIM}`.

:samp:`{Standard}:`
  Fortran 90 and later

:samp:`{Class}:`
  Transformational function

:samp:`{Syntax}:`
  ``RESULT = ALL(MASK [, DIM])``

:samp:`{Arguments}:`
  ==============  ==================================================================
  :samp:`{MASK}`  The type of the argument shall be ``LOGICAL`` and
                  it shall not be scalar.
  ==============  ==================================================================
  :samp:`{DIM}`   (Optional) :samp:`{DIM}` shall be a scalar integer
                  with a value that lies between one and the rank of :samp:`{MASK}`.
  ==============  ==================================================================

:samp:`{Return value}:`
  ``ALL(MASK)`` returns a scalar value of type ``LOGICAL`` where
  the kind type parameter is the same as the kind type parameter of
  :samp:`{MASK}`.  If :samp:`{DIM}` is present, then ``ALL(MASK, DIM)`` returns
  an array with the rank of :samp:`{MASK}` minus 1.  The shape is determined from
  the shape of :samp:`{MASK}` where the :samp:`{DIM}` dimension is elided. 

  (A)
    ``ALL(MASK)`` is true if all elements of :samp:`{MASK}` are true.
    It also is true if :samp:`{MASK}` has zero size; otherwise, it is false.

  (B)
    If the rank of :samp:`{MASK}` is one, then ``ALL(MASK,DIM)`` is equivalent
    to ``ALL(MASK)``.  If the rank is greater than one, then ``ALL(MASK,DIM)``
    is determined by applying ``ALL`` to the array sections.

:samp:`{Example}:`

  .. code-block:: c++

    program test_all
      logical l
      l = all((/.true., .true., .true./))
      print *, l
      call section
      contains
        subroutine section
          integer a(2,3), b(2,3)
          a = 1
          b = 1
          b(2,2) = 2
          print *, all(a .eq. b, 1)
          print *, all(a .eq. b, 2)
        end subroutine section
    end program test_all

