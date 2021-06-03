  .. _move_alloc:

MOVE_ALLOC --- Move allocation from one object to another
*********************************************************

.. index:: MOVE_ALLOC

.. index:: moving allocation

.. index:: allocation, moving

:samp:`{Description}:`
  ``MOVE_ALLOC(FROM, TO)`` moves the allocation from :samp:`{FROM}` to
  :samp:`{TO}`.  :samp:`{FROM}` will become deallocated in the process.

:samp:`{Standard}:`
  Fortran 2003 and later

:samp:`{Class}:`
  Pure subroutine

:samp:`{Syntax}:`
  ``CALL MOVE_ALLOC(FROM, TO)``

:samp:`{Arguments}:`
  ==============  ==================================================
  :samp:`{FROM}`  ``ALLOCATABLE``, ``INTENT(INOUT)``, may be
                  of any type and kind.
  ==============  ==================================================
  :samp:`{TO}`    ``ALLOCATABLE``, ``INTENT(OUT)``, shall be
                  of the same type, kind and rank as :samp:`{FROM}`.
  ==============  ==================================================

:samp:`{Return value}:`
  None

:samp:`{Example}:`

  .. code-block:: c++

    program test_move_alloc
        integer, allocatable :: a(:), b(:)

        allocate(a(3))
        a = [ 1, 2, 3 ]
        call move_alloc(a, b)
        print *, allocated(a), allocated(b)
        print *, b
    end program test_move_alloc

